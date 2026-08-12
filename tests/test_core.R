# Tests for the non-Shiny core: distributions, summaries, simulation,
# and prior-strength math. Run from the repo root with:
#   Rscript tests/test_core.R
# Requires only base R (no shiny/bslib/ggplot2).

source("R/dist.R")
source("R/priors_registry.R")
source("R/summaries.R")
source("R/simulation.R")
source("R/prior_strength.R")

failures <- 0
check <- function(desc, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    message("  error: ", conditionMessage(e))
    FALSE
  })
  status <- if (ok) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", status, desc))
  if (!ok) {
    failures <<- failures + 1
  }
  invisible(ok)
}

expect_error <- function(expr) {
  inherits(tryCatch(expr, error = function(e) e), "error")
}

set.seed(42)

# --- dist.R -----------------------------------------------------------------

positive_only <- c(
  "exponential", "gamma", "lognormal", "inv_gamma",
  "half_student_t", "half_normal", "half_cauchy"
)

for (dist in names(DIST_SPECS)) {
  x <- draw_dist(dist, DIST_SPECS[[dist]]$params, 500)
  check(paste0("draw_dist('", dist, "') returns 500 numeric draws"),
        is.numeric(x) && length(x) == 500 && !anyNA(x))
  if (dist %in% positive_only) {
    check(paste0("draw_dist('", dist, "') draws are positive"), all(x >= 0))
  }
}

check("uniform draws respect bounds", {
  x <- draw_dist("uniform", list(min = -1, max = 1), 1000)
  all(x >= -1 & x <= 1)
})

check("lkj_corr draws stay in [-1, 1]", {
  x <- draw_dist("lkj_corr", list(eta = 1), 1000)
  all(x >= -1 & x <= 1)
})

check("draw_dist errors on unknown distribution",
      expect_error(draw_dist("nope", list(), 10)))

check("resolve_params merges overrides onto defaults", {
  p <- resolve_params("normal", list(sigma = 5))
  p$mu == 0 && p$sigma == 5
})

check("resolve_params errors on unknown distribution",
      expect_error(resolve_params("nope", list())))

check("clip_rho clips to [-0.99, 0.99]",
      clip_rho(1) == 0.99 && clip_rho(-1) == -0.99 && clip_rho(0.5) == 0.5)

# --- summaries.R ------------------------------------------------------------

check("hdi_interval covers requested mass on uniform draws", {
  x <- stats::runif(10000)
  hdi <- hdi_interval(x, mass = 0.9)
  width <- hdi[["upper"]] - hdi[["lower"]]
  hdi[["lower"]] < hdi[["upper"]] && abs(width - 0.9) < 0.05
})

check("hdi_interval finds the dense region of skewed draws", {
  x <- stats::rexp(10000)
  hdi <- hdi_interval(x, mass = 0.5)
  hdi[["lower"]] < 0.2
})

check("hdi_interval on empty input returns NAs", {
  hdi <- hdi_interval(numeric(0))
  is.na(hdi[["lower"]]) && is.na(hdi[["upper"]])
})

check("summarize_draws returns one labelled row per parameter", {
  draws <- list(a = stats::rnorm(1000), b = stats::rexp(1000))
  s <- summarize_draws(draws, hdi_mass = 0.9)
  nrow(s) == 2 && identical(s$parameter, c("a", "b")) &&
    all(c("mean", "sd", "mad", "hdi_lower", "hdi_upper",
          "p10", "p25", "p50", "p75", "p90") %in% names(s))
})

check("summarize_draws quantiles are ordered", {
  s <- summarize_draws(list(a = stats::rnorm(1000)))
  s$p10 <= s$p25 && s$p25 <= s$p50 && s$p50 <= s$p75 && s$p75 <= s$p90 &&
    s$hdi_lower <= s$hdi_upper
})

# --- simulation.R -----------------------------------------------------------

base_priors <- list(
  intercept = list(dist = "normal", params = list(mu = 0, sigma = 1)),
  beta = list(dist = "normal", params = list(mu = 0, sigma = 1)),
  sigma = list(dist = "exponential", params = list(rate = 1))
)
settings <- list(n_sims = 20, n_points = 10, x_min = -2, x_max = 2, n_groups = 4)

check("cap_eta bounds the linear predictor",
      all(abs(cap_eta(c(-100, 0, 100))) <= ETA_LIMIT))

for (key in c("linear", "logistic", "poisson", "gamma", "negbin")) {
  ps <- if (key == "linear") base_priors else base_priors[c("intercept", "beta")]
  d <- simulate_analysis(key, ps, settings)
  check(paste0("simulate_analysis('", key, "') returns n_sims * n_points rows"),
        is.data.frame(d) && nrow(d) == 20 * 10 && all(is.finite(d$y)))
}

check("simulate_analysis('logistic') yields probabilities in [0, 1]", {
  d <- simulate_analysis("logistic", base_priors[c("intercept", "beta")], settings)
  all(d$y >= 0 & d$y <= 1)
})

check("simulate_analysis('multilevel') covers all groups", {
  ps <- c(base_priors, list(
    tau_intercept = list(dist = "exponential", params = list(rate = 1)),
    tau_slope = list(dist = "exponential", params = list(rate = 1)),
    rho = list(dist = "uniform", params = list(min = -0.5, max = 0.5))
  ))
  d <- simulate_analysis("multilevel", ps, settings)
  nrow(d) == 20 * 10 * 4 && length(unique(d$group)) == 4
})

check("simulate_analysis errors on unknown analysis",
      expect_error(simulate_analysis("nope", base_priors, settings)))

# --- priors_registry.R -------------------------------------------------------

check("every template default distribution is allowed and specified", {
  ok <- TRUE
  for (template in PRIOR_TEMPLATES) {
    for (param in template$parameters) {
      ok <- ok && param$default$dist %in% param$allowed &&
        param$default$dist %in% names(DIST_SPECS)
    }
  }
  ok
})

check("every preset references known distributions", {
  ok <- TRUE
  for (analysis in PRIOR_PRESETS) {
    for (preset in analysis) {
      for (spec in preset) {
        ok <- ok && spec$dist %in% names(DIST_SPECS)
      }
    }
  }
  ok
})

# --- prior_strength.R --------------------------------------------------------
# Regression tests: these calls used to error because positive-parameter
# validation checked each parameter set against the other set's names.

check("normal_mean weights match precision arithmetic", {
  w <- prior_strength_weights("normal_mean",
                              list(mu0 = 0, sigma0 = 1),
                              list(sigma = 2), n = 30)
  isTRUE(all.equal(w$prior_weight, 1)) &&
    isTRUE(all.equal(w$likelihood_weight, 7.5)) &&
    isTRUE(all.equal(w$prior_share, 1 / 8.5)) &&
    isTRUE(all.equal(w$prior_share + w$likelihood_share, 1))
})

check("normal_mean thresholds match sigma^2 / sigma0^2", {
  t <- prior_strength_thresholds("normal_mean",
                                 list(mu0 = 0, sigma0 = 1),
                                 list(sigma = 2))
  isTRUE(all.equal(t$n_equal, 4)) &&
    t$n_min_likelihood_dominates == 5 &&
    t$n_max_prior_dominates == 3
})

check("bernoulli prior weight equals pseudo-count alpha + beta", {
  w <- prior_strength_weights("bernoulli", list(alpha = 2, beta = 2), list(), n = 5)
  isTRUE(all.equal(w$prior_weight, 4)) && isTRUE(all.equal(w$prior_share, 4 / 9))
})

check("poisson prior weight equals beta", {
  w <- prior_strength_weights("poisson", list(alpha = 2, beta = 1), list(), n = 5)
  isTRUE(all.equal(w$prior_weight, 1)) && isTRUE(all.equal(w$prior_share, 1 / 6))
})

check("prior_strength_summary reports dominance", {
  s <- prior_strength_summary("normal_mean",
                              list(mu0 = 0, sigma0 = 1),
                              list(sigma = 2), n = 30)
  s$dominance == "likelihood" && s$n == 30
})

check("validation rejects non-positive prior scale",
      expect_error(prior_strength_weights("normal_mean",
                                          list(mu0 = 0, sigma0 = -1),
                                          list(sigma = 2), n = 30)))

check("validation rejects non-positive likelihood sigma",
      expect_error(prior_strength_weights("normal_mean",
                                          list(mu0 = 0, sigma0 = 1),
                                          list(sigma = 0), n = 30)))

check("validation rejects non-positive n",
      expect_error(prior_strength_weights("normal_mean",
                                          list(mu0 = 0, sigma0 = 1),
                                          list(sigma = 2), n = 0)))

check("validation rejects missing prior parameters",
      expect_error(prior_strength_weights("bernoulli", list(alpha = 2), list(), n = 5)))

# --- syntax check on every R file (incl. Shiny files not sourced above) ------

for (f in c("app.R", list.files("R", full.names = TRUE))) {
  check(paste0("parse(", f, ") succeeds"),
        is.expression(parse(f)))
}

# ------------------------------------------------------------------------------

cat(sprintf("\n%s\n", if (failures == 0) "All tests passed." else
  sprintf("%d test(s) FAILED.", failures)))
if (failures > 0) {
  quit(status = 1)
}
