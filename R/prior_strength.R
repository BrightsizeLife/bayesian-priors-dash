# Experimental helpers for prior strength calculations.

PRIOR_STRENGTH_SPECS <- list(
  normal_mean = list(
    label = "Normal mean (known sigma)",
    prior_params = c("mu0", "sigma0"),
    likelihood_params = c("sigma"),
    positive_params = c("sigma0", "sigma"),
    notes = "Posterior mean uses precision weights; assumes known observation SD.",
    weight_fn = function(prior, likelihood, n) {
      list(
        prior = 1 / (prior$sigma0^2),
        likelihood = n / (likelihood$sigma^2)
      )
    },
    n_equal_fn = function(prior, likelihood) {
      (likelihood$sigma^2) / (prior$sigma0^2)
    }
  ),
  bernoulli = list(
    label = "Bernoulli/Binomial (Beta prior)",
    prior_params = c("alpha", "beta"),
    likelihood_params = character(0),
    positive_params = c("alpha", "beta"),
    notes = "Posterior mean is weighted by prior pseudo-counts (alpha+beta) vs. n.",
    weight_fn = function(prior, likelihood, n) {
      list(
        prior = prior$alpha + prior$beta,
        likelihood = n
      )
    },
    n_equal_fn = function(prior, likelihood) {
      prior$alpha + prior$beta
    }
  ),
  poisson = list(
    label = "Poisson (Gamma prior on rate)",
    prior_params = c("alpha", "beta"),
    likelihood_params = character(0),
    positive_params = c("alpha", "beta"),
    notes = "Posterior mean uses weights beta (prior) vs. n (likelihood).",
    weight_fn = function(prior, likelihood, n) {
      list(
        prior = prior$beta,
        likelihood = n
      )
    },
    n_equal_fn = function(prior, likelihood) {
      prior$beta
    }
  )
)

prior_strength_choices <- function() {
  labels <- vapply(names(PRIOR_STRENGTH_SPECS), function(x) PRIOR_STRENGTH_SPECS[[x]]$label, character(1))
  setNames(names(PRIOR_STRENGTH_SPECS), labels)
}

prior_strength_spec <- function(likelihood) {
  spec <- PRIOR_STRENGTH_SPECS[[likelihood]]
  if (is.null(spec)) {
    stop("Unknown likelihood: ", likelihood)
  }
  spec
}

ensure_named_params <- function(params, required, label) {
  if (is.null(params)) {
    params <- list()
  }
  if (!is.list(params)) {
    stop(label, " parameters must be a list.")
  }
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop(label, " parameters missing: ", paste(missing, collapse = ", "))
  }
  params
}

ensure_positive_params <- function(params, names, label) {
  for (nm in names) {
    val <- params[[nm]]
    if (!is.numeric(val) || length(val) != 1 || is.na(val)) {
      stop(label, " parameter '", nm, "' must be a single numeric value.")
    }
    if (val <= 0) {
      stop(label, " parameter '", nm, "' must be > 0.")
    }
  }
}

ensure_positive_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
    stop("n must be a single numeric value.")
  }
  if (n <= 0) {
    stop("n must be > 0.")
  }
}

prior_strength_weights <- function(likelihood, prior_params, likelihood_params, n) {
  spec <- prior_strength_spec(likelihood)
  prior_params <- ensure_named_params(prior_params, spec$prior_params, "Prior")
  likelihood_params <- ensure_named_params(likelihood_params, spec$likelihood_params, "Likelihood")
  ensure_positive_params(prior_params, spec$positive_params, "Prior")
  ensure_positive_params(likelihood_params, spec$positive_params, "Likelihood")
  ensure_positive_n(n)

  weights <- spec$weight_fn(prior_params, likelihood_params, n)
  total <- weights$prior + weights$likelihood
  list(
    prior_weight = weights$prior,
    likelihood_weight = weights$likelihood,
    prior_share = weights$prior / total,
    likelihood_share = weights$likelihood / total
  )
}

prior_strength_thresholds <- function(likelihood, prior_params, likelihood_params, n_is_integer = TRUE) {
  spec <- prior_strength_spec(likelihood)
  prior_params <- ensure_named_params(prior_params, spec$prior_params, "Prior")
  likelihood_params <- ensure_named_params(likelihood_params, spec$likelihood_params, "Likelihood")
  ensure_positive_params(prior_params, spec$positive_params, "Prior")
  ensure_positive_params(likelihood_params, spec$positive_params, "Likelihood")

  n_equal <- spec$n_equal_fn(prior_params, likelihood_params)
  if (n_is_integer) {
    list(
      n_equal = n_equal,
      n_min_likelihood_dominates = floor(n_equal) + 1,
      n_max_prior_dominates = ceiling(n_equal) - 1
    )
  } else {
    list(
      n_equal = n_equal,
      n_min_likelihood_dominates = n_equal,
      n_max_prior_dominates = n_equal
    )
  }
}

prior_strength_summary <- function(likelihood, prior_params, likelihood_params, n, n_is_integer = TRUE) {
  spec <- prior_strength_spec(likelihood)
  weights <- prior_strength_weights(likelihood, prior_params, likelihood_params, n)
  thresholds <- prior_strength_thresholds(likelihood, prior_params, likelihood_params, n_is_integer = n_is_integer)

  dominance <- if (weights$prior_weight > weights$likelihood_weight) {
    "prior"
  } else if (weights$prior_weight < weights$likelihood_weight) {
    "likelihood"
  } else {
    "tie"
  }

  data.frame(
    likelihood = likelihood,
    label = spec$label,
    n = n,
    prior_weight = weights$prior_weight,
    likelihood_weight = weights$likelihood_weight,
    prior_share = weights$prior_share,
    likelihood_share = weights$likelihood_share,
    dominance = dominance,
    n_equal = thresholds$n_equal,
    n_min_likelihood_dominates = thresholds$n_min_likelihood_dominates,
    n_max_prior_dominates = thresholds$n_max_prior_dominates,
    notes = spec$notes,
    stringsAsFactors = FALSE
  )
}
