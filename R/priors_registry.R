# Prior templates by analysis type.
# NOTE: Default presets for brms/rstanarm are placeholders until verified.

PRIOR_TEMPLATES <- list(
  linear = list(
    title = "Linear regression",
    parameters = list(
      intercept = list(
        label = "Intercept",
        allowed = c("student_t", "normal", "cauchy", "flat"),
        default = list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 10))
      ),
      beta = list(
        label = "Slope(s)",
        allowed = c("normal", "student_t", "cauchy", "flat"),
        default = list(dist = "normal", params = list(mu = 0, sigma = 1))
      ),
      sigma = list(
        label = "Residual SD",
        allowed = c("exponential", "half_normal", "half_cauchy", "half_student_t"),
        default = list(dist = "exponential", params = list(rate = 1))
      )
    ),
    settings = list(n_sims = 1000, n_points = 50, x_min = -2, x_max = 2)
  ),
  logistic = list(
    title = "Logistic regression",
    parameters = list(
      intercept = list(
        label = "Intercept",
        allowed = c("student_t", "normal", "cauchy", "flat"),
        default = list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 5))
      ),
      beta = list(
        label = "Slope(s)",
        allowed = c("normal", "student_t", "cauchy", "flat"),
        default = list(dist = "normal", params = list(mu = 0, sigma = 2.5))
      )
    ),
    settings = list(n_sims = 1000, n_points = 50, x_min = -3, x_max = 3)
  ),
  poisson = list(
    title = "Poisson regression",
    parameters = list(
      intercept = list(
        label = "Intercept",
        allowed = c("student_t", "normal", "cauchy", "flat"),
        default = list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 5))
      ),
      beta = list(
        label = "Slope(s)",
        allowed = c("normal", "student_t", "cauchy", "flat"),
        default = list(dist = "normal", params = list(mu = 0, sigma = 1))
      )
    ),
    settings = list(n_sims = 1000, n_points = 50, x_min = -2, x_max = 2)
  ),
  gamma = list(
    title = "Gamma regression",
    parameters = list(
      intercept = list(
        label = "Intercept",
        allowed = c("student_t", "normal", "cauchy", "flat"),
        default = list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 5))
      ),
      beta = list(
        label = "Slope(s)",
        allowed = c("normal", "student_t", "cauchy", "flat"),
        default = list(dist = "normal", params = list(mu = 0, sigma = 1))
      ),
      shape = list(
        label = "Shape",
        allowed = c("gamma", "exponential", "lognormal"),
        default = list(dist = "gamma", params = list(shape = 2, rate = 0.5))
      )
    ),
    settings = list(n_sims = 1000, n_points = 50, x_min = -2, x_max = 2)
  ),
  negbin = list(
    title = "Negative binomial regression",
    parameters = list(
      intercept = list(
        label = "Intercept",
        allowed = c("student_t", "normal", "cauchy", "flat"),
        default = list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 5))
      ),
      beta = list(
        label = "Slope(s)",
        allowed = c("normal", "student_t", "cauchy", "flat"),
        default = list(dist = "normal", params = list(mu = 0, sigma = 1))
      ),
      shape = list(
        label = "Dispersion (shape)",
        allowed = c("gamma", "exponential", "lognormal", "inv_gamma"),
        default = list(dist = "gamma", params = list(shape = 2, rate = 0.5))
      )
    ),
    settings = list(n_sims = 1000, n_points = 50, x_min = -2, x_max = 2)
  ),
  multilevel = list(
    title = "Multilevel regression",
    parameters = list(
      intercept = list(
        label = "Intercept",
        allowed = c("student_t", "normal", "cauchy", "flat"),
        default = list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 10))
      ),
      beta = list(
        label = "Slope(s)",
        allowed = c("normal", "student_t", "cauchy", "flat"),
        default = list(dist = "normal", params = list(mu = 0, sigma = 1))
      ),
      sigma = list(
        label = "Residual SD",
        allowed = c("exponential", "half_normal", "half_cauchy", "half_student_t"),
        default = list(dist = "exponential", params = list(rate = 1))
      ),
      tau_intercept = list(
        label = "Group SD (intercept)",
        allowed = c("exponential", "half_normal", "half_cauchy", "half_student_t"),
        default = list(dist = "exponential", params = list(rate = 1))
      ),
      tau_slope = list(
        label = "Group SD (slope)",
        allowed = c("exponential", "half_normal", "half_cauchy", "half_student_t"),
        default = list(dist = "exponential", params = list(rate = 1))
      ),
      rho = list(
        label = "Group corr (intercept/slope)",
        allowed = c("uniform", "normal"),
        default = list(dist = "uniform", params = list(min = -0.5, max = 0.5))
      )
    ),
    settings = list(n_sims = 1000, n_points = 30, x_min = -2, x_max = 2, n_groups = 8)
  )
)

PRESET_NAMES <- c("custom", "brms", "rstanarm")

build_custom_defaults <- function(template) {
  lapply(template$parameters, function(param) {
    list(dist = param$default$dist, params = param$default$params)
  })
}

build_brms_defaults <- function(analysis_key, template) {
  defaults <- build_custom_defaults(template)

  if (!is.null(defaults$intercept)) {
    defaults$intercept <- list(dist = "student_t", params = list(df = 3, mu = 0, sigma = 2.5))
  }
  if (!is.null(defaults$beta)) {
    defaults$beta <- list(dist = "flat", params = list())
  }
  if (!is.null(defaults$sigma)) {
    defaults$sigma <- list(dist = "half_student_t", params = list(df = 3, sigma = 2.5))
  }
  if (!is.null(defaults$tau_intercept)) {
    defaults$tau_intercept <- list(dist = "half_student_t", params = list(df = 3, sigma = 2.5))
  }
  if (!is.null(defaults$tau_slope)) {
    defaults$tau_slope <- list(dist = "half_student_t", params = list(df = 3, sigma = 2.5))
  }
  if (!is.null(defaults$rho)) {
    defaults$rho <- list(dist = "uniform", params = list(min = -1, max = 1))
  }
  if (!is.null(defaults$shape) && analysis_key == "gamma") {
    defaults$shape <- list(dist = "gamma", params = list(shape = 0.01, rate = 0.01))
  }
  if (!is.null(defaults$shape) && analysis_key == "negbin") {
    defaults$shape <- list(dist = "inv_gamma", params = list(shape = 0.4, rate = 0.3))
  }

  defaults
}

build_rstanarm_defaults <- function(template) {
  build_custom_defaults(template)
}

PRIOR_PRESETS <- lapply(names(PRIOR_TEMPLATES), function(key) {
  template <- PRIOR_TEMPLATES[[key]]
  list(
    custom = build_custom_defaults(template),
    brms = build_brms_defaults(key, template),
    rstanarm = build_rstanarm_defaults(template)
  )
})
names(PRIOR_PRESETS) <- names(PRIOR_TEMPLATES)
