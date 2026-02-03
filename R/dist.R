# Distribution specifications and sampling helpers

FLAT_SD <- 1000

DIST_SPECS <- list(
  flat = list(
    label = "Flat (improper)",
    params = list()
  ),
  normal = list(
    label = "Normal",
    params = list(mu = 0, sigma = 1)
  ),
  laplace = list(
    label = "Laplace (lasso)",
    params = list(mu = 0, scale = 1)
  ),
  student_t = list(
    label = "Student t",
    params = list(df = 3, mu = 0, sigma = 2.5)
  ),
  cauchy = list(
    label = "Cauchy",
    params = list(loc = 0, scale = 1)
  ),
  exponential = list(
    label = "Exponential",
    params = list(rate = 1)
  ),
  gamma = list(
    label = "Gamma",
    params = list(shape = 2, rate = 1)
  ),
  lognormal = list(
    label = "Log-normal",
    params = list(meanlog = 0, sdlog = 1)
  ),
  inv_gamma = list(
    label = "Inverse-gamma",
    params = list(shape = 2, rate = 1)
  ),
  horseshoe = list(
    label = "Horseshoe",
    params = list(tau = 1, lambda_scale = 1)
  ),
  half_student_t = list(
    label = "Half-Student t",
    params = list(df = 3, sigma = 2.5)
  ),
  half_normal = list(
    label = "Half-normal",
    params = list(sigma = 1)
  ),
  half_cauchy = list(
    label = "Half-Cauchy",
    params = list(scale = 1)
  ),
  uniform = list(
    label = "Uniform",
    params = list(min = -1, max = 1)
  )
)

DIST_HELP <- list(
  flat = list(
    description = "An improper flat prior (no preference across all real values). Used as a weak baseline.",
    params = list()
  ),
  normal = list(
    description = "Bell-shaped distribution centered at mu. Sigma controls spread.",
    params = list(
      mu = "Center / mean of the distribution.",
      sigma = "Standard deviation (spread)."
    )
  ),
  laplace = list(
    description = "Sharper peak at zero with heavier tails than normal. Encourages shrinkage (lasso).",
    params = list(
      mu = "Center of the distribution.",
      scale = "Scale (larger = wider; smaller = stronger shrinkage)."
    )
  ),
  student_t = list(
    description = "Like normal but with heavier tails for robustness.",
    params = list(
      df = "Degrees of freedom (smaller = heavier tails).",
      mu = "Center / mean.",
      sigma = "Scale (similar to SD)."
    )
  ),
  cauchy = list(
    description = "Very heavy tails. Allows large effects but can be unstable if too wide.",
    params = list(
      loc = "Center of the distribution.",
      scale = "Scale (controls tail heaviness)."
    )
  ),
  exponential = list(
    description = "Positive-only distribution for rates or scales.",
    params = list(
      rate = "Rate (larger = more mass near 0)."
    )
  ),
  gamma = list(
    description = "Positive-only distribution for scales or shapes.",
    params = list(
      shape = "Shape (controls skew and peak).",
      rate = "Rate (inverse scale)."
    )
  ),
  lognormal = list(
    description = "Positive-only with multiplicative variability; log of values is normal.",
    params = list(
      meanlog = "Mean on the log scale.",
      sdlog = "SD on the log scale."
    )
  ),
  inv_gamma = list(
    description = "Positive-only; commonly used for variance/scale parameters.",
    params = list(
      shape = "Shape parameter.",
      rate = "Rate (inverse scale)."
    )
  ),
  horseshoe = list(
    description = "Strong shrinkage for most coefficients while allowing a few large effects.",
    params = list(
      tau = "Global shrinkage (smaller = stronger overall shrinkage).",
      lambda_scale = "Scale for local shrinkage (controls how easily large effects escape)."
    )
  ),
  half_student_t = list(
    description = "Positive-only Student t; used for scale parameters with heavy tails.",
    params = list(
      df = "Degrees of freedom.",
      sigma = "Scale."
    )
  ),
  half_normal = list(
    description = "Positive-only normal; useful for standard deviations.",
    params = list(
      sigma = "Scale (SD of the underlying normal)."
    )
  ),
  half_cauchy = list(
    description = "Positive-only Cauchy; very heavy tails for scale parameters.",
    params = list(
      scale = "Scale (controls tail heaviness)."
    )
  ),
  uniform = list(
    description = "All values between min and max are equally likely.",
    params = list(
      min = "Lower bound.",
      max = "Upper bound."
    )
  )
)

get_dist_choices <- function(allowed) {
  labels <- vapply(allowed, function(x) DIST_SPECS[[x]]$label, character(1))
  setNames(allowed, labels)
}

get_dist_help <- function(dist) {
  help <- DIST_HELP[[dist]]
  if (is.null(help)) {
    return(list(description = "No description available.", params = list()))
  }
  help
}

get_all_dist_help <- function() {
  names(DIST_SPECS)
}

resolve_params <- function(dist, params) {
  spec <- DIST_SPECS[[dist]]
  if (is.null(spec)) {
    stop("Unknown distribution: ", dist)
  }
  defaults <- spec$params
  merged <- defaults
  for (nm in names(defaults)) {
    if (!is.null(params[[nm]])) {
      merged[[nm]] <- params[[nm]]
    }
  }
  merged
}

clip_rho <- function(rho) {
  max(min(rho, 0.99), -0.99)
}

# Draw samples from supported distributions.
draw_dist <- function(dist, params, n) {
  params <- resolve_params(dist, params)
  if (dist == "normal") {
    return(stats::rnorm(n, mean = params$mu, sd = params$sigma))
  }
  if (dist == "flat") {
    return(stats::rnorm(n, mean = 0, sd = FLAT_SD))
  }
  if (dist == "laplace") {
    u <- stats::runif(n, min = -0.5, max = 0.5)
    return(params$mu - params$scale * sign(u) * log(1 - 2 * abs(u)))
  }
  if (dist == "student_t") {
    return(stats::rt(n, df = params$df) * params$sigma + params$mu)
  }
  if (dist == "cauchy") {
    return(stats::rcauchy(n, location = params$loc, scale = params$scale))
  }
  if (dist == "exponential") {
    return(stats::rexp(n, rate = params$rate))
  }
  if (dist == "gamma") {
    return(stats::rgamma(n, shape = params$shape, rate = params$rate))
  }
  if (dist == "lognormal") {
    return(stats::rlnorm(n, meanlog = params$meanlog, sdlog = params$sdlog))
  }
  if (dist == "inv_gamma") {
    return(1 / stats::rgamma(n, shape = params$shape, rate = params$rate))
  }
  if (dist == "horseshoe") {
    lambda <- abs(stats::rcauchy(n, location = 0, scale = params$lambda_scale))
    return(stats::rnorm(n, mean = 0, sd = params$tau * lambda))
  }
  if (dist == "half_student_t") {
    return(abs(stats::rt(n, df = params$df) * params$sigma))
  }
  if (dist == "half_normal") {
    return(abs(stats::rnorm(n, mean = 0, sd = params$sigma)))
  }
  if (dist == "half_cauchy") {
    return(abs(stats::rcauchy(n, location = 0, scale = params$scale)))
  }
  if (dist == "uniform") {
    return(stats::runif(n, min = params$min, max = params$max))
  }
  stop("Unsupported distribution: ", dist)
}
