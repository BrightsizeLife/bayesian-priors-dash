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
  lkj_corr = list(
    label = "LKJ (eta)",
    params = list(eta = 1)
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
    example = "Baseline when you truly have no prior preference (use with caution).",
    sources = list(
      list(
        label = "SAS/STAT: Improper priors",
        url = "https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/statug_introbayes_sect004.htm"
      )
    ),
    params = list()
  ),
  normal = list(
    description = "Bell-shaped distribution centered at mu. Sigma controls spread.",
    example = "Measurement error and average effects.",
    sources = list(
      list(
        label = "NIST e-Handbook: Normal distribution",
        url = "https://itl.nist.gov/div898/handbook/eda/section3/eda3661.htm"
      )
    ),
    params = list(
      mu = "Center / mean of the distribution.",
      sigma = "Standard deviation (spread)."
    )
  ),
  laplace = list(
    description = "Sharper peak at zero with heavier tails than normal. Encourages shrinkage (lasso).",
    example = "Regression coefficients in Bayesian lasso / sparse models.",
    sources = list(
      list(
        label = "Laplace distribution (overview)",
        url = "https://en.wikipedia.org/wiki/Laplace_distribution"
      ),
      list(
        label = "Bayesian lasso (Laplace prior)",
        url = "https://link.springer.com/article/10.1007/s42081-023-00213-2"
      )
    ),
    params = list(
      mu = "Center of the distribution.",
      scale = "Scale (larger = wider; smaller = stronger shrinkage)."
    )
  ),
  student_t = list(
    description = "Like normal but with heavier tails for robustness.",
    example = "Robust regression where occasional outliers exist.",
    sources = list(
      list(
        label = "Student's t distribution",
        url = "https://mathworld.wolfram.com/Studentst-Distribution.html"
      )
    ),
    params = list(
      df = "Degrees of freedom (smaller = heavier tails).",
      mu = "Center / mean.",
      sigma = "Scale (similar to SD)."
    )
  ),
  cauchy = list(
    description = "Very heavy tails. Allows large effects but can be unstable if too wide.",
    example = "Weakly informative priors on coefficients or scales.",
    sources = list(
      list(
        label = "Cauchy distribution",
        url = "https://en.wikipedia.org/wiki/Cauchy_distribution"
      )
    ),
    params = list(
      loc = "Center of the distribution.",
      scale = "Scale (controls tail heaviness)."
    )
  ),
  exponential = list(
    description = "Positive-only distribution for rates or scales.",
    example = "Time between events; simple priors on scale parameters.",
    sources = list(
      list(
        label = "NIST e-Handbook: Exponential distribution",
        url = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3667.htm"
      )
    ),
    params = list(
      rate = "Rate (larger = more mass near 0)."
    )
  ),
  gamma = list(
    description = "Positive-only distribution for scales or shapes.",
    example = "Waiting times, positive rates, or dispersion parameters.",
    sources = list(
      list(
        label = "NIST e-Handbook: Gamma distribution",
        url = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda366b.htm"
      )
    ),
    params = list(
      shape = "Shape (controls skew and peak).",
      rate = "Rate (inverse scale)."
    )
  ),
  lognormal = list(
    description = "Positive-only with multiplicative variability; log of values is normal.",
    example = "Incomes, reaction times, or multiplicative growth.",
    sources = list(
      list(
        label = "NIST e-Handbook: Lognormal distribution",
        url = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm"
      )
    ),
    params = list(
      meanlog = "Mean on the log scale.",
      sdlog = "SD on the log scale."
    )
  ),
  inv_gamma = list(
    description = "Positive-only; commonly used for variance/scale parameters.",
    example = "Variance or dispersion parameters in older Bayesian models.",
    sources = list(
      list(
        label = "Inverse-gamma distribution",
        url = "https://en.wikipedia.org/wiki/Inverse-gamma_distribution"
      )
    ),
    params = list(
      shape = "Shape parameter.",
      rate = "Rate (inverse scale)."
    )
  ),
  horseshoe = list(
    description = "Strong shrinkage for most coefficients while allowing a few large effects.",
    example = "Sparse signals with a few strong predictors.",
    sources = list(
      list(
        label = "Handling Sparsity via the Horseshoe",
        url = "https://proceedings.mlr.press/v5/carvalho09a"
      )
    ),
    params = list(
      tau = "Global shrinkage (smaller = stronger overall shrinkage).",
      lambda_scale = "Scale for local shrinkage (controls how easily large effects escape)."
    )
  ),
  lkj_corr = list(
    description = "Prior for correlation. eta = 1 is uniform; larger values favor correlations near 0.",
    example = "Correlations among group-level effects in multilevel models.",
    sources = list(
      list(
        label = "Stan User's Guide: LKJ prior",
        url = "https://mc-stan.org/docs/2_23/stan-users-guide/multivariate-hierarchical-priors-section.html"
      )
    ),
    params = list(
      eta = "Shape parameter (higher = stronger pull toward zero correlation)."
    )
  ),
  half_student_t = list(
    description = "Positive-only Student t; used for scale parameters with heavy tails.",
    example = "Standard deviations with heavy tails.",
    sources = list(
      list(
        label = "Student's t distribution",
        url = "https://mathworld.wolfram.com/Studentst-Distribution.html"
      )
    ),
    params = list(
      df = "Degrees of freedom.",
      sigma = "Scale."
    )
  ),
  half_normal = list(
    description = "Positive-only normal; useful for standard deviations.",
    example = "Standard deviations and other positive scales.",
    sources = list(
      list(
        label = "Half-normal distribution",
        url = "https://en.wikipedia.org/wiki/Half-normal_distribution"
      )
    ),
    params = list(
      sigma = "Scale (SD of the underlying normal)."
    )
  ),
  half_cauchy = list(
    description = "Positive-only Cauchy; very heavy tails for scale parameters.",
    example = "Weakly informative prior for scale parameters in hierarchical models.",
    sources = list(
      list(
        label = "Stan User's Guide: half-Cauchy recommendation",
        url = "https://mc-stan.org/docs/2_23/stan-users-guide/multivariate-hierarchical-priors-section.html"
      )
    ),
    params = list(
      scale = "Scale (controls tail heaviness)."
    )
  ),
  uniform = list(
    description = "All values between min and max are equally likely.",
    example = "Bounded parameters like correlations or probabilities (with care).",
    sources = list(
      list(
        label = "Britannica: Uniform distribution",
        url = "https://www.britannica.com/topic/uniform-distribution-statistics"
      )
    ),
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
  if (dist == "lkj_corr") {
    u <- stats::rbeta(n, shape1 = params$eta, shape2 = params$eta)
    return(2 * u - 1)
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
