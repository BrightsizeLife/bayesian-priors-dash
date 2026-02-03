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

get_dist_choices <- function(allowed) {
  labels <- vapply(allowed, function(x) DIST_SPECS[[x]]$label, character(1))
  setNames(allowed, labels)
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
