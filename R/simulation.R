# Simulation helpers for implied data under prior settings

source("R/dist.R")

ETA_LIMIT <- 6

cap_eta <- function(eta, limit = ETA_LIMIT) {
  pmax(pmin(eta, limit), -limit)
}

sample_prior_draws <- function(prior_state, n) {
  draws <- list()
  for (name in names(prior_state)) {
    spec <- prior_state[[name]]
    draws[[name]] <- draw_dist(spec$dist, spec$params, n)
  }
  draws
}

simulate_linear <- function(prior_state, settings) {
  n_sims <- settings$n_sims
  n_points <- settings$n_points
  x <- seq(settings$x_min, settings$x_max, length.out = n_points)

  draws <- sample_prior_draws(prior_state, n_sims)
  intercept <- draws$intercept
  beta <- draws$beta
  sigma <- draws$sigma

  x_rep <- rep(x, times = n_sims)
  intercept_rep <- rep(intercept, each = n_points)
  beta_rep <- rep(beta, each = n_points)
  sigma_rep <- rep(sigma, each = n_points)
  mu <- intercept_rep + beta_rep * x_rep
  y <- mu + stats::rnorm(length(mu), mean = 0, sd = sigma_rep)

  data.frame(
    sim = rep(seq_len(n_sims), each = n_points),
    x = x_rep,
    y = y
  )
}

simulate_logistic <- function(prior_state, settings) {
  n_sims <- settings$n_sims
  n_points <- settings$n_points
  x <- seq(settings$x_min, settings$x_max, length.out = n_points)

  draws <- sample_prior_draws(prior_state, n_sims)
  intercept <- draws$intercept
  beta <- draws$beta

  x_rep <- rep(x, times = n_sims)
  intercept_rep <- rep(intercept, each = n_points)
  beta_rep <- rep(beta, each = n_points)
  eta <- cap_eta(intercept_rep + beta_rep * x_rep)
  p <- stats::plogis(eta)

  data.frame(
    sim = rep(seq_len(n_sims), each = n_points),
    x = x_rep,
    y = p
  )
}

simulate_poisson <- function(prior_state, settings) {
  n_sims <- settings$n_sims
  n_points <- settings$n_points
  x <- seq(settings$x_min, settings$x_max, length.out = n_points)

  draws <- sample_prior_draws(prior_state, n_sims)
  intercept <- draws$intercept
  beta <- draws$beta

  x_rep <- rep(x, times = n_sims)
  intercept_rep <- rep(intercept, each = n_points)
  beta_rep <- rep(beta, each = n_points)
  eta <- cap_eta(intercept_rep + beta_rep * x_rep)
  mu <- exp(eta)

  data.frame(
    sim = rep(seq_len(n_sims), each = n_points),
    x = x_rep,
    y = mu
  )
}

simulate_gamma <- function(prior_state, settings) {
  n_sims <- settings$n_sims
  n_points <- settings$n_points
  x <- seq(settings$x_min, settings$x_max, length.out = n_points)

  draws <- sample_prior_draws(prior_state, n_sims)
  intercept <- draws$intercept
  beta <- draws$beta

  x_rep <- rep(x, times = n_sims)
  intercept_rep <- rep(intercept, each = n_points)
  beta_rep <- rep(beta, each = n_points)
  eta <- cap_eta(intercept_rep + beta_rep * x_rep)
  mu <- exp(eta)

  data.frame(
    sim = rep(seq_len(n_sims), each = n_points),
    x = x_rep,
    y = mu
  )
}

simulate_negbin <- function(prior_state, settings) {
  n_sims <- settings$n_sims
  n_points <- settings$n_points
  x <- seq(settings$x_min, settings$x_max, length.out = n_points)

  draws <- sample_prior_draws(prior_state, n_sims)
  intercept <- draws$intercept
  beta <- draws$beta

  x_rep <- rep(x, times = n_sims)
  intercept_rep <- rep(intercept, each = n_points)
  beta_rep <- rep(beta, each = n_points)
  eta <- cap_eta(intercept_rep + beta_rep * x_rep)
  mu <- exp(eta)

  data.frame(
    sim = rep(seq_len(n_sims), each = n_points),
    x = x_rep,
    y = mu
  )
}

simulate_multilevel <- function(prior_state, settings) {
  n_sims <- settings$n_sims
  n_points <- settings$n_points
  n_groups <- settings$n_groups
  x <- seq(settings$x_min, settings$x_max, length.out = n_points)

  draws <- sample_prior_draws(prior_state, n_sims)
  intercept <- draws$intercept
  beta <- draws$beta
  sigma <- draws$sigma
  tau_intercept <- draws$tau_intercept
  tau_slope <- draws$tau_slope
  rho <- draws$rho

  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    r <- clip_rho(rho[[i]])
    Sigma <- matrix(
      c(tau_intercept[[i]]^2, r * tau_intercept[[i]] * tau_slope[[i]],
        r * tau_intercept[[i]] * tau_slope[[i]], tau_slope[[i]]^2),
      nrow = 2, byrow = TRUE
    )
    L <- chol(Sigma)
    z <- matrix(stats::rnorm(n_groups * 2), n_groups, 2)
    b <- z %*% L

    group_ids <- rep(seq_len(n_groups), each = n_points)
    x_rep <- rep(x, times = n_groups)
    b0 <- rep(b[, 1], each = n_points)
    b1 <- rep(b[, 2], each = n_points)

    mu <- (intercept[[i]] + b0) + (beta[[i]] + b1) * x_rep
    y <- mu + stats::rnorm(length(mu), mean = 0, sd = sigma[[i]])

    sims[[i]] <- data.frame(
      sim = i,
      group = group_ids,
      x = x_rep,
      y = y
    )
  }

  do.call(rbind, sims)
}

simulate_analysis <- function(analysis_key, prior_state, settings) {
  if (analysis_key == "linear") {
    return(simulate_linear(prior_state, settings))
  }
  if (analysis_key == "logistic") {
    return(simulate_logistic(prior_state, settings))
  }
  if (analysis_key == "poisson") {
    return(simulate_poisson(prior_state, settings))
  }
  if (analysis_key == "gamma") {
    return(simulate_gamma(prior_state, settings))
  }
  if (analysis_key == "negbin") {
    return(simulate_negbin(prior_state, settings))
  }
  if (analysis_key == "multilevel") {
    return(simulate_multilevel(prior_state, settings))
  }
  stop("Unknown analysis type: ", analysis_key)
}
