# Summary helpers for prior draws

hdi_interval <- function(x, mass = 0.9) {
  x <- sort(x)
  n <- length(x)
  if (n == 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  m <- floor(mass * n)
  if (m < 1) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  widths <- x[(m + 1):n] - x[1:(n - m)]
  idx <- which.min(widths)
  c(lower = x[idx], upper = x[idx + m])
}

summarize_draws <- function(draws, hdi_mass = 0.9) {
  rows <- lapply(names(draws), function(name) {
    x <- draws[[name]]
    hdi <- hdi_interval(x, hdi_mass)
    qs <- stats::quantile(x, probs = c(0.5, 0.8, 0.85, 0.9), names = FALSE)
    data.frame(
      parameter = name,
      mean = mean(x),
      sd = stats::sd(x),
      mad = stats::mad(x),
      hdi_lower = hdi[["lower"]],
      hdi_upper = hdi[["upper"]],
      q50 = qs[[1]],
      q80 = qs[[2]],
      q85 = qs[[3]],
      q90 = qs[[4]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

