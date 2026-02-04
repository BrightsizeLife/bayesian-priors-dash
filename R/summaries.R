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
    qs <- stats::quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE)
    data.frame(
      parameter = name,
      mean = mean(x),
      sd = stats::sd(x),
      mad = stats::mad(x),
      hdi_lower = hdi[["lower"]],
      hdi_upper = hdi[["upper"]],
      p10 = qs[[1]],
      p25 = qs[[2]],
      p50 = qs[[3]],
      p75 = qs[[4]],
      p90 = qs[[5]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
