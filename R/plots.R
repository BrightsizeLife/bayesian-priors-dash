# Plot helpers

library(ggplot2)

# Shared dark theme matching the app's bslib palette.
theme_dash <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "#141824", color = NA),
      plot.background = element_rect(fill = "#141824", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#2a3140"),
      axis.text = element_text(color = "#cbd5e1"),
      axis.title = element_text(color = "#e6e8f0"),
      plot.subtitle = element_text(color = "#9aa4b5"),
      strip.background = element_rect(fill = "#1a2030", color = "#293041"),
      strip.text = element_text(color = "#e6e8f0")
    )
}

plot_implied <- function(analysis_key, data) {
  base_theme <- theme_dash()

  if (analysis_key %in% c("logistic", "poisson", "gamma", "negbin")) {
    y_label <- if (analysis_key == "logistic") "Implied probability" else "Implied mean"
    if (analysis_key %in% c("poisson", "gamma", "negbin")) {
      y_cap <- stats::quantile(data$y, probs = 0.99, na.rm = TRUE)
      data$y <- pmin(data$y, y_cap)
    }
    return(
      ggplot(data, aes(x = x, y = y, group = sim)) +
        geom_line(color = "#7dd3fc", alpha = 0.05) +
        labs(x = "x", y = y_label) +
        base_theme
    )
  }

  point_alpha <- if (analysis_key == "multilevel") 0.06 else 0.04
  ggplot(data, aes(x = x, y = y)) +
    geom_point(color = "#7dd3fc", alpha = point_alpha, size = 0.6) +
    labs(x = "x", y = "y") +
    base_theme
}

plot_prior_density <- function(draws_df) {
  ggplot(draws_df, aes(x = value)) +
    geom_density(color = "#7dd3fc", fill = "#7dd3fc", alpha = 0.2, linewidth = 0.6) +
    facet_wrap(~ parameter, scales = "free") +
    labs(x = "Value", y = "Density") +
    theme_dash()
}
