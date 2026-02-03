# Plot helpers

library(ggplot2)

plot_implied <- function(analysis_key, data) {
  base_theme <- theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#141824", color = NA),
      plot.background = element_rect(fill = "#141824", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#2a3140"),
      axis.text = element_text(color = "#cbd5e1"),
      axis.title = element_text(color = "#e6e8f0")
    )

  if (analysis_key %in% c("logistic", "poisson", "gamma", "negbin")) {
    y_label <- if (analysis_key == "logistic") "Implied probability" else "Implied mean"
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
  palette <- c(
    "#7dd3fc",
    "#fca5a5",
    "#fdba74",
    "#c4b5fd",
    "#5eead4",
    "#f0abfc"
  )
  params <- unique(draws_df$parameter)
  pal <- setNames(rep(palette, length.out = length(params)), params)

  ggplot(draws_df, aes(x = value, color = parameter, fill = parameter)) +
    geom_density(alpha = 0.18, linewidth = 0.6) +
    labs(x = "Value", y = "Density") +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#141824", color = NA),
      plot.background = element_rect(fill = "#141824", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#2a3140"),
      legend.position = "bottom",
      axis.text = element_text(color = "#cbd5e1"),
      axis.title = element_text(color = "#e6e8f0"),
      legend.text = element_text(color = "#cbd5e1"),
      legend.title = element_blank()
    )
}
