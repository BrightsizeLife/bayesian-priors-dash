# Help tab server logic for rendering distribution previews

library(ggplot2)

plot_dist_preview <- function(dist) {
  params <- DIST_SPECS[[dist]]$params
  n <- 4000

  if (dist == "flat") {
    x <- stats::runif(n, min = -3, max = 3)
  } else {
    x <- draw_dist(dist, params, n)
  }

  x <- x[is.finite(x)]
  if (length(x) == 0) {
    x <- stats::rnorm(n)
  }

  if (dist != "lkj_corr") {
    q <- stats::quantile(x, probs = c(0.005, 0.995), names = FALSE)
    x <- x[x >= q[1] & x <= q[2]]
  }

  dens <- stats::density(x, n = 256)
  df <- data.frame(x = dens$x, y = dens$y)

  ggplot(df, aes(x = x, y = y)) +
    geom_area(fill = "#7dd3fc", alpha = 0.25) +
    geom_line(color = "#7dd3fc", linewidth = 0.7) +
    theme_minimal(base_size = 11) +
    theme(
      panel.background = element_rect(fill = "#141824", color = NA),
      plot.background = element_rect(fill = "#141824", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#2a3140"),
      axis.text = element_text(color = "#cbd5e1", size = 8),
      axis.title = element_blank()
    )
}

helpTabServer <- function(output, session) {
  dist_keys <- names(DIST_SPECS)
  for (key in dist_keys) {
    local({
      dist <- key
      output_id <- paste0("help_plot_", dist)
      output[[output_id]] <- renderPlot({
        plot_dist_preview(dist)
      })
    })
  }
}

