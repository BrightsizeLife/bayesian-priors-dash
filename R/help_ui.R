# Help tab UI for prior distributions

helpTabUI <- function() {
  dist_keys <- names(DIST_SPECS)
  cards <- lapply(dist_keys, function(key) {
    spec <- DIST_SPECS[[key]]
    help <- get_dist_help(key)
    plot_id <- paste0("help_plot_", key)
    popover_params <- lapply(names(help$params), function(param_key) {
      tags$li(
        span(class = "help-param-name", param_key),
        span(class = "help-param-desc", help$params[[param_key]])
      )
    })

    details_popover <- NULL
    if (length(popover_params) > 0) {
      details_popover <- bslib::popover(
        span(class = "help-details-button", "Details"),
        title = paste0(spec$label, " parameters"),
        content = tags$ul(class = "help-popover-list", popover_params),
        placement = "right",
        trigger = "click"
      )
    }

    bslib::card(
      class = "soft-card help-card",
      bslib::card_header(spec$label),
      plotOutput(plot_id, height = 140),
      if (!is.null(details_popover)) {
        div(class = "help-details-row", details_popover)
      },
      div(class = "help-text", help$description),
      if (!is.null(help$example) && nzchar(help$example)) {
        div(
          class = "help-example",
          span(class = "help-example-label", "Example:"),
          span(class = "help-example-text", help$example)
        )
      }
    )
  })

  div(
    class = "help-grid",
    cards
  )
}
