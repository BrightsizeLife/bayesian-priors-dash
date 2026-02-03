# Help tab UI for prior distributions

helpTabUI <- function() {
  dist_keys <- names(DIST_SPECS)
  cards <- lapply(dist_keys, function(key) {
    spec <- DIST_SPECS[[key]]
    help <- get_dist_help(key)
    param_rows <- lapply(names(help$params), function(param_key) {
      div(
        class = "help-param",
        span(class = "help-param-name", param_key),
        span(class = "help-param-desc", help$params[[param_key]])
      )
    })

    bslib::card(
      class = "soft-card help-card",
      bslib::card_header(spec$label),
      div(class = "help-text", help$description),
      if (length(param_rows) > 0) {
        div(class = "help-params", param_rows)
      }
    )
  })

  div(
    class = "help-grid",
    cards
  )
}

