# Shiny modules for each analysis tab

analysisModuleUI <- function(id, analysis_key) {
  template <- PRIOR_TEMPLATES[[analysis_key]]
  ns <- NS(id)
  is_glm <- analysis_key %in% c("logistic", "poisson", "gamma", "negbin")

  param_cards <- lapply(names(template$parameters), function(param_name) {
    param <- template$parameters[[param_name]]
    dist_id <- paste0(param_name, "_dist")
    params_ui_id <- paste0(param_name, "_params")

    bslib::card(
      class = "soft-card",
      bslib::card_header(param$label),
      selectInput(
        ns(dist_id),
        "Distribution",
        choices = get_dist_choices(param$allowed),
        selected = param$default$dist
      ),
      uiOutput(ns(params_ui_id))
    )
  })

  settings <- template$settings

  sidebar_controls <- tagList(
    div(class = "sidebar-title", template$title),
    selectInput(
      ns("preset"),
      "Preset",
      choices = c(
        "Custom" = "custom",
        "brms 2.22.0 defaults" = "brms",
        "rstanarm (placeholder)" = "rstanarm"
      ),
      selected = "custom"
    ),
    uiOutput(ns("preset_note")),
    uiOutput(ns("flat_warning")),
    param_cards,
    bslib::card(
      class = "soft-card",
      bslib::card_header("Run"),
      actionButton(ns("run_sim"), "Run simulations", class = "run-button"),
      div(class = "muted-note", "Updates only when you press run.")
    ),
    if (is_glm) {
      bslib::card(
        class = "soft-card",
        bslib::card_header("GLM scale"),
        selectInput(
          ns("glm_scale"),
          "Scale for priors & summaries",
          choices = c(
            "Log scale (linear predictor)" = "linear",
            "Exponentiated (odds/mean ratio)" = "exp"
          ),
          selected = "linear"
        ),
        uiOutput(ns("glm_note"))
      )
    },
    bslib::card(
      class = "soft-card",
      bslib::card_header("Simulation"),
      numericInput(ns("n_sims"), "Simulations", settings$n_sims, min = 100, max = 5000, step = 50),
      numericInput(ns("n_points"), "Points per sim", settings$n_points, min = 20, max = 200, step = 5),
      numericInput(ns("x_min"), "x min", settings$x_min, step = 0.5),
      numericInput(ns("x_max"), "x max", settings$x_max, step = 0.5),
      numericInput(ns("seed"), "Seed", 123, min = 1, step = 1),
      if (analysis_key == "multilevel") {
        numericInput(ns("n_groups"), "Groups", settings$n_groups, min = 2, max = 50, step = 1)
      }
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Summaries"),
      numericInput(ns("n_draws"), "Draws", 5000, min = 500, max = 50000, step = 500),
      numericInput(ns("hdi_mass"), "HDI mass", 0.9, min = 0.5, max = 0.99, step = 0.01)
    )
  )

  fluidRow(
    column(
      width = 3,
      div(class = "sidebar-panel", sidebar_controls)
    ),
    column(
      width = 9,
      div(
        class = "main-panel",
        fluidRow(
          column(
            width = 6,
            bslib::card(
              class = "soft-card",
              bslib::card_header("Implied data"),
              plotOutput(ns("implied_plot"), height = 360)
            )
          ),
          column(
            width = 6,
            bslib::card(
              class = "soft-card",
              bslib::card_header("Parameter priors"),
              plotOutput(ns("prior_plot"), height = 360)
            )
          )
        ),
        bslib::card(
          class = "soft-card",
          bslib::card_header("Prior summaries"),
          tableOutput(ns("summary_table"))
        )
      )
    )
  )
}

analysisModuleServer <- function(id, analysis_key) {
  moduleServer(id, function(input, output, session) {
    template <- PRIOR_TEMPLATES[[analysis_key]]
    presets <- PRIOR_PRESETS[[analysis_key]]
    is_glm <- analysis_key %in% c("logistic", "poisson", "gamma", "negbin")
    ns <- session$ns

    safe_value <- function(value, default) {
      if (is.null(value) || is.na(value)) {
        return(default)
      }
      value
    }

    # Render parameter-specific numeric inputs based on selected distribution.
    for (param_name in names(template$parameters)) {
      local({
        param <- param_name
        dist_id <- paste0(param, "_dist")
        params_ui_id <- paste0(param, "_params")

        output[[params_ui_id]] <- renderUI({
          dist <- safe_value(input[[dist_id]], template$parameters[[param]]$default$dist)
          spec <- DIST_SPECS[[dist]]
          defaults <- resolve_params(dist, template$parameters[[param]]$default$params)
          help <- get_dist_help(dist)

          inputs <- lapply(names(spec$params), function(param_key) {
            input_id <- paste0(param, "_", param_key)
            numericInput(
              inputId = ns(input_id),
              label = param_key,
              value = safe_value(input[[input_id]], defaults[[param_key]]),
              step = 0.1
            )
          })

          help_params <- lapply(names(help$params), function(param_key) {
            div(
              class = "help-param",
              span(class = "help-param-name", param_key),
              span(class = "help-param-desc", help$params[[param_key]])
            )
          })

          tagList(
            inputs,
            if (!is.null(help$description) && nzchar(help$description)) {
              div(class = "help-text", help$description)
            },
            if (length(help_params) > 0) {
              div(class = "help-params", help_params)
            }
          )
        })
      })
    }

    # Apply preset defaults when the preset changes.
    observeEvent(input$preset, {
      preset <- input$preset
      if (is.null(preset) || is.null(presets[[preset]])) {
        return()
      }

      for (param_name in names(template$parameters)) {
        preset_spec <- presets[[preset]][[param_name]]
        dist_id <- paste0(param_name, "_dist")
        updateSelectInput(session, dist_id, selected = preset_spec$dist)

        spec <- DIST_SPECS[[preset_spec$dist]]
        defaults <- resolve_params(preset_spec$dist, preset_spec$params)
        for (param_key in names(spec$params)) {
          input_id <- paste0(param_name, "_", param_key)
          updateNumericInput(session, input_id, value = defaults[[param_key]])
        }
      }
    }, ignoreInit = TRUE)

    prior_state_live <- reactive({
      params <- list()
      for (param_name in names(template$parameters)) {
        dist_id <- paste0(param_name, "_dist")
        dist <- safe_value(input[[dist_id]], template$parameters[[param_name]]$default$dist)
        param_spec <- DIST_SPECS[[dist]]$params

        param_values <- list()
        for (param_key in names(param_spec)) {
          input_id <- paste0(param_name, "_", param_key)
          default_val <- template$parameters[[param_name]]$default$params[[param_key]]
          param_values[[param_key]] <- safe_value(input[[input_id]], default_val)
        }

        params[[param_name]] <- list(dist = dist, params = param_values)
      }
      params
    })

    output$flat_warning <- renderUI({
      has_flat <- any(vapply(prior_state_live(), function(spec) spec$dist == "flat", logical(1)))
      if (!has_flat) {
        return(NULL)
      }
      div(
        class = "warning-note",
        "Flat priors are improper. For plots and summaries, we approximate them with Normal(0, 1000)."
      )
    })

    output$preset_note <- renderUI({
      if (input$preset == "brms") {
        return(div(
          class = "muted-note",
          "Defaults follow brms 2.22.0 docs. Scale terms use the minimum 2.5 where brms adapts to data."
        ))
      }
      if (input$preset == "rstanarm") {
        return(div(
          class = "muted-note",
          "rstanarm defaults are not wired yet."
        ))
      }
      NULL
    })

    applied_inputs <- eventReactive(input$run_sim, {
      defaults <- template$settings
      list(
        settings = list(
          n_sims = safe_value(input$n_sims, defaults$n_sims),
          n_points = safe_value(input$n_points, defaults$n_points),
          x_min = safe_value(input$x_min, defaults$x_min),
          x_max = safe_value(input$x_max, defaults$x_max),
          seed = safe_value(input$seed, 123),
          n_groups = if (analysis_key == "multilevel") safe_value(input$n_groups, defaults$n_groups) else NULL
        ),
        prior_state = prior_state_live(),
        hdi_mass = safe_value(input$hdi_mass, 0.9),
        n_draws = safe_value(input$n_draws, 5000),
        glm_scale = if (is_glm) safe_value(input$glm_scale, "linear") else "linear"
      )
    }, ignoreInit = FALSE)

    output$glm_note <- renderUI({
      if (!is_glm) {
        return(NULL)
      }
      scale <- safe_value(input$glm_scale, "linear")
      if (scale == "exp") {
        return(div(
          class = "muted-note",
          "Exponentiated shows multiplicative effects. Intercept becomes baseline odds/mean at x = 0."
        ))
      }
      div(
        class = "muted-note",
        "Log scale matches the linear predictor (log-odds or log-mean)."
      )
    })

    prior_draws <- reactive({
      applied <- applied_inputs()
      set.seed(applied$settings$seed)
      sample_prior_draws(applied$prior_state, n = applied$n_draws)
    })

    transformed_draws <- reactive({
      draws <- prior_draws()
      if (!is_glm) {
        return(draws)
      }
      scale <- applied_inputs()$glm_scale
      if (scale != "exp") {
        return(draws)
      }
      out <- list()
      for (name in names(draws)) {
        if (name %in% c("intercept", "beta")) {
          out[[paste0(name, " (exp)")]] <- exp(draws[[name]])
        } else {
          out[[name]] <- draws[[name]]
        }
      }
      out
    })

    summary_table <- reactive({
      hdi_mass <- applied_inputs()$hdi_mass
      summarize_draws(transformed_draws(), hdi_mass = hdi_mass)
    })

    implied_data <- reactive({
      applied <- applied_inputs()
      set.seed(applied$settings$seed)
      simulate_analysis(analysis_key, applied$prior_state, applied$settings)
    })

    output$implied_plot <- renderPlot({
      plot_implied(analysis_key, implied_data())
    })

    output$prior_plot <- renderPlot({
      draws <- transformed_draws()
      df <- do.call(rbind, lapply(names(draws), function(name) {
        data.frame(parameter = name, value = draws[[name]], stringsAsFactors = FALSE)
      }))
      plot_prior_density(df)
    })

    format_number <- function(x) {
      if (is.na(x)) {
        return(NA_character_)
      }
      if (x == 0) {
        return("0")
      }
      if (abs(x) >= 1e6 || abs(x) < 1e-4) {
        return(formatC(x, format = "e", digits = 2))
      }
      formatC(x, format = "f", digits = 3)
    }

    format_summary_table <- function(df) {
      out <- df
      numeric_cols <- vapply(out, is.numeric, logical(1))
      for (col in names(out)[numeric_cols]) {
        out[[col]] <- vapply(out[[col]], format_number, character(1))
      }
      out
    }

    output$summary_table <- renderTable({
      format_summary_table(summary_table())
    }, sanitize.text.function = identity)
  })
}
