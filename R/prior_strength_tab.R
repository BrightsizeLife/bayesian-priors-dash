# Prior strength exploration tab

priorStrengthUI <- function(id) {
  ns <- NS(id)

  sidebar_controls <- tagList(
    div(class = "sidebar-title", "Prior Strength"),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Prior"),
      selectInput(
        ns("prior_dist"),
        "Distribution",
        choices = c(
          "Normal" = "normal",
          "Student t" = "student_t",
          "Cauchy" = "cauchy"
        ),
        selected = "normal"
      ),
      uiOutput(ns("prior_inputs")),
      uiOutput(ns("prior_help"))
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Likelihood"),
      selectInput(
        ns("likelihood"),
        "Family",
        choices = prior_strength_choices(),
        selected = "normal_mean"
      ),
      uiOutput(ns("likelihood_inputs")),
      uiOutput(ns("likelihood_help"))
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Grid"),
      checkboxGroupInput(
        ns("grid_vars"),
        "Vary",
        choices = c(
          "delta (beta_hat - mu0)" = "delta",
          "tau0 (prior SD)" = "tau0",
          "s_like (likelihood SD)" = "s_like"
        ),
        selected = c("delta", "tau0", "s_like")
      ),
      selectInput(
        ns("grid_preset"),
        "Preset",
        choices = c("Custom" = "custom", "2x2" = "preset_2", "3x3" = "preset_3"),
        selected = "preset_3"
      ),
      uiOutput(ns("grid_inputs"))
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Truth (optional)"),
      numericInput(ns("beta_true"), "beta_true", NA_real_, step = 0.1),
      div(class = "muted-note", "Shows bias vs true value when provided.")
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Notes"),
      div(class = "muted-note", "Assumes standardized x unless you provide Sxx. Likelihood SD for beta_hat is s_like = sigma / sqrt(Sxx)."),
      div(class = "muted-note", "See experimental/prior_strength_report.md for context.")
    )
  )

  main_panel <- tagList(
    fluidRow(
      column(
        width = 6,
        bslib::card(
          class = "soft-card",
          bslib::card_header("Prior share vs n"),
          plotOutput(ns("prior_share_plot"), height = 320)
        )
      ),
      column(
        width = 6,
        uiOutput(ns("posterior_mean_card"))
      )
    ),
    fluidRow(
      column(
        width = 6,
        uiOutput(ns("kl_card"))
      ),
      column(
        width = 6,
        bslib::card(
          class = "soft-card",
          bslib::card_header("Summary"),
          tableOutput(ns("summary_table"))
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bslib::card(
          class = "soft-card",
          bslib::card_header("Prior share heatmap"),
          plotOutput(ns("heatmap_plot"), height = 320)
        )
      ),
      column(
        width = 6,
        bslib::card(
          class = "soft-card",
          bslib::card_header("Interaction plot"),
          plotOutput(ns("interaction_plot"), height = 320)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bslib::card(
          class = "soft-card",
          bslib::card_header("Main effects & interactions"),
          tableOutput(ns("effects_table"))
        )
      )
    ),
    uiOutput(ns("note"))
  )

  fluidRow(
    column(width = 3, div(class = "sidebar-panel", sidebar_controls)),
    column(width = 9, div(class = "main-panel", main_panel))
  )
}

priorStrengthServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    strength_theme <- function() {
      theme_minimal(base_size = 12) +
        theme(
          panel.background = element_rect(fill = "#141824", color = NA),
          plot.background = element_rect(fill = "#141824", color = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#2a3140"),
          axis.text = element_text(color = "#cbd5e1"),
          axis.title = element_text(color = "#e6e8f0"),
          strip.background = element_rect(fill = "#1a2030", color = "#293041"),
          strip.text = element_text(color = "#e6e8f0")
        )
    }

    safe_value <- function(value, default) {
      if (is.null(value) || is.na(value)) {
        return(default)
      }
      value
    }

    output$likelihood_inputs <- renderUI({
      if (input$likelihood == "normal_mean") {
        n_label <- if (input$x_scale == "sxx") "Sxx" else "n"
        return(tagList(
          numericInput(session$ns("beta_hat"), "beta_hat (likelihood mean)", 0.8, step = 0.1),
          numericInput(session$ns("sigma"), "sigma (likelihood SD)", 2, min = 0.1, step = 0.1),
          selectInput(
            session$ns("x_scale"),
            "x scaling",
            choices = c("Standardized x (Sxx ≈ n)" = "n", "Provide Sxx" = "sxx"),
            selected = "n"
          ),
          numericInput(session$ns("n_min"), paste(n_label, "min"), 5, min = 1, step = 1),
          numericInput(session$ns("n_max"), paste(n_label, "max"), 200, min = 2, step = 1),
          numericInput(session$ns("n_step"), paste(n_label, "step"), 5, min = 1, step = 1),
          numericInput(session$ns("n_ref"), paste(n_label, "(summary)"), 30, min = 1, step = 1)
        ))
      }
      if (input$likelihood == "bernoulli") {
        return(tagList(
          div(class = "muted-note", "Likelihood weight uses n for Bernoulli/Binomial."),
          numericInput(session$ns("n_min"), "n min", 5, min = 1, step = 1),
          numericInput(session$ns("n_max"), "n max", 200, min = 2, step = 1),
          numericInput(session$ns("n_step"), "n step", 5, min = 1, step = 1),
          numericInput(session$ns("n_ref"), "n (summary)", 30, min = 1, step = 1)
        ))
      }
      if (input$likelihood == "poisson") {
        return(tagList(
          div(class = "muted-note", "Likelihood weight uses n for Poisson."),
          numericInput(session$ns("n_min"), "n min", 5, min = 1, step = 1),
          numericInput(session$ns("n_max"), "n max", 200, min = 2, step = 1),
          numericInput(session$ns("n_step"), "n step", 5, min = 1, step = 1),
          numericInput(session$ns("n_ref"), "n (summary)", 30, min = 1, step = 1)
        ))
      }
      NULL
    })

    output$prior_inputs <- renderUI({
      if (input$likelihood == "normal_mean") {
        if (input$prior_dist == "normal") {
          return(tagList(
            numericInput(session$ns("mu0"), "mu0 (prior mean)", 0, step = 0.1),
            numericInput(session$ns("tau0"), "tau0 (prior SD)", 1, min = 0.01, step = 0.1)
          ))
        }
        if (input$prior_dist == "student_t") {
          return(tagList(
            numericInput(session$ns("df"), "df", 3, min = 2.1, step = 0.1),
            numericInput(session$ns("mu0"), "mu0 (location)", 0, step = 0.1),
            numericInput(session$ns("sigma0"), "sigma0 (scale)", 1, min = 0.01, step = 0.1),
            checkboxInput(session$ns("precision_model"), "Use precision approximation", value = TRUE)
          ))
        }
        if (input$prior_dist == "cauchy") {
          return(tagList(
            numericInput(session$ns("loc0"), "loc0 (location)", 0, step = 0.1),
            numericInput(session$ns("scale0"), "scale0", 1, min = 0.01, step = 0.1),
            checkboxInput(session$ns("precision_model"), "Use precision approximation", value = TRUE)
          ))
        }
        return(NULL)
      }
      if (input$likelihood == "bernoulli") {
        return(tagList(
          numericInput(session$ns("alpha"), "alpha", 2, min = 0.1, step = 0.1),
          numericInput(session$ns("beta"), "beta", 2, min = 0.1, step = 0.1)
        ))
      }
      if (input$likelihood == "poisson") {
        return(tagList(
          numericInput(session$ns("alpha"), "alpha", 2, min = 0.1, step = 0.1),
          numericInput(session$ns("beta"), "beta", 1, min = 0.1, step = 0.1)
        ))
      }
      NULL
    })

    output$prior_help <- renderUI({
      if (input$likelihood != "normal_mean") {
        return(div(class = "muted-note", "Prior distribution selector is used for Normal mean only."))
      }
      help <- get_dist_help(input$prior_dist)
      tagList(
        div(class = "help-text", help$description),
        if (!is.null(help$example)) {
          div(class = "help-text", paste("Example:", help$example))
        }
      )
    })

    output$likelihood_help <- renderUI({
      if (input$likelihood == "normal_mean") {
        return(div(class = "muted-note", "beta_hat is the likelihood mean for the slope."))
      }
      if (input$likelihood == "bernoulli") {
        return(div(class = "muted-note", "Beta prior pseudo-counts compete with n in the likelihood."))
      }
      if (input$likelihood == "poisson") {
        return(div(class = "muted-note", "Gamma prior shape/rate competes with n in the likelihood."))
      }
      NULL
    })

    output$grid_inputs <- renderUI({
      vars <- input$grid_vars
      if (is.null(vars) || length(vars) == 0) {
        return(div(class = "muted-note", "Select at least one variable to vary."))
      }

      defaults <- list(
        delta = c(0.2, 1.0),
        tau0 = c(0.3, 2.0),
        s_like = c(0.2, 1.0)
      )

      rows <- lapply(vars, function(var) {
        default_range <- defaults[[var]]
        min_default <- default_range[[1]]
        max_default <- default_range[[2]]
        tagList(
          numericInput(session$ns(paste0(var, "_min")), paste(var, "min"), min_default, step = 0.1),
          numericInput(session$ns(paste0(var, "_max")), paste(var, "max"), max_default, step = 0.1),
          numericInput(session$ns(paste0(var, "_levels")), paste(var, "levels"), 3, min = 2, step = 1)
        )
      })
      tagList(rows)
    })

    n_seq <- reactive({
      n_min <- safe_value(input$n_min, 5)
      n_max <- safe_value(input$n_max, 200)
      n_step <- safe_value(input$n_step, 5)
      if (n_max < n_min) {
        n_max <- n_min
      }
      seq(n_min, n_max, by = max(1, n_step))
    })

    prior_effective <- reactive({
      if (input$likelihood != "normal_mean") {
        return(list(mu0 = NA_real_, tau0 = NA_real_, note = NULL))
      }
      if (input$prior_dist == "normal") {
        return(list(
          mu0 = safe_value(input$mu0, 0),
          tau0 = safe_value(input$tau0, 1),
          note = NULL
        ))
      }
      if (input$prior_dist == "student_t") {
        df <- safe_value(input$df, 3)
        mu0 <- safe_value(input$mu0, 0)
        sigma0 <- safe_value(input$sigma0, 1)
        if (!isTRUE(input$precision_model)) {
          return(list(mu0 = mu0, tau0 = NA_real_, note = "Enable precision approximation to map to an effective SD."))
        }
        if (df <= 2) {
          return(list(mu0 = mu0, tau0 = NA_real_, note = "Student t variance is undefined for df ≤ 2."))
        }
        tau0 <- sigma0 * sqrt(df / (df - 2))
        return(list(mu0 = mu0, tau0 = tau0, note = NULL))
      }
      if (input$prior_dist == "cauchy") {
        loc0 <- safe_value(input$loc0, 0)
        scale0 <- safe_value(input$scale0, 1)
        if (!isTRUE(input$precision_model)) {
          return(list(mu0 = loc0, tau0 = NA_real_, note = "Enable precision approximation to map to an effective SD."))
        }
        return(list(mu0 = loc0, tau0 = scale0, note = "Cauchy has no variance; using scale as effective SD."))
      }
      list(mu0 = NA_real_, tau0 = NA_real_, note = NULL)
    })

    grid_levels <- reactive({
      vars <- input$grid_vars
      if (is.null(vars) || length(vars) == 0) {
        return(list())
      }
      preset <- input$grid_preset
      preset_levels <- if (preset == "preset_2") 2 else if (preset == "preset_3") 3 else NULL

      levels <- list()
      for (var in vars) {
        min_val <- safe_value(input[[paste0(var, "_min")]], 0.2)
        max_val <- safe_value(input[[paste0(var, "_max")]], 1.0)
        level_count <- if (!is.null(preset_levels)) {
          preset_levels
        } else {
          safe_value(input[[paste0(var, "_levels")]], 3)
        }
        levels[[var]] <- seq(min_val, max_val, length.out = max(2, level_count))
      }
      levels
    })

    grid_data <- reactive({
      levels <- grid_levels()
      if (length(levels) == 0) {
        return(data.frame())
      }
      expand.grid(levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    })

    strength_grid <- reactive({
      likelihood <- input$likelihood
      n_values <- n_seq()

      if (likelihood == "normal_mean") {
        prior <- prior_effective()
        mu0 <- prior$mu0
        tau0 <- prior$tau0
        beta_hat <- safe_value(input$beta_hat, 0.8)
        sigma <- safe_value(input$sigma, 2)
        x_scale <- safe_value(input$x_scale, "n")

        if (is.na(tau0) || tau0 <= 0) {
          return(data.frame(n = n_values, prior_share = NA_real_, posterior_mean = NA_real_, kl = NA_real_))
        }

        sxx_values <- n_values
        if (x_scale == "sxx") {
          sxx_values <- n_values
        }

        w_prior <- 1 / (tau0^2)
        w_like <- sxx_values / (sigma^2)
        prior_share <- w_prior / (w_prior + w_like)
        posterior_mean <- (w_prior * mu0 + w_like * beta_hat) / (w_prior + w_like)
        s2 <- (sigma^2) / sxx_values
        kl <- 0.5 * (log(s2 / (tau0^2)) + (tau0^2 + (mu0 - beta_hat)^2) / s2 - 1)

        return(data.frame(
          n = n_values,
          prior_share = prior_share,
          posterior_mean = posterior_mean,
          kl = kl,
          stringsAsFactors = FALSE
        ))
      }

      if (likelihood %in% c("bernoulli", "poisson")) {
        if (likelihood == "bernoulli") {
          prior_params <- list(alpha = safe_value(input$alpha, 2), beta = safe_value(input$beta, 2))
        } else {
          prior_params <- list(alpha = safe_value(input$alpha, 2), beta = safe_value(input$beta, 1))
        }
        like_params <- list()
        w_prior <- prior_strength_weights(likelihood, prior_params, like_params, n = n_values[1])$prior_weight
        prior_share <- vapply(n_values, function(n_val) {
          weights <- prior_strength_weights(likelihood, prior_params, like_params, n = n_val)
          weights$prior_share
        }, numeric(1))

        return(data.frame(
          n = n_values,
          prior_share = prior_share,
          posterior_mean = NA_real_,
          kl = NA_real_,
          stringsAsFactors = FALSE
        ))
      }

      data.frame(n = n_values, prior_share = NA_real_, posterior_mean = NA_real_, kl = NA_real_)
    })

    grid_metrics <- reactive({
      if (input$likelihood != "normal_mean") {
        return(data.frame())
      }
      prior <- prior_effective()
      mu0 <- prior$mu0
      tau0_fixed <- prior$tau0
      beta_hat <- safe_value(input$beta_hat, 0.8)
      sigma <- safe_value(input$sigma, 2)
      n_ref <- safe_value(input$n_ref, 30)
      x_scale <- safe_value(input$x_scale, "n")
      s_like_fixed <- sigma / sqrt(n_ref)

      if (is.na(tau0_fixed) || tau0_fixed <= 0) {
        return(data.frame())
      }

      grid <- grid_data()
      if (nrow(grid) == 0) {
        return(data.frame())
      }

      if (!"delta" %in% names(grid)) {
        grid$delta <- beta_hat - mu0
      }
      if (!"tau0" %in% names(grid)) {
        grid$tau0 <- tau0_fixed
      }
      if (!"s_like" %in% names(grid)) {
        grid$s_like <- s_like_fixed
      }

      w_prior <- 1 / (grid$tau0^2)
      w_like <- 1 / (grid$s_like^2)
      prior_share <- w_prior / (w_prior + w_like)
      posterior_mean <- mu0 + (w_like / (w_prior + w_like)) * grid$delta
      abs_shift <- abs((w_prior / (w_prior + w_like)) * grid$delta)
      kl <- 0.5 * (log(grid$s_like^2 / (grid$tau0^2)) + ((grid$tau0^2 + grid$delta^2) / (grid$s_like^2)) - 1)

      cbind(grid, prior_share = prior_share, posterior_mean = posterior_mean, abs_shift = abs_shift, kl = kl)
    })

    output$prior_share_plot <- renderPlot({
      df <- strength_grid()
      x_scale <- safe_value(input$x_scale, "n")
      x_label <- if (x_scale == "sxx") "Sxx" else "n"
      ggplot(df, aes(x = n, y = prior_share)) +
        geom_line(color = "#7dd3fc", linewidth = 0.9) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = x_label, y = "Prior share", subtitle = "< 50% = likelihood dominates") +
        strength_theme()
    })

    output$posterior_mean_card <- renderUI({
      if (input$likelihood != "normal_mean") {
        return(bslib::card(
          class = "soft-card",
          bslib::card_header("Posterior mean"),
          div(class = "muted-note", "Posterior mean plot is available for Normal mean likelihood only.")
        ))
      }
      bslib::card(
        class = "soft-card",
        bslib::card_header("Posterior mean vs n"),
        plotOutput(session$ns("posterior_mean_plot"), height = 320)
      )
    })

    output$posterior_mean_plot <- renderPlot({
      req(input$likelihood == "normal_mean")
      df <- strength_grid()
      beta_true <- input$beta_true
      ggplot(df, aes(x = n, y = posterior_mean)) +
        geom_line(color = "#7dd3fc", linewidth = 0.9) +
        if (!is.na(beta_true)) geom_hline(yintercept = beta_true, linetype = "dashed", color = "#fca5a5") else NULL +
        labs(x = "n", y = "Posterior mean") +
        strength_theme()
    })

    output$kl_card <- renderUI({
      if (input$likelihood != "normal_mean") {
        return(NULL)
      }
      bslib::card(
        class = "soft-card",
        bslib::card_header("KL divergence vs n"),
        plotOutput(session$ns("kl_plot"), height = 320)
      )
    })

    output$kl_plot <- renderPlot({
      req(input$likelihood == "normal_mean")
      df <- strength_grid()
      ggplot(df, aes(x = n, y = kl)) +
        geom_line(color = "#7dd3fc", linewidth = 0.9) +
        scale_y_log10() +
        labs(x = "n", y = "KL (log scale)") +
        strength_theme()
    })

    output$heatmap_plot <- renderPlot({
      grid <- grid_metrics()
      if (nrow(grid) == 0 || !all(c("tau0", "s_like") %in% names(grid))) {
        return(NULL)
      }
      if (length(unique(grid$tau0)) < 2 || length(unique(grid$s_like)) < 2) {
        return(NULL)
      }
      ggplot(grid, aes(x = tau0, y = s_like, fill = prior_share)) +
        geom_tile(color = "#202636") +
        scale_fill_gradient(low = "#111827", high = "#7dd3fc") +
        labs(x = "tau0", y = "s_like", fill = "Prior share") +
        strength_theme()
    })

    output$interaction_plot <- renderPlot({
      grid <- grid_metrics()
      if (nrow(grid) == 0 || !all(c("delta", "tau0", "s_like") %in% names(grid))) {
        return(NULL)
      }
      if (length(unique(grid$delta)) < 2 || length(unique(grid$tau0)) < 2 || length(unique(grid$s_like)) < 2) {
        return(NULL)
      }
      ggplot(grid, aes(x = delta, y = abs_shift, color = factor(round(tau0, 3)))) +
        geom_line(linewidth = 0.8) +
        facet_wrap(~ s_like, scales = "free_y") +
        labs(x = "delta", y = "abs_shift", color = "tau0") +
        strength_theme()
    })

    output$summary_table <- renderTable({
      likelihood <- input$likelihood
      n_ref <- safe_value(input$n_ref, 30)

      if (likelihood == "normal_mean") {
        prior <- prior_effective()
        prior_params <- list(mu0 = prior$mu0, sigma0 = prior$tau0)
        like_params <- list(sigma = safe_value(input$sigma, 2))
        if (is.na(prior$tau0) || prior$tau0 <= 0) {
          return(data.frame(metric = "note", value = "Invalid prior scale; enable precision approximation or adjust parameters.", stringsAsFactors = FALSE))
        }
      } else if (likelihood == "bernoulli") {
        prior_params <- list(alpha = safe_value(input$alpha, 2), beta = safe_value(input$beta, 2))
        like_params <- list()
      } else {
        prior_params <- list(alpha = safe_value(input$alpha, 2), beta = safe_value(input$beta, 1))
        like_params <- list()
      }

      thresholds <- prior_strength_thresholds(likelihood, prior_params, like_params, n_is_integer = TRUE)
      weights <- prior_strength_weights(likelihood, prior_params, like_params, n = n_ref)

      rows <- list(
        data.frame(metric = "n_equal", value = thresholds$n_equal, stringsAsFactors = FALSE),
        data.frame(metric = "n_min_likelihood_dominates", value = thresholds$n_min_likelihood_dominates, stringsAsFactors = FALSE),
        data.frame(metric = "n_max_prior_dominates", value = thresholds$n_max_prior_dominates, stringsAsFactors = FALSE),
        data.frame(metric = "prior_share_at_n", value = weights$prior_share, stringsAsFactors = FALSE),
        data.frame(metric = "likelihood_share_at_n", value = weights$likelihood_share, stringsAsFactors = FALSE)
      )

      if (likelihood == "normal_mean") {
        mu0 <- prior$mu0
        tau0 <- prior$tau0
        beta_hat <- safe_value(input$beta_hat, 0.8)
        sigma <- safe_value(input$sigma, 2)
        w_prior <- 1 / (tau0^2)
        w_like <- n_ref / (sigma^2)
        post_mean <- (w_prior * mu0 + w_like * beta_hat) / (w_prior + w_like)
        s2 <- (sigma^2) / n_ref
        kl_val <- 0.5 * (log(s2 / (tau0^2)) + (tau0^2 + (mu0 - beta_hat)^2) / s2 - 1)
        rows <- c(rows,
          list(
            data.frame(metric = "posterior_mean_at_n", value = post_mean, stringsAsFactors = FALSE),
            data.frame(metric = "kl_at_n", value = kl_val, stringsAsFactors = FALSE)
          )
        )

        grid <- grid_metrics()
        if (nrow(grid) > 0 && "prior_share" %in% names(grid)) {
          rows <- c(rows, list(
            data.frame(metric = "grid_likelihood_dominates_count", value = sum(grid$prior_share < 0.5), stringsAsFactors = FALSE),
            data.frame(metric = "grid_total_points", value = nrow(grid), stringsAsFactors = FALSE)
          ))
        }
      }

      do.call(rbind, rows)
    }, digits = 3)

    output$effects_table <- renderTable({
      grid <- grid_metrics()
      if (nrow(grid) == 0 || !all(c("delta", "tau0", "s_like") %in% names(grid))) {
        return(data.frame(note = "Select grid variables to compute effects.", stringsAsFactors = FALSE))
      }

      levels_ok <- all(c(
        length(unique(grid$delta)) == 2,
        length(unique(grid$tau0)) == 2,
        length(unique(grid$s_like)) == 2
      ))

      if (!levels_ok) {
        return(data.frame(note = "Effects table uses 2-level grids. Use preset 2x2 for delta, tau0, s_like.", stringsAsFactors = FALSE))
      }

      effect_code <- function(x) {
        vals <- sort(unique(x))
        if (length(vals) != 2) return(rep(NA_real_, length(x)))
        ifelse(x == vals[1], -0.5, 0.5)
      }

      df <- data.frame(
        delta = effect_code(grid$delta),
        tau0 = effect_code(grid$tau0),
        s_like = effect_code(grid$s_like),
        abs_shift = grid$abs_shift
      )
      df$delta_s_like <- df$delta * df$s_like
      df$delta_tau0 <- df$delta * df$tau0
      df$s_like_tau0 <- df$s_like * df$tau0

      fit <- stats::lm(abs_shift ~ delta + tau0 + s_like + delta_s_like + delta_tau0 + s_like_tau0, data = df)
      coefs <- summary(fit)$coefficients
      out <- data.frame(term = rownames(coefs), estimate = coefs[, "Estimate"], stringsAsFactors = FALSE)
      rownames(out) <- NULL
      out
    }, digits = 3)

    output$note <- renderUI({
      if (input$likelihood == "normal_mean") {
        prior <- prior_effective()
        notes <- list(
          div(class = "muted-note", "Normal mean plots use n and sigma to define likelihood variance. If x is not standardized, use Sxx in place of n.")
        )
        if (!is.null(prior$note)) {
          notes <- c(notes, list(div(class = "warning-note", prior$note)))
        }
        return(tagList(notes))
      }
      div(class = "muted-note", "For non-Normal likelihoods, this tab focuses on weight dominance (prior vs likelihood) rather than posterior mean or KL.")
    })
  })
}
