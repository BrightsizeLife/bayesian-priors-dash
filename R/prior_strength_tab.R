# Prior strength exploration tab

priorStrengthUI <- function(id) {
  ns <- NS(id)

  sidebar_controls <- tagList(
    div(class = "sidebar-title", "Prior Strength"),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Likelihood"),
      selectInput(
        ns("likelihood"),
        "Family",
        choices = prior_strength_choices(),
        selected = "normal_mean"
      ),
      uiOutput(ns("likelihood_inputs"))
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Prior"),
      uiOutput(ns("prior_inputs"))
    ),
    bslib::card(
      class = "soft-card",
      bslib::card_header("Grid"),
      numericInput(ns("n_min"), "n min", 5, min = 1, step = 1),
      numericInput(ns("n_max"), "n max", 200, min = 2, step = 1),
      numericInput(ns("n_step"), "n step", 5, min = 1, step = 1),
      numericInput(ns("n_ref"), "n (summary)", 30, min = 1, step = 1)
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
    uiOutput(ns("note"))
  )

  fluidRow(
    column(width = 3, div(class = "sidebar-panel", sidebar_controls)),
    column(width = 9, div(class = "main-panel", main_panel))
  )
}

priorStrengthServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    safe_value <- function(value, default) {
      if (is.null(value) || is.na(value)) {
        return(default)
      }
      value
    }

    output$likelihood_inputs <- renderUI({
      if (input$likelihood == "normal_mean") {
        return(tagList(
          numericInput(session$ns("beta_hat"), "beta_hat (likelihood mean)", 0.8, step = 0.1),
          numericInput(session$ns("sigma"), "sigma (likelihood SD)", 2, min = 0.1, step = 0.1)
        ))
      }
      if (input$likelihood == "bernoulli") {
        return(div(class = "muted-note", "Likelihood weight uses n for Bernoulli/Binomial."))
      }
      if (input$likelihood == "poisson") {
        return(div(class = "muted-note", "Likelihood weight uses n for Poisson."))
      }
      NULL
    })

    output$prior_inputs <- renderUI({
      if (input$likelihood == "normal_mean") {
        return(tagList(
          numericInput(session$ns("mu0"), "mu0 (prior mean)", 0, step = 0.1),
          numericInput(session$ns("tau0"), "tau0 (prior SD)", 1, min = 0.01, step = 0.1)
        ))
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

    n_seq <- reactive({
      n_min <- safe_value(input$n_min, 5)
      n_max <- safe_value(input$n_max, 200)
      n_step <- safe_value(input$n_step, 5)
      if (n_max < n_min) {
        n_max <- n_min
      }
      seq(n_min, n_max, by = max(1, n_step))
    })

    strength_grid <- reactive({
      likelihood <- input$likelihood
      n_values <- n_seq()

      if (likelihood == "normal_mean") {
        mu0 <- safe_value(input$mu0, 0)
        tau0 <- safe_value(input$tau0, 1)
        beta_hat <- safe_value(input$beta_hat, 0.8)
        sigma <- safe_value(input$sigma, 2)

        w_prior <- 1 / (tau0^2)
        w_like <- n_values / (sigma^2)
        prior_share <- w_prior / (w_prior + w_like)
        posterior_mean <- (w_prior * mu0 + w_like * beta_hat) / (w_prior + w_like)
        s2 <- (sigma^2) / n_values
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

    output$prior_share_plot <- renderPlot({
      df <- strength_grid()
      ggplot(df, aes(x = n, y = prior_share)) +
        geom_line(color = "#7dd3fc", linewidth = 0.9) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "n", y = "Prior share", subtitle = "< 50% = likelihood dominates") +
        theme_minimal(base_size = 12)
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
      ggplot(df, aes(x = n, y = posterior_mean)) +
        geom_line(color = "#7dd3fc", linewidth = 0.9) +
        labs(x = "n", y = "Posterior mean") +
        theme_minimal(base_size = 12)
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
        theme_minimal(base_size = 12)
    })

    output$summary_table <- renderTable({
      likelihood <- input$likelihood
      n_ref <- safe_value(input$n_ref, 30)

      if (likelihood == "normal_mean") {
        prior_params <- list(mu0 = safe_value(input$mu0, 0), sigma0 = safe_value(input$tau0, 1))
        like_params <- list(sigma = safe_value(input$sigma, 2))
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
        mu0 <- safe_value(input$mu0, 0)
        tau0 <- safe_value(input$tau0, 1)
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
      }

      do.call(rbind, rows)
    }, digits = 3)

    output$note <- renderUI({
      if (input$likelihood == "normal_mean") {
        return(div(class = "muted-note", "Normal mean plots use n and sigma to define likelihood variance. If x is not standardized, use Sxx in place of n."))
      }
      div(class = "muted-note", "For non-Normal likelihoods, this tab focuses on weight dominance (prior vs likelihood) rather than posterior mean or KL.")
    })
  })
}
