library(shiny)
library(bslib)
library(ggplot2)

source("R/dist.R")
source("R/priors_registry.R")
source("R/summaries.R")
source("R/simulation.R")
source("R/plots.R")
source("R/modules.R")
source("R/prior_strength.R")
source("R/prior_strength_tab.R")
source("R/help_ui.R")

app_theme <- bs_theme(
  version = 5,
  bg = "#0f1117",
  fg = "#e6e8f0",
  primary = "#7dd3fc",
  secondary = "#9aa4b5"
)

ui <- page_navbar(
  title = "Bayesian Priors Explorer",
  theme = app_theme,
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  nav_panel("Linear", analysisModuleUI("linear", "linear")),
  nav_panel("Logistic", analysisModuleUI("logistic", "logistic")),
  nav_panel("Poisson", analysisModuleUI("poisson", "poisson")),
  nav_panel("Gamma", analysisModuleUI("gamma", "gamma")),
  nav_panel("NegBin", analysisModuleUI("negbin", "negbin")),
  nav_panel("Multilevel", analysisModuleUI("multilevel", "multilevel")),
  nav_panel("Prior Strength", priorStrengthUI("prior_strength")),
  nav_panel("Help", helpTabUI())
)

server <- function(input, output, session) {
  analysisModuleServer("linear", "linear")
  analysisModuleServer("logistic", "logistic")
  analysisModuleServer("poisson", "poisson")
  analysisModuleServer("gamma", "gamma")
  analysisModuleServer("negbin", "negbin")
  analysisModuleServer("multilevel", "multilevel")
  priorStrengthServer("prior_strength")
}

shinyApp(ui, server)
