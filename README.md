# Bayesian Priors Explorer

A Shiny dashboard for building intuition about Bayesian priors before you fit
a model. Pick a prior, see what data it *implies*, and measure how strongly it
competes with the likelihood as sample size grows.

## What it does

- **Model tabs (Linear, Logistic, Poisson, Gamma, NegBin, Multilevel)** — for
  each model family, choose prior distributions for every parameter (intercept,
  slopes, scales, group SDs, correlation), then press **Run simulations** to see:
  - *Implied data*: outcomes simulated from the priors alone (a prior
    predictive check), so unreasonable priors are visible before fitting.
  - *Parameter priors*: density plots of the prior draws.
  - *Prior summaries*: mean, SD, MAD, HDI, and quantiles per parameter.
  GLM tabs can display priors and summaries on the linear-predictor scale or
  the exponentiated (probability / odds-ratio / multiplicative) scale.
- **Prior Strength tab** — for conjugate cases (Normal mean, Beta–Bernoulli,
  Gamma–Poisson), shows the prior's share of the posterior weight versus
  sample size `n`, the `n` at which prior and likelihood balance, and (for the
  Normal case) the posterior mean and prior-vs-likelihood KL divergence.
- **Help tab** — a card per distribution with a shape preview, parameter
  glossary, typical uses, and sources.

Presets: `brms 2.22.0 defaults` sets the priors brms would use (scale terms use
brms's minimum 2.5 where brms adapts to data). The `rstanarm` preset is a
placeholder and is not wired yet. Flat (improper) priors are approximated by
Normal(0, 1000) for plotting and summaries; a warning appears whenever one is
selected.

## Running the app

Requires R (≥ 4.1) with:

```r
install.packages(c("shiny", "bslib", "ggplot2"))
```

Then, from the repository root:

```r
shiny::runApp()
```

## Repository layout

| Path | Purpose |
|---|---|
| `app.R` | App entry point: theme, navbar, module wiring |
| `R/dist.R` | Distribution catalog (`DIST_SPECS`), help text, and samplers (`draw_dist`) |
| `R/priors_registry.R` | Per-model parameter templates, defaults, and presets |
| `R/simulation.R` | Prior-predictive simulation for each model family |
| `R/summaries.R` | HDI and summary-table helpers |
| `R/plots.R` | ggplot helpers and the shared dark theme (`theme_dash`) |
| `R/modules.R` | Shiny module (UI + server) reused by all six model tabs |
| `R/prior_strength.R` | Conjugate prior-vs-likelihood weight math |
| `R/prior_strength_tab.R` | Prior Strength tab UI + server |
| `R/help_ui.R`, `R/help_server.R` | Help tab cards and distribution previews |
| `www/styles.css` | Dark-theme styling for cards, tables, and inputs |
| `tests/test_core.R` | Tests for the non-Shiny core (see below) |
| `experimental/` | Notes and figures for the prior-strength write-up |

## Tests

The core math (samplers, summaries, simulation shapes, prior-strength weights)
is covered by a dependency-free test script:

```sh
Rscript tests/test_core.R
```

It exits non-zero on failure, so it can be used as a pre-commit or CI check.
