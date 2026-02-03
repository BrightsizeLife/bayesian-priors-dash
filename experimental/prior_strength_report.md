# Slope Prior Strength Report (Linear Regression)

## Goal
Explore how prior strength on the slope affects the sample size needed for the likelihood to dominate the posterior, and how disagreement between prior and data shifts the posterior.

## Parameter Definitions
- `y`: outcome
- `x`: predictor
- `alpha`: intercept
- `beta`: slope coefficient (parameter of interest)
- `eps`: residual noise
- `sigma`: residual standard deviation (SD)
- `sigma^2`: residual variance
- `mu0`: prior mean for `beta`
- `tau0`: prior SD for `beta` (smaller = more informative)
- `beta_hat`: likelihood mean for `beta` (approx. OLS estimate)
- `n`: sample size
- `Sxx`: sum of squared centered predictors, `sum((x - xbar)^2)`

## Model And Definitions
Assume a simple linear regression with known residual SD:

- Model: `y = alpha + beta * x + eps`, `eps ~ Normal(0, sigma^2)`
- Prior on slope: `beta ~ Normal(mu0, tau0^2)`
- Likelihood for `beta` (approximate): `beta_hat ~ Normal(beta, sigma^2 / Sxx)`
- With standardized `x` (mean 0, var 1), `Sxx ≈ n`

Posterior mean is a precision-weighted average:

- `w_prior = 1 / tau0^2`
- `w_like = Sxx / sigma^2`
- `beta_post = (w_prior * mu0 + w_like * beta_hat) / (w_prior + w_like)`

Definition used here for "likelihood has more impact":

- Likelihood dominates the posterior mean when `w_like > w_prior`
- That implies `Sxx > sigma^2 / tau0^2`
- With standardized `x`: `n > sigma^2 / tau0^2`

## Experiment Setup
Fixed assumptions for this report:

- `sigma = 2` (so `sigma^2 = 4`)
- `x` standardized (var = 1)
- Prior mean `mu0 = 0` for all priors
- Example likelihood mean: `beta_hat = 0.8` (data suggest a positive slope)

Four priors from weak to strong:

- Uninformed: `tau0 = 10`
- Weakly informed: `tau0 = 2.5`
- Moderately informed: `tau0 = 1`
- Highly informed: `tau0 = 0.25`

## Visualizations
Prior share vs sample size:

![](experimental/figs/prior_share_vs_n.png)

Posterior mean vs sample size (mu0 = 0, beta_hat = 0.8):

![](experimental/figs/posterior_mean_vs_n.png)

KL divergence between prior and likelihood (log scale):

![](experimental/figs/kl_vs_n.png)

Prior share heatmap vs likelihood SD and prior SD:

![](experimental/figs/prior_share_heatmap.png)

Interaction plot for posterior shift magnitude:

![](experimental/figs/interaction_abs_shift.png)

## Sensitivity To Key Parameters
We focus on three knobs that determine how much the posterior mean moves:

- `delta = beta_hat - mu0` (mean difference between likelihood and prior)
- `s_like = sigma / sqrt(Sxx)` (likelihood SD for `beta_hat`)
- `tau0` (prior SD)

Posterior shift relative to the likelihood mean:

- `beta_post - beta_hat = -(w_prior / (w_prior + w_like)) * delta`
- `abs_shift = |beta_post - beta_hat|`

So:

- Larger `delta` increases the magnitude of the shift.
- Larger `s_like` weakens the likelihood and increases the shift.
- Larger `tau0` weakens the prior and reduces the shift.

## 2x2 Interaction Quantification (Regression)
We used a simple 2x2x2 factorial design to quantify main effects and pairwise interactions.

Levels:

- `delta`: 0.2 (low) vs 1.0 (high)
- `s_like`: 0.2 (low) vs 1.0 (high)
- `tau0`: 0.3 (low, strong prior) vs 2.0 (high, weak prior)

Response variable: `abs_shift`.
Predictors use effect coding (-0.5, 0.5) so coefficients are *high − low* differences.
This is a deterministic summary (not statistical inference).

| Term | Estimate (abs_shift units) | Interpretation |
| --- | --- | --- |
| Intercept | 0.215 | Grand mean abs_shift |
| delta | 0.287 | High delta increases abs_shift |
| s_like | 0.240 | Higher likelihood SD increases abs_shift |
| tau0 | -0.305 | Higher prior SD decreases abs_shift |
| delta:s_like | 0.320 | Delta effect is stronger when likelihood SD is high |
| delta:tau0 | -0.406 | Delta effect is weaker when prior SD is high |
| s_like:tau0 | -0.252 | Likelihood SD effect is weaker when prior SD is high |

## “Actual Population” Note
If the likelihood is centered on the true population slope (`beta_hat = beta_true`), then the posterior bias is:

- `beta_post - beta_true = (w_prior / (w_prior + w_like)) * (mu0 - beta_true)`

So the distance between the prior mean and the true value drives the bias, and the bias shrinks as the likelihood weight grows.

## Sample Size Thresholds (Likelihood Dominates)
Threshold `n_equal = sigma^2 / tau0^2`. For integer `n`, the smallest `n` with likelihood dominance is `floor(n_equal) + 1`.

| Prior (tau0) | n_equal | Smallest integer n with likelihood dominance |
| --- | --- | --- |
| 10.00 | 0.04 | 1 |
| 2.50 | 0.64 | 1 |
| 1.00 | 4.00 | 5 |
| 0.25 | 64.00 | 65 |

Interpretation:

- For very weak priors, even a handful of observations gives the likelihood more weight in the posterior mean.
- For strong priors (tau0 = 0.25), you need around 65 standardized observations before the likelihood outweighs the prior.

## Example Posterior Weights And Means
Weights are for the posterior mean (`w_prior / (w_prior + w_like)`). Posterior mean uses `beta_hat = 0.8`.

| n | Prior (tau0) | Prior share | Posterior mean |
| --- | --- | --- | --- |
| 5 | 10.00 | 0.008 | 0.794 |
| 5 | 2.50 | 0.113 | 0.709 |
| 5 | 1.00 | 0.444 | 0.444 |
| 5 | 0.25 | 0.928 | 0.058 |
| 20 | 10.00 | 0.002 | 0.798 |
| 20 | 2.50 | 0.031 | 0.775 |
| 20 | 1.00 | 0.167 | 0.667 |
| 20 | 0.25 | 0.762 | 0.190 |
| 80 | 10.00 | 0.000 | 0.800 |
| 80 | 2.50 | 0.008 | 0.794 |
| 80 | 1.00 | 0.048 | 0.762 |
| 80 | 0.25 | 0.444 | 0.444 |

Key takeaway:

- The weight is controlled by sample size, sigma, and x scaling (via `Sxx`).
- The *magnitude* of the posterior shift depends on the gap between `beta_hat` and `mu0`.

Posterior shift from prior mean:

- `beta_post - mu0 = (w_like / (w_prior + w_like)) * (beta_hat - mu0)`

Even with a strong prior, if `beta_hat` is far from `mu0`, the posterior can still move noticeably. If `beta_hat` is close to `mu0`, the posterior will stay near `mu0` even at large `n`.

## Is It Just Sample Size? What About Deviance Or KL Divergence?
Sample size (and `Sxx`) determines the weight of likelihood vs prior. But the *distance between prior and likelihood means* determines how much the posterior actually moves.

A mismatch measure like KL divergence between prior and likelihood can quantify conflict:

- `KL(N(mu0, tau0^2) || N(beta_hat, sigma^2 / n))`
- `= 0.5 * [ log((sigma^2 / n) / tau0^2) + (tau0^2 + (mu0 - beta_hat)^2) / (sigma^2 / n) - 1 ]`

This KL grows when:

- `n` increases (likelihood variance shrinks), and/or
- `beta_hat` is far from `mu0`

So:

- "Influence" is mainly about weights (sample size, sigma, x scaling, prior variance).
- "Conflict" is about how far the likelihood sits from the prior mean (captured by KL or simply the mean difference).

## Dashboard Integration Ideas
If we want this in the Shiny dashboard:

- Add a "Prior Strength" tab under Linear with controls for `sigma`, `x` scaling (or `Sxx`), `mu0`, `tau0`, `beta_hat`, and `n` range.
- Include output plots: prior share vs `n`, posterior mean vs `n`, KL vs `n`.
- Include a table: threshold `n_equal` and the smallest integer `n` where likelihood dominates.
- Add a simple grid viewer that shows all combinations of prior/likelihood settings (a 2x2 or 3x3 matrix).
- Connect to existing priors UI by letting users pick the slope prior from the Linear tab and auto-populate `mu0` and `tau0`.
- Add a toggle for standardized vs raw `x`, exposing `Sxx` if not standardized.
- Add a likelihood family selector so this stays consistent across distributions; use conjugate formulas when available and approximation/MC otherwise.

## Suggestions For Next Steps
- Let users simulate `beta_hat` from a likelihood (sampling) to show variability in influence.
- Extend to unknown `sigma` by integrating over a prior (approximate effective weights).
- Show sensitivity to `x` scaling by plotting vs `Sxx` rather than `n`.
- Add a small “prior–data conflict” badge driven by KL or the standardized mean difference.
- Generalize to more likelihoods (Bernoulli/Beta, Poisson/Gamma, Normal with unknown sigma) using effective sample size or Fisher information.

## Notes
- If `x` is not standardized, replace `n` with `Sxx = sum((x - xbar)^2)`.
- This report uses conjugate normal theory for the slope; for other likelihoods, you can still use the same weight logic (via the Fisher information or approximate likelihood variance).
