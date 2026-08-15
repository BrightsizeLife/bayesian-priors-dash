# Bayesian Priors Explorer — web version (no Shiny)

A dependency-free, client-side port of the Shiny app in the repository root.
Everything — sampling, summaries, simulation, charts — runs in the visitor's
browser. There is no server, no build step, and no framework: plain HTML, CSS,
and ES modules.

**Why this exists:** a Shiny app needs an R server process per visitor
(shinyapps.io, Posit Connect, or your own box). A static page costs nothing to
host, loads instantly, never sleeps, and deploys anywhere. The trade-offs are
listed honestly below.

## Run locally

Any static file server works (ES modules don't load from `file://`):

```sh
cd web
python3 -m http.server 8000
# open http://localhost:8000
```

## Run the tests

```sh
node web/tests/test_core.mjs
```

Covers the samplers (moment checks against closed forms), summaries (exact
matches against R's type-7 quantiles, `sd`, `mad`), simulation shapes, registry
integrity, and the prior-strength math (same reference values as the R test
suite). Exits non-zero on failure; CI runs it on every push.

## Architecture

| Path | Purpose |
|---|---|
| `index.html` | Shell: navbar, tab containers |
| `css/styles.css` | Dark theme (same palette as the Shiny app) |
| `js/rng.js` | Seeded RNG (mulberry32) + normal/gamma/beta/t/cauchy samplers |
| `js/dist.js` | Distribution catalog, help text, `drawDist` |
| `js/registry.js` | Model templates, brms preset, input bounds |
| `js/simulate.js` | Prior-predictive simulation per model family |
| `js/summaries.js` | R-compatible quantiles, HDI, MAD, summary tables |
| `js/priorStrength.js` | Conjugate weight math (with the validation fix) |
| `js/plots.js` | Canvas charts: spaghetti, scatter, KDE densities, line + tooltip |
| `js/modelTab.js` | The six analysis tabs (one data-driven component) |
| `js/priorStrengthTab.js` | Prior Strength tab |
| `js/helpTab.js` | Help cards with density previews |
| `js/app.js` | Tab navigation, lazy init |
| `tests/test_core.mjs` | Node test suite (no browser needed) |

## Limitations vs. the Shiny app

Read this before trusting a number from this version in a paper.

1. **Same seed, different draws.** R uses the Mersenne Twister and its own
   samplers; this port uses mulberry32 + Box-Muller / Marsaglia-Tsang. Setting
   seed 123 here does *not* reproduce the R app's draws. Distributional
   behavior is verified by tests (moments, quantiles, support), and the
   closed-form Prior Strength numbers match R to machine precision — but
   draw-for-draw parity with R does not exist and can't.
2. **Hand-rolled statistical primitives.** Quantiles (type 7), SD, and MAD
   match R exactly (tested against R reference values). The density *plots*
   use a binned Gaussian KDE with R's `nrd0` bandwidth — close to
   `stats::density()` but not bit-identical (R uses FFT smoothing and
   different boundary handling). Shapes are equivalent at a glance; the exact
   curve heights can differ slightly.
3. **Extreme parameters are less hardened.** R's samplers are decades-old C
   with careful edge-case handling. The JS gamma/beta samplers are standard
   algorithms and well-behaved for realistic prior settings, but degenerate
   inputs (e.g. `shape = 1e-12`) are not guaranteed to behave gracefully.
4. **Density displays trim the extreme 1%** (below the 0.5% / above the 99.5%
   quantile) so heavy-tailed priors (Cauchy, horseshoe) remain readable. The
   Shiny app's parameter-prior panels do not trim (its help previews do). The
   summary table always uses untrimmed draws.
5. **Compute happens on the visitor's device.** A phone runs the same
   simulation your laptop does. Inputs are clamped (≤ 5,000 simulations,
   ≤ 50,000 draws) so a stray keystroke can't freeze the tab; the Shiny
   version relies on the server surviving instead. Heavy runs still block the
   UI briefly — a Web Worker would fix that and is future work.
6. **Charts are hand-rolled canvas, not ggplot2.** Layouts match in spirit,
   not pixel-for-pixel. Tick placement, exponent formatting (`1.00e+7` vs
   R's `1.00e+07`), and density smoothing differ in small ways.
7. **The rstanarm preset is omitted** rather than shown as a non-functional
   placeholder. rstanarm's defaults are autoscaled to data this app doesn't
   have; adding them honestly needs an "assumed data scale" input first. The
   brms 2.22.0 preset is fully ported.
8. **Multilevel group effects** use an equivalent bivariate-normal
   construction (same covariance, different factorization order than R's
   `chol()`), so multilevel draws are distributionally identical but not
   numerically re-creatable from the R code path.
9. **No R ecosystem.** The Shiny version can grow into calling brms/rstanarm
   for real; this version can never fit a model — it is a prior *explorer*
   only.
10. **Modern browsers only** (ES modules, no transpilation). Anything
    evergreen from ~2020 onward works; IE does not.

Improvements over the Shiny version, for balance: zero hosting cost and no
cold starts, input clamping, crosshair tooltips on the Prior Strength charts,
alt text on every chart, keyboard-visible focus states, and a test suite wired
into CI.

## Deploying

### Vercel (recommended)

The app is static — no build, no functions, free tier is plenty.

**Dashboard (one-time, ~2 minutes):**
1. [vercel.com/new](https://vercel.com/new) → Import the
   `bayesian-priors-dash` GitHub repo.
2. **Root Directory**: `web` · **Framework Preset**: *Other* · leave Build
   Command and Output Directory empty.
3. Deploy. Every push to `main` redeploys production; every PR gets its own
   preview URL automatically.

**CLI alternative:**
```sh
cd web
npx vercel        # first run links the project; --prod to promote
```

`vercel.json` in this directory adds security headers (CSP, nosniff,
referrer policy) — static hosts serve it automatically.

### Alternatives

- **GitHub Pages** — free, already attached to the repo; needs a tiny Actions
  workflow to publish `web/` and lacks PR previews.
- **Netlify / Cloudflare Pages** — equivalent to Vercel for this app; pick
  whichever dashboard you already live in.

Vercel is recommended here mainly for the automatic per-PR preview deploys,
which pair well with reviewing prior-behavior changes visually.
