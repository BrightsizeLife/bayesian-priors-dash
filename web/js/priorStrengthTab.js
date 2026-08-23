// Prior Strength tab: conjugate prior-vs-likelihood dominance explorer.
// Mirrors R/prior_strength_tab.R; updates live (no run button), and the
// Normal-mean math is shared with the summary table via normalMeanMetrics.

import {
  PRIOR_STRENGTH_SPECS,
  priorStrengthWeights,
  priorStrengthThresholds,
  normalMeanMetrics,
} from "./priorStrength.js";
import { drawLineChart } from "./plots.js";
import { el, card, numberField, selectField, safeNumber } from "./domUtils.js";

export function buildPriorStrengthTab(container) {
  // --- sidebar --------------------------------------------------------------
  const likelihoodSelect = selectField(
    "Family",
    Object.entries(PRIOR_STRENGTH_SPECS).map(([value, spec]) => ({
      value,
      label: spec.label,
    })),
    "normal_mean"
  );
  const likelihoodInputs = el("div");
  const priorInputs = el("div");

  const nMinField = numberField("n min", 5, 1);
  const nMaxField = numberField("n max", 200, 1);
  const nStepField = numberField("n step", 5, 1);
  const nRefField = numberField("n (summary)", 30, 1);

  const fields = {};
  function rebuildInputs() {
    likelihoodInputs.replaceChildren();
    priorInputs.replaceChildren();
    const likelihood = likelihoodSelect.select.value;
    if (likelihood === "normal_mean") {
      fields.beta_hat = numberField("beta_hat (likelihood mean)", 0.8, 0.1);
      fields.sigma = numberField("sigma (likelihood SD)", 2, 0.1);
      likelihoodInputs.append(fields.beta_hat.wrap, fields.sigma.wrap);
      fields.mu0 = numberField("mu0 (prior mean)", 0, 0.1);
      fields.tau0 = numberField("tau0 (prior SD)", 1, 0.1);
      priorInputs.append(fields.mu0.wrap, fields.tau0.wrap);
    } else {
      likelihoodInputs.append(
        el("div", { class: "muted-note" },
          `Likelihood weight uses n for ${likelihood === "bernoulli" ? "Bernoulli/Binomial" : "Poisson"}.`)
      );
      fields.alpha = numberField("alpha", 2, 0.1);
      fields.beta = numberField("beta", likelihood === "bernoulli" ? 2 : 1, 0.1);
      priorInputs.append(fields.alpha.wrap, fields.beta.wrap);
    }
    for (const field of Object.values(fields)) {
      field.input.addEventListener("input", update);
    }
  }

  const sidebar = el("div", { class: "sidebar-panel" },
    el("div", { class: "sidebar-title" }, "Prior Strength"),
    card("Likelihood", el("div", {}, likelihoodSelect.wrap, likelihoodInputs)),
    card("Prior", priorInputs),
    card("Grid", el("div", {}, nMinField.wrap, nMaxField.wrap, nStepField.wrap, nRefField.wrap)));

  // --- main panel -----------------------------------------------------------
  function chartCard(title, ariaLabel) {
    const canvas = el("canvas", { class: "chart", role: "img", "aria-label": ariaLabel });
    const tooltip = el("div", { class: "chart-tooltip", hidden: true });
    const body = el("div", { class: "chart-holder" }, canvas, tooltip);
    return { card: card(title, body), canvas, tooltip };
  }

  const shareChart = chartCard("Prior share vs n",
    "Line chart of the prior's share of posterior weight as sample size grows.");
  const meanChart = chartCard("Posterior mean vs n",
    "Line chart of the posterior mean as sample size grows.");
  const meanFallback = card("Posterior mean",
    el("div", { class: "muted-note" }, "Posterior mean plot is available for Normal mean likelihood only."));
  const klChart = chartCard("KL divergence vs n",
    "Line chart of prior-vs-likelihood KL divergence (log scale) as sample size grows.");
  const tableHolder = el("div", { class: "table-scroll" });
  const note = el("div", { class: "muted-note" });

  const main = el("div", { class: "main-panel" },
    el("div", { class: "plot-row" }, shareChart.card, meanChart.card, meanFallback),
    el("div", { class: "plot-row" }, klChart.card, card("Summary", tableHolder)),
    note);

  container.appendChild(el("div", { class: "tab-layout" }, sidebar, main));

  // --- computation ----------------------------------------------------------
  function nSequence() {
    const nMin = Math.max(1, safeNumber(nMinField.input.value, 5));
    let nMax = safeNumber(nMaxField.input.value, 200);
    if (nMax < nMin) nMax = nMin;
    const step = Math.max(1, safeNumber(nStepField.input.value, 5));
    const seq = [];
    for (let n = nMin; n <= nMax; n += step) seq.push(n);
    return seq;
  }

  function currentParams() {
    const likelihood = likelihoodSelect.select.value;
    if (likelihood === "normal_mean") {
      return {
        likelihood,
        prior: {
          mu0: safeNumber(fields.mu0.input.value, 0),
          sigma0: Math.max(safeNumber(fields.tau0.input.value, 1), 1e-6),
        },
        like: { sigma: Math.max(safeNumber(fields.sigma.input.value, 2), 1e-6) },
        betaHat: safeNumber(fields.beta_hat.input.value, 0.8),
      };
    }
    return {
      likelihood,
      prior: {
        alpha: Math.max(safeNumber(fields.alpha.input.value, 2), 1e-6),
        beta: Math.max(safeNumber(fields.beta.input.value, likelihood === "bernoulli" ? 2 : 1), 1e-6),
      },
      like: {},
    };
  }

  function renderSummary(params, nRef) {
    const thresholds = priorStrengthThresholds(params.likelihood, params.prior, params.like, true);
    const weights = priorStrengthWeights(params.likelihood, params.prior, params.like, nRef);
    const rows = [
      ["n_equal", thresholds.nEqual],
      ["n_min_likelihood_dominates", thresholds.nMinLikelihoodDominates],
      ["n_max_prior_dominates", thresholds.nMaxPriorDominates],
      ["prior_share_at_n", weights.priorShare],
      ["likelihood_share_at_n", weights.likelihoodShare],
    ];
    if (params.likelihood === "normal_mean") {
      const m = normalMeanMetrics(
        params.prior.mu0, params.prior.sigma0, params.betaHat, params.like.sigma, nRef);
      rows.push(["posterior_mean_at_n", m.posteriorMean], ["kl_at_n", m.kl]);
    }
    tableHolder.replaceChildren(
      el("table", { class: "summary-table" },
        el("thead", {}, el("tr", {}, el("th", {}, "metric"), el("th", {}, "value"))),
        el("tbody", {},
          ...rows.map(([metric, value]) =>
            el("tr", {}, el("td", {}, metric), el("td", {}, value.toFixed(3)))))));
  }

  function update() {
    const params = currentParams();
    const nValues = nSequence();
    const nRef = Math.max(1, safeNumber(nRefField.input.value, 30));
    const isNormal = params.likelihood === "normal_mean";

    const share = [];
    const postMean = [];
    const kl = [];
    for (const n of nValues) {
      if (isNormal) {
        const m = normalMeanMetrics(
          params.prior.mu0, params.prior.sigma0, params.betaHat, params.like.sigma, n);
        share.push(m.priorShare);
        postMean.push(m.posteriorMean);
        kl.push(m.kl);
      } else {
        share.push(priorStrengthWeights(params.likelihood, params.prior, params.like, n).priorShare);
      }
    }

    drawLineChart(shareChart.canvas, shareChart.tooltip,
      { x: nValues, y: share },
      {
        xLabel: "n", yLabel: "Prior share", yPercent: true, refY: 0.5,
        subtitle: "< 50% = likelihood dominates", tooltipLabel: "prior share",
      });

    meanChart.card.hidden = !isNormal;
    meanFallback.hidden = isNormal;
    klChart.card.hidden = !isNormal;
    if (isNormal) {
      drawLineChart(meanChart.canvas, meanChart.tooltip,
        { x: nValues, y: postMean },
        { xLabel: "n", yLabel: "Posterior mean", tooltipLabel: "posterior mean" });
      const positiveKl = kl.map((v) => (v > 0 ? v : NaN));
      drawLineChart(klChart.canvas, klChart.tooltip,
        { x: nValues, y: positiveKl },
        { xLabel: "n", yLabel: "KL (log scale)", logY: true, tooltipLabel: "KL" });
    }

    renderSummary(params, nRef);

    note.textContent = isNormal
      ? "Normal mean plots use n and sigma to define likelihood variance. If x is not standardized, use Sxx in place of n."
      : "For non-Normal likelihoods, this tab focuses on weight dominance (prior vs likelihood) rather than posterior mean or KL.";
  }

  likelihoodSelect.select.addEventListener("change", () => {
    rebuildInputs();
    update();
  });
  for (const field of [nMinField, nMaxField, nStepField, nRefField]) {
    field.input.addEventListener("input", update);
  }

  rebuildInputs();
  return { run: update };
}
