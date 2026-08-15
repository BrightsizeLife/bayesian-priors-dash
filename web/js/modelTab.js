// Generic analysis tab (Linear, Logistic, Poisson, Gamma, NegBin, Multilevel).
// One component driven by the registry, like the Shiny module in R/modules.R.

import { DIST_SPECS, getDistHelp, resolveParams } from "./dist.js";
import {
  MODEL_PURPOSE,
  PRIOR_TEMPLATES,
  PRIOR_PRESETS,
  SETTING_BOUNDS,
  GLM_ANALYSES,
} from "./registry.js";
import { createRng } from "./rng.js";
import { samplePriorDraws, simulateAnalysis } from "./simulate.js";
import { summarizeDraws, formatNumber } from "./summaries.js";
import { drawCurves, drawScatter, drawDensity } from "./plots.js";
import { el, card, numberField, selectField, clamp, safeNumber } from "./domUtils.js";

const plogis = (eta) => 1 / (1 + Math.exp(-eta));

const SUMMARY_COLUMNS = [
  ["parameter", "parameter"],
  ["mean", "mean"],
  ["sd", "sd"],
  ["mad", "mad"],
  ["hdi_lower", "hdi lower"],
  ["hdi_upper", "hdi upper"],
  ["p10", "p10"],
  ["p25", "p25"],
  ["p50", "p50"],
  ["p75", "p75"],
  ["p90", "p90"],
];

export function buildModelTab(container, analysisKey) {
  const template = PRIOR_TEMPLATES[analysisKey];
  const presets = PRIOR_PRESETS[analysisKey];
  const isGlm = GLM_ANALYSES.includes(analysisKey);

  // --- state ----------------------------------------------------------------
  const state = {
    dists: {},
    paramValues: {},
  };
  for (const [name, param] of Object.entries(template.parameters)) {
    state.dists[name] = param.default.dist;
    state.paramValues[name] = { ...resolveParams(param.default.dist, param.default.params) };
  }

  // --- sidebar --------------------------------------------------------------
  const flatWarning = el("div", { class: "warning-note", hidden: true },
    "Flat priors are improper. For plots and summaries, we approximate them with Normal(0, 1000).");

  const presetNote = el("div", { class: "muted-note", hidden: true });
  const presetSelect = selectField("Preset", [
    { value: "custom", label: "Custom" },
    { value: "brms", label: "brms 2.22.0 defaults" },
  ]);
  presetSelect.select.addEventListener("change", () => {
    const preset = presets[presetSelect.select.value];
    if (!preset) return;
    for (const name of Object.keys(template.parameters)) {
      state.dists[name] = preset[name].dist;
      state.paramValues[name] = { ...resolveParams(preset[name].dist, preset[name].params) };
    }
    renderParamCards();
    updateFlatWarning();
    presetNote.hidden = presetSelect.select.value !== "brms";
    presetNote.textContent =
      "Defaults follow brms 2.22.0 docs. Scale terms use the minimum 2.5 where brms adapts to data.";
  });

  const paramCardsHolder = el("div");

  function updateFlatWarning() {
    flatWarning.hidden = !Object.values(state.dists).includes("flat");
  }

  function renderParamCards() {
    paramCardsHolder.replaceChildren();
    for (const [name, param] of Object.entries(template.parameters)) {
      const body = el("div");
      const distField = selectField(
        "Distribution",
        param.allowed.map((d) => ({ value: d, label: DIST_SPECS[d].label })),
        state.dists[name]
      );
      distField.select.addEventListener("change", () => {
        state.dists[name] = distField.select.value;
        // Merge the template's default params into the new distribution's
        // defaults where names overlap (same behavior as the Shiny app).
        state.paramValues[name] = {
          ...resolveParams(distField.select.value, param.default.params),
        };
        renderParamInputs();
        updateFlatWarning();
        presetSelect.select.value = "custom";
        presetNote.hidden = true;
      });
      body.appendChild(distField.wrap);

      const inputsHolder = el("div");
      body.appendChild(inputsHolder);

      function renderParamInputs() {
        inputsHolder.replaceChildren();
        const dist = state.dists[name];
        const help = getDistHelp(dist);
        for (const key of Object.keys(DIST_SPECS[dist].params)) {
          const field = numberField(key, state.paramValues[name][key], 0.1);
          field.input.addEventListener("change", () => {
            state.paramValues[name][key] = safeNumber(
              field.input.value,
              DIST_SPECS[dist].params[key]
            );
          });
          inputsHolder.appendChild(field.wrap);
        }
        if (help.description) {
          inputsHolder.appendChild(el("div", { class: "help-text" }, help.description));
        }
        const paramRows = Object.entries(help.params || {}).map(([key, desc]) =>
          el("div", { class: "help-param" },
            el("span", { class: "help-param-name" }, key),
            el("span", { class: "help-param-desc" }, desc))
        );
        if (paramRows.length > 0) {
          inputsHolder.appendChild(el("div", { class: "help-params" }, ...paramRows));
        }
      }
      renderParamInputs();

      paramCardsHolder.appendChild(card(param.label, body));
    }
  }
  renderParamCards();

  const runButton = el("button", { class: "run-button", type: "button" }, "Run simulations");
  const runCard = card("Run", el("div", {},
    runButton,
    el("div", { class: "muted-note" }, "Updates only when you press run.")));

  let glmScaleSelect = null;
  const glmNote = el("div", { class: "muted-note" });
  let glmCard = null;
  if (isGlm) {
    const choices =
      analysisKey === "logistic"
        ? [
            { value: "linear", label: "Log-odds (linear predictor)" },
            { value: "exp", label: "Probability / odds ratio" },
          ]
        : [
            { value: "linear", label: "Log scale (linear predictor)" },
            { value: "exp", label: "Exponentiated (multiplicative)" },
          ];
    glmScaleSelect = selectField("Scale for priors & summaries", choices);
    const updateGlmNote = () => {
      const scale = glmScaleSelect.select.value;
      if (scale === "exp" && analysisKey === "logistic") {
        glmNote.textContent =
          "Intercept becomes baseline probability at x = 0. Slopes become odds ratios; medians/quantiles are most stable.";
      } else if (scale === "exp") {
        glmNote.textContent =
          "Exponentiated shows multiplicative effects. Intercept becomes baseline mean at x = 0.";
      } else {
        glmNote.textContent = "Log scale matches the linear predictor (log-odds or log-mean).";
      }
    };
    glmScaleSelect.select.addEventListener("change", updateGlmNote);
    updateGlmNote();
    glmCard = card("GLM scale", el("div", {}, glmScaleSelect.wrap, glmNote));
  }

  const settings = template.settings;
  const nSimsField = numberField("Simulations", settings.n_sims, 50);
  const nPointsField = numberField("Points per sim", settings.n_points, 5);
  const xMinField = numberField("x min", settings.x_min, 0.5);
  const xMaxField = numberField("x max", settings.x_max, 0.5);
  const seedField = numberField("Seed", 123, 1);
  const nGroupsField =
    analysisKey === "multilevel" ? numberField("Groups", settings.n_groups, 1) : null;
  const simCard = card("Simulation", el("div", {},
    nSimsField.wrap, nPointsField.wrap, xMinField.wrap, xMaxField.wrap, seedField.wrap,
    ...(nGroupsField ? [nGroupsField.wrap] : [])));

  const nDrawsField = numberField("Draws", 5000, 500);
  const hdiMassField = numberField("HDI mass", 0.9, 0.01);
  const summariesCard = card("Summaries", el("div", {}, nDrawsField.wrap, hdiMassField.wrap));

  const sidebar = el("div", { class: "sidebar-panel" },
    el("div", { class: "sidebar-title" }, template.title),
    card("Model purpose", el("div", { class: "model-purpose" }, MODEL_PURPOSE[analysisKey])),
    presetSelect.wrap,
    presetNote,
    flatWarning,
    paramCardsHolder,
    runCard,
    ...(glmCard ? [glmCard] : []),
    simCard,
    summariesCard);

  // --- main panel -----------------------------------------------------------
  const impliedCanvas = el("canvas", {
    class: "chart chart-tall",
    role: "img",
    "aria-label": `Implied data simulated from the priors for ${template.title.toLowerCase()}.`,
  });
  const densityHolder = el("div", { class: "density-grid" });
  const tableHolder = el("div", { class: "table-scroll" });

  const main = el("div", { class: "main-panel" },
    el("div", { class: "plot-row" },
      card("Implied data", impliedCanvas),
      card("Parameter priors", densityHolder)),
    card("Prior summaries", tableHolder));

  container.appendChild(el("div", { class: "tab-layout" }, sidebar, main));

  // --- run ------------------------------------------------------------------
  function readSettings() {
    const b = SETTING_BOUNDS;
    return {
      n_sims: clamp(safeNumber(nSimsField.input.value, settings.n_sims), b.n_sims),
      n_points: clamp(safeNumber(nPointsField.input.value, settings.n_points), b.n_points),
      x_min: safeNumber(xMinField.input.value, settings.x_min),
      x_max: safeNumber(xMaxField.input.value, settings.x_max),
      seed: Math.round(safeNumber(seedField.input.value, 123)),
      n_groups: nGroupsField
        ? clamp(safeNumber(nGroupsField.input.value, settings.n_groups), b.n_groups)
        : undefined,
    };
  }

  function priorState() {
    const out = {};
    for (const name of Object.keys(template.parameters)) {
      out[name] = { dist: state.dists[name], params: { ...state.paramValues[name] } };
    }
    return out;
  }

  function transformDraws(draws, glmScale) {
    if (!isGlm || glmScale !== "exp") return draws;
    const out = {};
    if (analysisKey === "logistic") {
      if (draws.intercept) out["intercept (prob)"] = Float64Array.from(draws.intercept, plogis);
      if (draws.beta) out["beta (OR)"] = Float64Array.from(draws.beta, Math.exp);
    } else {
      if (draws.intercept) out["intercept (exp)"] = Float64Array.from(draws.intercept, Math.exp);
      if (draws.beta) out["beta (exp)"] = Float64Array.from(draws.beta, Math.exp);
    }
    for (const name of Object.keys(draws)) {
      if (name !== "intercept" && name !== "beta") out[name] = draws[name];
    }
    return out;
  }

  function renderTable(rows) {
    const thead = el("thead", {},
      el("tr", {}, ...SUMMARY_COLUMNS.map(([, label]) => el("th", {}, label))));
    const tbody = el("tbody", {},
      ...rows.map((row) =>
        el("tr", {},
          ...SUMMARY_COLUMNS.map(([key]) =>
            el("td", {}, key === "parameter" ? row[key] : formatNumber(row[key]))))));
    tableHolder.replaceChildren(el("table", { class: "summary-table" }, thead, tbody));
  }

  function run() {
    const applied = readSettings();
    const nDraws = clamp(safeNumber(nDrawsField.input.value, 5000), SETTING_BOUNDS.n_draws);
    const hdiMass = clamp(safeNumber(hdiMassField.input.value, 0.9), SETTING_BOUNDS.hdi_mass);
    const glmScale = glmScaleSelect ? glmScaleSelect.select.value : "linear";
    const priors = priorState();

    if (applied.x_max <= applied.x_min) {
      applied.x_max = applied.x_min + 1;
    }

    // Two independent streams from the same seed, matching the Shiny app's
    // set.seed() before summaries and before the implied-data simulation.
    const draws = transformDraws(samplePriorDraws(createRng(applied.seed), priors, nDraws), glmScale);
    renderTable(summarizeDraws(draws, hdiMass));

    densityHolder.replaceChildren();
    for (const [name, values] of Object.entries(draws)) {
      const canvas = el("canvas", {
        class: "chart chart-small",
        role: "img",
        "aria-label": `Prior density for ${name}.`,
      });
      densityHolder.appendChild(el("div", { class: "density-facet" },
        el("div", { class: "facet-label" }, name), canvas));
      drawDensity(canvas, values);
    }

    const implied = simulateAnalysis(createRng(applied.seed), analysisKey, priors, applied);
    if (implied.kind === "curves") {
      const yLabel = analysisKey === "logistic" ? "Implied probability" : "Implied mean";
      const capQuantile = analysisKey === "logistic" ? undefined : 0.99;
      drawCurves(impliedCanvas, implied.x, implied.curves, { yLabel, capQuantile });
    } else {
      drawScatter(impliedCanvas, implied.xs, implied.ys, {
        alpha: analysisKey === "multilevel" ? 0.06 : 0.04,
      });
    }
  }

  runButton.addEventListener("click", run);
  updateFlatWarning();

  return { run };
}
