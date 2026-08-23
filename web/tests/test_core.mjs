// Tests for the web app's logic layer (no DOM needed).
// Run from the repo root or web/ with: node web/tests/test_core.mjs
// Exits non-zero on failure.

import { createRng } from "../js/rng.js";
import { DIST_SPECS, DIST_HELP, drawDist, resolveParams, clipRho } from "../js/dist.js";
import { PRIOR_TEMPLATES, PRIOR_PRESETS, SETTING_BOUNDS } from "../js/registry.js";
import {
  mean,
  sd,
  mad,
  quantile,
  hdiInterval,
  summarizeDraws,
  formatNumber,
} from "../js/summaries.js";
import { simulateAnalysis, capEta, samplePriorDraws } from "../js/simulate.js";
import {
  priorStrengthWeights,
  priorStrengthThresholds,
  normalMeanMetrics,
} from "../js/priorStrength.js";

let failures = 0;
function check(desc, fn) {
  let ok = false;
  try {
    ok = fn() === true;
  } catch (err) {
    console.log(`  error: ${err.message}`);
  }
  console.log(`[${ok ? "PASS" : "FAIL"}] ${desc}`);
  if (!ok) failures += 1;
}
const throws = (fn) => {
  try {
    fn();
    return false;
  } catch {
    return true;
  }
};
const near = (a, b, tol) => Math.abs(a - b) <= tol;

const rng = createRng(42);

// --- samplers: shape, support, and moments against closed forms ------------

const POSITIVE_ONLY = new Set([
  "exponential",
  "gamma",
  "lognormal",
  "inv_gamma",
  "half_student_t",
  "half_normal",
  "half_cauchy",
]);

for (const dist of Object.keys(DIST_SPECS)) {
  const x = drawDist(rng, dist, DIST_SPECS[dist].params, 2000);
  check(`drawDist('${dist}') returns 2000 finite-or-heavy-tail numeric draws`, () => {
    return x.length === 2000 && Array.from(x).every((v) => !Number.isNaN(v));
  });
  if (POSITIVE_ONLY.has(dist)) {
    check(`drawDist('${dist}') draws are positive`, () => Array.from(x).every((v) => v >= 0));
  }
}

const N = 200000;
check("normal(0,1) sample moments match", () => {
  const x = drawDist(rng, "normal", { mu: 0, sigma: 1 }, N);
  return near(mean(x), 0, 0.01) && near(sd(x), 1, 0.01);
});
check("normal(2,3) sample moments match", () => {
  const x = drawDist(rng, "normal", { mu: 2, sigma: 3 }, N);
  return near(mean(x), 2, 0.03) && near(sd(x), 3, 0.03);
});
check("exponential(2) mean ≈ 0.5", () => {
  const x = drawDist(rng, "exponential", { rate: 2 }, N);
  return near(mean(x), 0.5, 0.01);
});
check("gamma(2,1) mean ≈ 2, sd ≈ sqrt(2)", () => {
  const x = drawDist(rng, "gamma", { shape: 2, rate: 1 }, N);
  return near(mean(x), 2, 0.02) && near(sd(x), Math.sqrt(2), 0.02);
});
check("gamma(0.5,1) (shape<1 branch) mean ≈ 0.5", () => {
  const x = drawDist(rng, "gamma", { shape: 0.5, rate: 1 }, N);
  return near(mean(x), 0.5, 0.01);
});
check("lognormal(0,1) mean ≈ exp(0.5)", () => {
  const x = drawDist(rng, "lognormal", { meanlog: 0, sdlog: 1 }, N);
  return near(mean(x), Math.exp(0.5), 0.05);
});
check("laplace(0,1) mean ≈ 0, sd ≈ sqrt(2)", () => {
  const x = drawDist(rng, "laplace", { mu: 0, scale: 1 }, N);
  return near(mean(x), 0, 0.01) && near(sd(x), Math.sqrt(2), 0.02);
});
check("student_t(df=5) median ≈ 0, sd ≈ sqrt(5/3)", () => {
  const x = drawDist(rng, "student_t", { df: 5, mu: 0, sigma: 1 }, N);
  return near(quantile(x, 0.5), 0, 0.01) && near(sd(x), Math.sqrt(5 / 3), 0.05);
});
check("cauchy(0,1) quartiles ≈ ±1", () => {
  const x = drawDist(rng, "cauchy", { loc: 0, scale: 1 }, N);
  return near(quantile(x, 0.25), -1, 0.03) && near(quantile(x, 0.75), 1, 0.03);
});
check("uniform draws respect bounds and mean", () => {
  const x = drawDist(rng, "uniform", { min: -1, max: 1 }, N);
  return (
    Array.from(x).every((v) => v >= -1 && v <= 1) && near(mean(x), 0, 0.01)
  );
});
check("lkj_corr(eta=1) is uniform on [-1,1]", () => {
  const x = drawDist(rng, "lkj_corr", { eta: 1 }, N);
  return (
    Array.from(x).every((v) => v >= -1 && v <= 1) &&
    near(mean(x), 0, 0.01) &&
    near(sd(x), Math.sqrt(1 / 3), 0.01)
  );
});
check("half_normal(1) mean ≈ sqrt(2/pi)", () => {
  const x = drawDist(rng, "half_normal", { sigma: 1 }, N);
  return near(mean(x), Math.sqrt(2 / Math.PI), 0.01);
});
check("seeded rng reproduces itself", () => {
  const a = drawDist(createRng(7), "normal", { mu: 0, sigma: 1 }, 10);
  const b = drawDist(createRng(7), "normal", { mu: 0, sigma: 1 }, 10);
  return a.every((v, i) => v === b[i]);
});
check("drawDist throws on unknown distribution", () => throws(() => drawDist(rng, "nope", {}, 10)));
check("resolveParams merges overrides onto defaults", () => {
  const p = resolveParams("normal", { sigma: 5 });
  return p.mu === 0 && p.sigma === 5;
});
check("clipRho clips to [-0.99, 0.99]", () =>
  clipRho(1) === 0.99 && clipRho(-1) === -0.99 && clipRho(0.5) === 0.5);

// --- summaries: exact checks against R reference values --------------------
// R: quantile(1:10, c(.1,.25,.5,.75,.9)) -> 1.9 3.25 5.5 7.75 9.1  (type 7)
// R: mad(c(1,2,3,4,100)) -> 1.4826 ; sd(1:10) -> 3.02765...

const oneToTen = Float64Array.from({ length: 10 }, (_, i) => i + 1);
check("quantile matches R type-7 exactly", () => {
  return (
    near(quantile(oneToTen, 0.1), 1.9, 1e-12) &&
    near(quantile(oneToTen, 0.25), 3.25, 1e-12) &&
    near(quantile(oneToTen, 0.5), 5.5, 1e-12) &&
    near(quantile(oneToTen, 0.75), 7.75, 1e-12) &&
    near(quantile(oneToTen, 0.9), 9.1, 1e-12)
  );
});
check("sd matches R exactly", () => near(sd(oneToTen), 3.0276503540974917, 1e-12));
check("mad matches R exactly", () =>
  near(mad(Float64Array.from([1, 2, 3, 4, 100])), 1.4826, 1e-12));
check("hdi covers requested mass on uniform draws", () => {
  const x = drawDist(rng, "uniform", { min: 0, max: 1 }, 10000);
  const hdi = hdiInterval(x, 0.9);
  return hdi.lower < hdi.upper && near(hdi.upper - hdi.lower, 0.9, 0.05);
});
check("hdi finds the dense region of skewed draws", () => {
  const x = drawDist(rng, "exponential", { rate: 1 }, 10000);
  const hdi = hdiInterval(x, 0.5);
  return hdi.lower < 0.2;
});
check("hdi on empty input returns NaNs", () => {
  const hdi = hdiInterval([]);
  return Number.isNaN(hdi.lower) && Number.isNaN(hdi.upper);
});
check("summarizeDraws returns ordered labelled rows", () => {
  const rows = summarizeDraws({
    a: drawDist(rng, "normal", { mu: 0, sigma: 1 }, 1000),
    b: drawDist(rng, "exponential", { rate: 1 }, 1000),
  });
  const r = rows[0];
  return (
    rows.length === 2 &&
    rows[0].parameter === "a" &&
    rows[1].parameter === "b" &&
    r.p10 <= r.p25 &&
    r.p25 <= r.p50 &&
    r.p50 <= r.p75 &&
    r.p75 <= r.p90 &&
    r.hdi_lower <= r.hdi_upper
  );
});
check("formatNumber matches Shiny formatting rules", () => {
  return (
    formatNumber(0) === "0" &&
    formatNumber(1.23456) === "1.235" &&
    formatNumber(1e7) === "1.00e+7" &&
    formatNumber(0.00001) === "1.00e-5" &&
    formatNumber(NaN) === ""
  );
});

// --- simulation -------------------------------------------------------------

const basePriors = {
  intercept: { dist: "normal", params: { mu: 0, sigma: 1 } },
  beta: { dist: "normal", params: { mu: 0, sigma: 1 } },
  sigma: { dist: "exponential", params: { rate: 1 } },
};
const settings = { n_sims: 20, n_points: 10, x_min: -2, x_max: 2, n_groups: 4 };

check("capEta bounds the linear predictor", () =>
  capEta(-100) === -6 && capEta(100) === 6 && capEta(0.5) === 0.5);

check("simulateAnalysis('linear') returns n_sims*n_points points", () => {
  const d = simulateAnalysis(rng, "linear", basePriors, settings);
  return d.kind === "points" && d.xs.length === 200 && d.ys.length === 200;
});
for (const key of ["logistic", "poisson", "gamma", "negbin"]) {
  check(`simulateAnalysis('${key}') returns n_sims curves of n_points`, () => {
    const d = simulateAnalysis(
      rng,
      key,
      { intercept: basePriors.intercept, beta: basePriors.beta },
      settings
    );
    return d.kind === "curves" && d.curves.length === 20 && d.curves[0].length === 10;
  });
}
check("logistic curves stay in [0,1]", () => {
  const d = simulateAnalysis(
    rng,
    "logistic",
    { intercept: basePriors.intercept, beta: basePriors.beta },
    settings
  );
  return d.curves.every((c) => Array.from(c).every((v) => v >= 0 && v <= 1));
});
check("poisson/gamma curves are positive and eta-capped", () => {
  const d = simulateAnalysis(
    rng,
    "poisson",
    { intercept: basePriors.intercept, beta: basePriors.beta },
    settings
  );
  const cap = Math.exp(6);
  return d.curves.every((c) => Array.from(c).every((v) => v > 0 && v <= cap));
});
check("simulateAnalysis('multilevel') returns sims*groups*points points", () => {
  const priors = {
    ...basePriors,
    tau_intercept: { dist: "exponential", params: { rate: 1 } },
    tau_slope: { dist: "exponential", params: { rate: 1 } },
    rho: { dist: "uniform", params: { min: -0.5, max: 0.5 } },
  };
  const d = simulateAnalysis(rng, "multilevel", priors, settings);
  return d.kind === "points" && d.xs.length === 20 * 4 * 10;
});
check("simulateAnalysis throws on unknown analysis", () =>
  throws(() => simulateAnalysis(rng, "nope", basePriors, settings)));
check("samplePriorDraws draws every parameter", () => {
  const draws = samplePriorDraws(rng, basePriors, 100);
  return Object.keys(draws).length === 3 && draws.sigma.length === 100;
});

// --- registry integrity ------------------------------------------------------

check("every template default distribution is allowed and specified", () => {
  return Object.values(PRIOR_TEMPLATES).every((template) =>
    Object.values(template.parameters).every(
      (param) =>
        param.allowed.includes(param.default.dist) && DIST_SPECS[param.default.dist]
    )
  );
});
check("every preset references known distributions", () => {
  return Object.values(PRIOR_PRESETS).every((presets) =>
    Object.values(presets).every((preset) =>
      Object.values(preset).every((spec) => DIST_SPECS[spec.dist])
    )
  );
});
check("every allowed distribution has help text", () => {
  return Object.values(PRIOR_TEMPLATES).every((template) =>
    Object.values(template.parameters).every((param) =>
      param.allowed.every((dist) => DIST_HELP[dist])
    )
  );
});
check("setting bounds are sane", () => {
  return Object.values(SETTING_BOUNDS).every((b) => b.min < b.max);
});

// --- prior strength: exact values matching the R reference ------------------

check("normal_mean weights match precision arithmetic", () => {
  const w = priorStrengthWeights("normal_mean", { mu0: 0, sigma0: 1 }, { sigma: 2 }, 30);
  return (
    near(w.priorWeight, 1, 1e-12) &&
    near(w.likelihoodWeight, 7.5, 1e-12) &&
    near(w.priorShare, 1 / 8.5, 1e-12) &&
    near(w.priorShare + w.likelihoodShare, 1, 1e-12)
  );
});
check("normal_mean thresholds match sigma^2/sigma0^2", () => {
  const t = priorStrengthThresholds("normal_mean", { mu0: 0, sigma0: 1 }, { sigma: 2 });
  return t.nEqual === 4 && t.nMinLikelihoodDominates === 5 && t.nMaxPriorDominates === 3;
});
check("bernoulli prior weight equals alpha+beta", () => {
  const w = priorStrengthWeights("bernoulli", { alpha: 2, beta: 2 }, {}, 5);
  return near(w.priorWeight, 4, 1e-12) && near(w.priorShare, 4 / 9, 1e-12);
});
check("poisson prior weight equals beta", () => {
  const w = priorStrengthWeights("poisson", { alpha: 2, beta: 1 }, {}, 5);
  return near(w.priorWeight, 1, 1e-12) && near(w.priorShare, 1 / 6, 1e-12);
});
check("normalMeanMetrics matches hand-computed values at n=30", () => {
  const m = normalMeanMetrics(0, 1, 0.8, 2, 30);
  // wPrior=1, wLike=7.5 -> posterior mean = 7.5*0.8/8.5 ; s2=4/30
  return (
    near(m.priorShare, 1 / 8.5, 1e-12) &&
    near(m.posteriorMean, 6 / 8.5, 1e-12) &&
    near(m.kl, 0.5 * (Math.log(4 / 30) + (1 + 0.64) / (4 / 30) - 1), 1e-12)
  );
});
check("validation rejects non-positive prior scale", () =>
  throws(() => priorStrengthWeights("normal_mean", { mu0: 0, sigma0: -1 }, { sigma: 2 }, 30)));
check("validation rejects non-positive likelihood sigma", () =>
  throws(() => priorStrengthWeights("normal_mean", { mu0: 0, sigma0: 1 }, { sigma: 0 }, 30)));
check("validation rejects non-positive n", () =>
  throws(() => priorStrengthWeights("normal_mean", { mu0: 0, sigma0: 1 }, { sigma: 2 }, 0)));
check("validation rejects missing prior parameters", () =>
  throws(() => priorStrengthWeights("bernoulli", { alpha: 2 }, {}, 5)));

// -----------------------------------------------------------------------------

console.log(failures === 0 ? "\nAll tests passed." : `\n${failures} test(s) FAILED.`);
if (failures > 0) process.exit(1);
