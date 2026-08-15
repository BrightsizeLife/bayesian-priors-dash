// Prior-predictive simulation for each model family. Mirrors R/simulation.R.

import { drawDist, clipRho } from "./dist.js";

export const ETA_LIMIT = 6;

export function capEta(eta) {
  return Math.max(Math.min(eta, ETA_LIMIT), -ETA_LIMIT);
}

export function samplePriorDraws(rng, priorState, n) {
  const draws = {};
  for (const [name, spec] of Object.entries(priorState)) {
    draws[name] = drawDist(rng, spec.dist, spec.params, n);
  }
  return draws;
}

function xGrid(settings) {
  const { n_points: nPoints, x_min: xMin, x_max: xMax } = settings;
  const x = new Float64Array(nPoints);
  for (let i = 0; i < nPoints; i++) {
    x[i] = xMin + ((xMax - xMin) * i) / (nPoints - 1);
  }
  return x;
}

// Linear: per-sim scatter of y = a + b x + noise. Returns flat point arrays.
function simulateLinear(rng, priorState, settings) {
  const { n_sims: nSims, n_points: nPoints } = settings;
  const x = xGrid(settings);
  const draws = samplePriorDraws(rng, priorState, nSims);
  const xs = new Float64Array(nSims * nPoints);
  const ys = new Float64Array(nSims * nPoints);
  for (let s = 0; s < nSims; s++) {
    for (let i = 0; i < nPoints; i++) {
      const idx = s * nPoints + i;
      const mu = draws.intercept[s] + draws.beta[s] * x[i];
      xs[idx] = x[i];
      ys[idx] = mu + draws.sigma[s] * rng.normal();
    }
  }
  return { kind: "points", xs, ys };
}

// GLM families: one mean curve per simulation on the response scale.
function simulateGlmCurves(rng, priorState, settings, linkInverse) {
  const { n_sims: nSims, n_points: nPoints } = settings;
  const x = xGrid(settings);
  const draws = samplePriorDraws(rng, priorState, nSims);
  const curves = [];
  for (let s = 0; s < nSims; s++) {
    const y = new Float64Array(nPoints);
    for (let i = 0; i < nPoints; i++) {
      y[i] = linkInverse(capEta(draws.intercept[s] + draws.beta[s] * x[i]));
    }
    curves.push(y);
  }
  return { kind: "curves", x, curves };
}

const plogis = (eta) => 1 / (1 + Math.exp(-eta));

function simulateMultilevel(rng, priorState, settings) {
  const { n_sims: nSims, n_points: nPoints, n_groups: nGroups } = settings;
  const x = xGrid(settings);
  const draws = samplePriorDraws(rng, priorState, nSims);
  const total = nSims * nGroups * nPoints;
  const xs = new Float64Array(total);
  const ys = new Float64Array(total);
  let idx = 0;
  for (let s = 0; s < nSims; s++) {
    const r = clipRho(draws.rho[s]);
    const tauI = draws.tau_intercept[s];
    const tauS = draws.tau_slope[s];
    for (let g = 0; g < nGroups; g++) {
      // Bivariate normal group effects via the Cholesky factor of
      // [[tauI^2, r tauI tauS], [r tauI tauS, tauS^2]].
      const z1 = rng.normal();
      const z2 = rng.normal();
      const b0 = tauI * z1;
      const b1 = tauS * (r * z1 + Math.sqrt(1 - r * r) * z2);
      for (let i = 0; i < nPoints; i++) {
        const mu = draws.intercept[s] + b0 + (draws.beta[s] + b1) * x[i];
        xs[idx] = x[i];
        ys[idx] = mu + draws.sigma[s] * rng.normal();
        idx++;
      }
    }
  }
  return { kind: "points", xs, ys };
}

export function simulateAnalysis(rng, analysisKey, priorState, settings) {
  switch (analysisKey) {
    case "linear":
      return simulateLinear(rng, priorState, settings);
    case "logistic":
      return simulateGlmCurves(rng, priorState, settings, plogis);
    case "poisson":
    case "gamma":
    case "negbin":
      return simulateGlmCurves(rng, priorState, settings, Math.exp);
    case "multilevel":
      return simulateMultilevel(rng, priorState, settings);
    default:
      throw new Error(`Unknown analysis type: ${analysisKey}`);
  }
}
