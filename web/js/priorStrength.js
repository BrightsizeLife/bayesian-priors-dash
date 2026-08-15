// Conjugate prior-vs-likelihood weight math. Mirrors R/prior_strength.R,
// including the fix where positive-parameter validation checks each parameter
// set only against the names it actually owns.

export const PRIOR_STRENGTH_SPECS = {
  normal_mean: {
    label: "Normal mean (known sigma)",
    priorParams: ["mu0", "sigma0"],
    likelihoodParams: ["sigma"],
    positiveParams: ["sigma0", "sigma"],
    notes: "Posterior mean uses precision weights; assumes known observation SD.",
    weightFn: (prior, likelihood, n) => ({
      prior: 1 / (prior.sigma0 * prior.sigma0),
      likelihood: n / (likelihood.sigma * likelihood.sigma),
    }),
    nEqualFn: (prior, likelihood) =>
      (likelihood.sigma * likelihood.sigma) / (prior.sigma0 * prior.sigma0),
  },
  bernoulli: {
    label: "Bernoulli/Binomial (Beta prior)",
    priorParams: ["alpha", "beta"],
    likelihoodParams: [],
    positiveParams: ["alpha", "beta"],
    notes: "Posterior mean is weighted by prior pseudo-counts (alpha+beta) vs. n.",
    weightFn: (prior, likelihood, n) => ({
      prior: prior.alpha + prior.beta,
      likelihood: n,
    }),
    nEqualFn: (prior) => prior.alpha + prior.beta,
  },
  poisson: {
    label: "Poisson (Gamma prior on rate)",
    priorParams: ["alpha", "beta"],
    likelihoodParams: [],
    positiveParams: ["alpha", "beta"],
    notes: "Posterior mean uses weights beta (prior) vs. n (likelihood).",
    weightFn: (prior, likelihood, n) => ({
      prior: prior.beta,
      likelihood: n,
    }),
    nEqualFn: (prior) => prior.beta,
  },
};

export function priorStrengthSpec(likelihood) {
  const spec = PRIOR_STRENGTH_SPECS[likelihood];
  if (!spec) {
    throw new Error(`Unknown likelihood: ${likelihood}`);
  }
  return spec;
}

function ensureNamedParams(params, required, label) {
  const p = params || {};
  const missing = required.filter((name) => p[name] === undefined);
  if (missing.length > 0) {
    throw new Error(`${label} parameters missing: ${missing.join(", ")}`);
  }
  return p;
}

function ensurePositiveParams(params, names, label) {
  for (const name of names) {
    const value = params[name];
    if (typeof value !== "number" || Number.isNaN(value)) {
      throw new Error(`${label} parameter '${name}' must be a single numeric value.`);
    }
    if (value <= 0) {
      throw new Error(`${label} parameter '${name}' must be > 0.`);
    }
  }
}

function ensurePositiveN(n) {
  if (typeof n !== "number" || Number.isNaN(n)) {
    throw new Error("n must be a single numeric value.");
  }
  if (n <= 0) {
    throw new Error("n must be > 0.");
  }
}

function validate(spec, priorParams, likelihoodParams) {
  const prior = ensureNamedParams(priorParams, spec.priorParams, "Prior");
  const likelihood = ensureNamedParams(likelihoodParams, spec.likelihoodParams, "Likelihood");
  ensurePositiveParams(
    prior,
    spec.positiveParams.filter((name) => spec.priorParams.includes(name)),
    "Prior"
  );
  ensurePositiveParams(
    likelihood,
    spec.positiveParams.filter((name) => spec.likelihoodParams.includes(name)),
    "Likelihood"
  );
  return { prior, likelihood };
}

export function priorStrengthWeights(likelihood, priorParams, likelihoodParams, n) {
  const spec = priorStrengthSpec(likelihood);
  const params = validate(spec, priorParams, likelihoodParams);
  ensurePositiveN(n);

  const weights = spec.weightFn(params.prior, params.likelihood, n);
  const total = weights.prior + weights.likelihood;
  return {
    priorWeight: weights.prior,
    likelihoodWeight: weights.likelihood,
    priorShare: weights.prior / total,
    likelihoodShare: weights.likelihood / total,
  };
}

export function priorStrengthThresholds(likelihood, priorParams, likelihoodParams, nIsInteger = true) {
  const spec = priorStrengthSpec(likelihood);
  const params = validate(spec, priorParams, likelihoodParams);

  const nEqual = spec.nEqualFn(params.prior, params.likelihood);
  if (nIsInteger) {
    return {
      nEqual,
      nMinLikelihoodDominates: Math.floor(nEqual) + 1,
      nMaxPriorDominates: Math.ceil(nEqual) - 1,
    };
  }
  return {
    nEqual,
    nMinLikelihoodDominates: nEqual,
    nMaxPriorDominates: nEqual,
  };
}

// Closed-form posterior metrics for the Normal-mean case, shared by the grid
// (plots) and the summary table so the two can never drift apart.
export function normalMeanMetrics(mu0, tau0, betaHat, sigma, n) {
  const wPrior = 1 / (tau0 * tau0);
  const wLike = n / (sigma * sigma);
  const s2 = (sigma * sigma) / n;
  return {
    priorShare: wPrior / (wPrior + wLike),
    posteriorMean: (wPrior * mu0 + wLike * betaHat) / (wPrior + wLike),
    kl: 0.5 * (Math.log(s2 / (tau0 * tau0)) + (tau0 * tau0 + (mu0 - betaHat) ** 2) / s2 - 1),
  };
}
