// Distribution catalog: specs, help text, and sampling.
// Mirrors R/dist.R in the Shiny app; parameter names and defaults match.

export const FLAT_SD = 1000;

export const DIST_SPECS = {
  flat: { label: "Flat (improper)", params: {} },
  normal: { label: "Normal", params: { mu: 0, sigma: 1 } },
  laplace: { label: "Laplace (lasso)", params: { mu: 0, scale: 1 } },
  student_t: { label: "Student t", params: { df: 3, mu: 0, sigma: 2.5 } },
  cauchy: { label: "Cauchy", params: { loc: 0, scale: 1 } },
  exponential: { label: "Exponential", params: { rate: 1 } },
  gamma: { label: "Gamma", params: { shape: 2, rate: 1 } },
  lognormal: { label: "Log-normal", params: { meanlog: 0, sdlog: 1 } },
  inv_gamma: { label: "Inverse-gamma", params: { shape: 2, rate: 1 } },
  horseshoe: { label: "Horseshoe", params: { tau: 1, lambda_scale: 1 } },
  lkj_corr: { label: "LKJ (eta)", params: { eta: 1 } },
  half_student_t: { label: "Half-Student t", params: { df: 3, sigma: 2.5 } },
  half_normal: { label: "Half-normal", params: { sigma: 1 } },
  half_cauchy: { label: "Half-Cauchy", params: { scale: 1 } },
  uniform: { label: "Uniform", params: { min: -1, max: 1 } },
};

export const DIST_HELP = {
  flat: {
    description:
      "An improper flat prior (no preference across all real values). Used as a weak baseline.",
    example: "Baseline when you truly have no prior preference (use with caution).",
    sources: [
      {
        label: "SAS/STAT: Improper priors",
        url: "https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/statug_introbayes_sect004.htm",
      },
    ],
    params: {},
  },
  normal: {
    description: "Bell-shaped distribution centered at mu. Sigma controls spread.",
    example: "Measurement error and average effects.",
    sources: [
      {
        label: "NIST e-Handbook: Normal distribution",
        url: "https://itl.nist.gov/div898/handbook/eda/section3/eda3661.htm",
      },
    ],
    params: {
      mu: "Center / mean of the distribution.",
      sigma: "Standard deviation (spread).",
    },
  },
  laplace: {
    description:
      "Sharper peak at zero with heavier tails than normal. Encourages shrinkage (lasso).",
    example: "Regression coefficients in Bayesian lasso / sparse models.",
    sources: [
      {
        label: "Laplace distribution (overview)",
        url: "https://en.wikipedia.org/wiki/Laplace_distribution",
      },
      {
        label: "Bayesian lasso (Laplace prior)",
        url: "https://link.springer.com/article/10.1007/s42081-023-00213-2",
      },
    ],
    params: {
      mu: "Center of the distribution.",
      scale: "Scale (larger = wider; smaller = stronger shrinkage).",
    },
  },
  student_t: {
    description: "Like normal but with heavier tails for robustness.",
    example: "Robust regression where occasional outliers exist.",
    sources: [
      {
        label: "Student's t distribution",
        url: "https://mathworld.wolfram.com/Studentst-Distribution.html",
      },
    ],
    params: {
      df: "Degrees of freedom (smaller = heavier tails).",
      mu: "Center / mean.",
      sigma: "Scale (similar to SD).",
    },
  },
  cauchy: {
    description: "Very heavy tails. Allows large effects but can be unstable if too wide.",
    example: "Weakly informative priors on coefficients or scales.",
    sources: [
      {
        label: "Cauchy distribution",
        url: "https://en.wikipedia.org/wiki/Cauchy_distribution",
      },
    ],
    params: {
      loc: "Center of the distribution.",
      scale: "Scale (controls tail heaviness).",
    },
  },
  exponential: {
    description: "Positive-only distribution for rates or scales.",
    example: "Time between events; simple priors on scale parameters.",
    sources: [
      {
        label: "NIST e-Handbook: Exponential distribution",
        url: "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3667.htm",
      },
    ],
    params: {
      rate: "Rate (larger = more mass near 0).",
    },
  },
  gamma: {
    description: "Positive-only distribution for scales or shapes.",
    example: "Waiting times, positive rates, or dispersion parameters.",
    sources: [
      {
        label: "NIST e-Handbook: Gamma distribution",
        url: "https://www.itl.nist.gov/div898/handbook/eda/section3/eda366b.htm",
      },
    ],
    params: {
      shape: "Shape (controls skew and peak).",
      rate: "Rate (inverse scale).",
    },
  },
  lognormal: {
    description: "Positive-only with multiplicative variability; log of values is normal.",
    example: "Incomes, reaction times, or multiplicative growth.",
    sources: [
      {
        label: "NIST e-Handbook: Lognormal distribution",
        url: "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm",
      },
    ],
    params: {
      meanlog: "Mean on the log scale.",
      sdlog: "SD on the log scale.",
    },
  },
  inv_gamma: {
    description: "Positive-only; commonly used for variance/scale parameters.",
    example: "Variance or dispersion parameters in older Bayesian models.",
    sources: [
      {
        label: "Inverse-gamma distribution",
        url: "https://en.wikipedia.org/wiki/Inverse-gamma_distribution",
      },
    ],
    params: {
      shape: "Shape parameter.",
      rate: "Rate (inverse scale).",
    },
  },
  horseshoe: {
    description:
      "Strong shrinkage for most coefficients while allowing a few large effects.",
    example: "Sparse signals with a few strong predictors.",
    sources: [
      {
        label: "Handling Sparsity via the Horseshoe",
        url: "https://proceedings.mlr.press/v5/carvalho09a",
      },
    ],
    params: {
      tau: "Global shrinkage (smaller = stronger overall shrinkage).",
      lambda_scale: "Scale for local shrinkage (controls how easily large effects escape).",
    },
  },
  lkj_corr: {
    description:
      "Prior for correlation. eta = 1 is uniform; larger values favor correlations near 0.",
    example: "Correlations among group-level effects in multilevel models.",
    sources: [
      {
        label: "Stan User's Guide: LKJ prior",
        url: "https://mc-stan.org/docs/2_23/stan-users-guide/multivariate-hierarchical-priors-section.html",
      },
    ],
    params: {
      eta: "Shape parameter (higher = stronger pull toward zero correlation).",
    },
  },
  half_student_t: {
    description: "Positive-only Student t; used for scale parameters with heavy tails.",
    example: "Standard deviations with heavy tails.",
    sources: [
      {
        label: "Student's t distribution",
        url: "https://mathworld.wolfram.com/Studentst-Distribution.html",
      },
    ],
    params: {
      df: "Degrees of freedom.",
      sigma: "Scale.",
    },
  },
  half_normal: {
    description: "Positive-only normal; useful for standard deviations.",
    example: "Standard deviations and other positive scales.",
    sources: [
      {
        label: "Half-normal distribution",
        url: "https://en.wikipedia.org/wiki/Half-normal_distribution",
      },
    ],
    params: {
      sigma: "Scale (SD of the underlying normal).",
    },
  },
  half_cauchy: {
    description: "Positive-only Cauchy; very heavy tails for scale parameters.",
    example: "Weakly informative prior for scale parameters in hierarchical models.",
    sources: [
      {
        label: "Stan User's Guide: half-Cauchy recommendation",
        url: "https://mc-stan.org/docs/2_23/stan-users-guide/multivariate-hierarchical-priors-section.html",
      },
    ],
    params: {
      scale: "Scale (controls tail heaviness).",
    },
  },
  uniform: {
    description: "All values between min and max are equally likely.",
    example: "Bounded parameters like correlations or probabilities (with care).",
    sources: [
      {
        label: "Britannica: Uniform distribution",
        url: "https://www.britannica.com/topic/uniform-distribution-statistics",
      },
    ],
    params: {
      min: "Lower bound.",
      max: "Upper bound.",
    },
  },
};

export function getDistHelp(dist) {
  return DIST_HELP[dist] || { description: "No description available.", params: {} };
}

export function resolveParams(dist, params) {
  const spec = DIST_SPECS[dist];
  if (!spec) {
    throw new Error(`Unknown distribution: ${dist}`);
  }
  const merged = { ...spec.params };
  for (const name of Object.keys(spec.params)) {
    if (params && params[name] !== undefined && params[name] !== null) {
      merged[name] = params[name];
    }
  }
  return merged;
}

export function clipRho(rho) {
  return Math.max(Math.min(rho, 0.99), -0.99);
}

// Draw n samples from a supported distribution.
export function drawDist(rng, dist, params, n) {
  const p = resolveParams(dist, params);
  const out = new Float64Array(n);
  switch (dist) {
    case "normal":
      for (let i = 0; i < n; i++) out[i] = p.mu + p.sigma * rng.normal();
      return out;
    case "flat":
      for (let i = 0; i < n; i++) out[i] = FLAT_SD * rng.normal();
      return out;
    case "laplace":
      for (let i = 0; i < n; i++) {
        const u = rng.uniform() - 0.5;
        out[i] = p.mu - p.scale * Math.sign(u) * Math.log(1 - 2 * Math.abs(u));
      }
      return out;
    case "student_t":
      for (let i = 0; i < n; i++) out[i] = p.mu + p.sigma * rng.studentT(p.df);
      return out;
    case "cauchy":
      for (let i = 0; i < n; i++) out[i] = rng.cauchy(p.loc, p.scale);
      return out;
    case "exponential":
      for (let i = 0; i < n; i++) out[i] = rng.exponential(p.rate);
      return out;
    case "gamma":
      for (let i = 0; i < n; i++) out[i] = rng.gamma(p.shape, p.rate);
      return out;
    case "lognormal":
      for (let i = 0; i < n; i++) out[i] = Math.exp(p.meanlog + p.sdlog * rng.normal());
      return out;
    case "inv_gamma":
      for (let i = 0; i < n; i++) out[i] = 1 / rng.gamma(p.shape, p.rate);
      return out;
    case "horseshoe":
      for (let i = 0; i < n; i++) {
        const lambda = Math.abs(rng.cauchy(0, p.lambda_scale));
        out[i] = p.tau * lambda * rng.normal();
      }
      return out;
    case "lkj_corr":
      // For a 2x2 correlation matrix, the LKJ(eta) marginal of rho satisfies
      // (rho + 1) / 2 ~ Beta(eta, eta).
      for (let i = 0; i < n; i++) out[i] = 2 * rng.beta(p.eta, p.eta) - 1;
      return out;
    case "half_student_t":
      for (let i = 0; i < n; i++) out[i] = Math.abs(p.sigma * rng.studentT(p.df));
      return out;
    case "half_normal":
      for (let i = 0; i < n; i++) out[i] = Math.abs(p.sigma * rng.normal());
      return out;
    case "half_cauchy":
      for (let i = 0; i < n; i++) out[i] = Math.abs(rng.cauchy(0, p.scale));
      return out;
    case "uniform":
      for (let i = 0; i < n; i++) out[i] = p.min + (p.max - p.min) * rng.uniform();
      return out;
    default:
      throw new Error(`Unsupported distribution: ${dist}`);
  }
}
