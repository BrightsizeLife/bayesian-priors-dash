// Prior templates and presets by analysis type. Mirrors R/priors_registry.R.
//
// Difference from the Shiny app: the "rstanarm (placeholder)" preset is not
// offered. rstanarm's defaults are autoscaled to the data, which this app
// does not have; showing an unwired preset was judged worse than omitting it.

export const MODEL_PURPOSE = {
  linear:
    "Continuous outcomes with roughly constant variance. Good for straight-line relationships.",
  logistic: "Binary outcomes (yes/no). Models probabilities and odds.",
  poisson: "Counts of events per unit time/space. Assumes mean ≈ variance.",
  gamma: "Positive, skewed continuous outcomes (e.g., durations, costs).",
  negbin: "Counts with overdispersion (variance > mean).",
  multilevel: "Grouped data with shared structure across groups (hierarchical).",
};

export const PRIOR_TEMPLATES = {
  linear: {
    title: "Linear regression",
    parameters: {
      intercept: {
        label: "Intercept",
        allowed: ["student_t", "normal", "cauchy", "flat"],
        default: { dist: "student_t", params: { df: 3, mu: 0, sigma: 10 } },
      },
      beta: {
        label: "Slope(s)",
        allowed: ["normal", "student_t", "cauchy", "laplace", "horseshoe", "flat"],
        default: { dist: "normal", params: { mu: 0, sigma: 1 } },
      },
      sigma: {
        label: "Residual SD",
        allowed: ["exponential", "half_normal", "half_cauchy", "half_student_t"],
        default: { dist: "exponential", params: { rate: 1 } },
      },
    },
    settings: { n_sims: 1000, n_points: 50, x_min: -2, x_max: 2 },
  },
  logistic: {
    title: "Logistic regression",
    parameters: {
      intercept: {
        label: "Intercept",
        allowed: ["student_t", "normal", "cauchy", "flat"],
        default: { dist: "student_t", params: { df: 3, mu: 0, sigma: 5 } },
      },
      beta: {
        label: "Slope(s)",
        allowed: ["normal", "student_t", "cauchy", "laplace", "horseshoe", "flat"],
        default: { dist: "normal", params: { mu: 0, sigma: 2.5 } },
      },
    },
    settings: { n_sims: 1000, n_points: 50, x_min: -3, x_max: 3 },
  },
  poisson: {
    title: "Poisson regression",
    parameters: {
      intercept: {
        label: "Intercept",
        allowed: ["student_t", "normal", "cauchy", "flat"],
        default: { dist: "student_t", params: { df: 3, mu: 0, sigma: 5 } },
      },
      beta: {
        label: "Slope(s)",
        allowed: ["normal", "student_t", "cauchy", "laplace", "horseshoe", "flat"],
        default: { dist: "normal", params: { mu: 0, sigma: 1 } },
      },
    },
    settings: { n_sims: 1000, n_points: 50, x_min: -2, x_max: 2 },
  },
  gamma: {
    title: "Gamma regression",
    parameters: {
      intercept: {
        label: "Intercept",
        allowed: ["student_t", "normal", "cauchy", "flat"],
        default: { dist: "student_t", params: { df: 3, mu: 0, sigma: 5 } },
      },
      beta: {
        label: "Slope(s)",
        allowed: ["normal", "student_t", "cauchy", "laplace", "horseshoe", "flat"],
        default: { dist: "normal", params: { mu: 0, sigma: 1 } },
      },
      shape: {
        label: "Shape",
        allowed: ["gamma", "exponential", "lognormal"],
        default: { dist: "gamma", params: { shape: 2, rate: 0.5 } },
      },
    },
    settings: { n_sims: 1000, n_points: 50, x_min: -2, x_max: 2 },
  },
  negbin: {
    title: "Negative binomial regression",
    parameters: {
      intercept: {
        label: "Intercept",
        allowed: ["student_t", "normal", "cauchy", "flat"],
        default: { dist: "student_t", params: { df: 3, mu: 0, sigma: 5 } },
      },
      beta: {
        label: "Slope(s)",
        allowed: ["normal", "student_t", "cauchy", "laplace", "horseshoe", "flat"],
        default: { dist: "normal", params: { mu: 0, sigma: 1 } },
      },
      shape: {
        label: "Dispersion (shape)",
        allowed: ["gamma", "exponential", "lognormal", "inv_gamma"],
        default: { dist: "gamma", params: { shape: 2, rate: 0.5 } },
      },
    },
    settings: { n_sims: 1000, n_points: 50, x_min: -2, x_max: 2 },
  },
  multilevel: {
    title: "Multilevel regression",
    parameters: {
      intercept: {
        label: "Intercept",
        allowed: ["student_t", "normal", "cauchy", "flat"],
        default: { dist: "student_t", params: { df: 3, mu: 0, sigma: 10 } },
      },
      beta: {
        label: "Slope(s)",
        allowed: ["normal", "student_t", "cauchy", "laplace", "horseshoe", "flat"],
        default: { dist: "normal", params: { mu: 0, sigma: 1 } },
      },
      sigma: {
        label: "Residual SD",
        allowed: ["exponential", "half_normal", "half_cauchy", "half_student_t"],
        default: { dist: "exponential", params: { rate: 1 } },
      },
      tau_intercept: {
        label: "Group SD (intercept)",
        allowed: ["exponential", "half_normal", "half_cauchy", "half_student_t"],
        default: { dist: "exponential", params: { rate: 1 } },
      },
      tau_slope: {
        label: "Group SD (slope)",
        allowed: ["exponential", "half_normal", "half_cauchy", "half_student_t"],
        default: { dist: "exponential", params: { rate: 1 } },
      },
      rho: {
        label: "Group corr (intercept/slope)",
        allowed: ["uniform", "normal", "lkj_corr"],
        default: { dist: "uniform", params: { min: -0.5, max: 0.5 } },
      },
    },
    settings: { n_sims: 1000, n_points: 30, x_min: -2, x_max: 2, n_groups: 8 },
  },
};

// Bounds applied server-side in spirit: values typed into the UI are clamped
// to these before simulating, so a pasted 1e7 cannot freeze the page.
export const SETTING_BOUNDS = {
  n_sims: { min: 100, max: 5000 },
  n_points: { min: 20, max: 200 },
  n_groups: { min: 2, max: 50 },
  n_draws: { min: 500, max: 50000 },
  hdi_mass: { min: 0.5, max: 0.99 },
};

function buildCustomDefaults(template) {
  const out = {};
  for (const [name, param] of Object.entries(template.parameters)) {
    out[name] = {
      dist: param.default.dist,
      params: { ...param.default.params },
    };
  }
  return out;
}

function buildBrmsDefaults(analysisKey, template) {
  const defaults = buildCustomDefaults(template);
  if (defaults.intercept) {
    defaults.intercept = { dist: "student_t", params: { df: 3, mu: 0, sigma: 2.5 } };
  }
  if (defaults.beta) {
    defaults.beta = { dist: "flat", params: {} };
  }
  if (defaults.sigma) {
    defaults.sigma = { dist: "half_student_t", params: { df: 3, sigma: 2.5 } };
  }
  if (defaults.tau_intercept) {
    defaults.tau_intercept = { dist: "half_student_t", params: { df: 3, sigma: 2.5 } };
  }
  if (defaults.tau_slope) {
    defaults.tau_slope = { dist: "half_student_t", params: { df: 3, sigma: 2.5 } };
  }
  if (defaults.rho) {
    defaults.rho = { dist: "uniform", params: { min: -1, max: 1 } };
  }
  if (defaults.shape && analysisKey === "gamma") {
    defaults.shape = { dist: "gamma", params: { shape: 0.01, rate: 0.01 } };
  }
  if (defaults.shape && analysisKey === "negbin") {
    defaults.shape = { dist: "inv_gamma", params: { shape: 0.4, rate: 0.3 } };
  }
  return defaults;
}

export const PRIOR_PRESETS = Object.fromEntries(
  Object.entries(PRIOR_TEMPLATES).map(([key, template]) => [
    key,
    {
      custom: buildCustomDefaults(template),
      brms: buildBrmsDefaults(key, template),
    },
  ])
);

export const GLM_ANALYSES = ["logistic", "poisson", "gamma", "negbin"];
