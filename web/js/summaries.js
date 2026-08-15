// Summary statistics for prior draws. Mirrors R/summaries.R; quantiles use
// R's default type-7 definition and MAD uses R's 1.4826 consistency constant.

export function mean(x) {
  let total = 0;
  for (let i = 0; i < x.length; i++) total += x[i];
  return total / x.length;
}

export function sd(x) {
  const m = mean(x);
  let total = 0;
  for (let i = 0; i < x.length; i++) {
    const d = x[i] - m;
    total += d * d;
  }
  return Math.sqrt(total / (x.length - 1));
}

// R type-7 quantile on a pre-sorted array.
export function quantileSorted(sorted, p) {
  const n = sorted.length;
  if (n === 0) return NaN;
  const h = (n - 1) * p;
  const lo = Math.floor(h);
  const hi = Math.min(lo + 1, n - 1);
  return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
}

export function quantile(x, p) {
  const sorted = Float64Array.from(x).sort();
  return quantileSorted(sorted, p);
}

export function median(x) {
  return quantile(x, 0.5);
}

// Median absolute deviation scaled by 1.4826 (consistent with sd for normals).
export function mad(x) {
  const m = median(x);
  const deviations = Float64Array.from(x, (v) => Math.abs(v - m));
  return 1.4826 * median(deviations);
}

// Narrowest interval containing `mass` of the draws (same algorithm as the
// Shiny app's hdi_interval).
export function hdiInterval(x, mass = 0.9) {
  const sorted = Float64Array.from(x).sort();
  const n = sorted.length;
  if (n === 0) return { lower: NaN, upper: NaN };
  const m = Math.floor(mass * n);
  if (m < 1 || m >= n) return { lower: NaN, upper: NaN };
  let bestIdx = 0;
  let bestWidth = Infinity;
  for (let i = 0; i + m < n; i++) {
    const width = sorted[i + m] - sorted[i];
    if (width < bestWidth) {
      bestWidth = width;
      bestIdx = i;
    }
  }
  return { lower: sorted[bestIdx], upper: sorted[bestIdx + m] };
}

export function summarizeDraws(draws, hdiMass = 0.9) {
  return Object.entries(draws).map(([parameter, x]) => {
    const sorted = Float64Array.from(x).sort();
    const hdi = hdiInterval(sorted, hdiMass);
    return {
      parameter,
      mean: mean(x),
      sd: sd(x),
      mad: mad(x),
      hdi_lower: hdi.lower,
      hdi_upper: hdi.upper,
      p10: quantileSorted(sorted, 0.1),
      p25: quantileSorted(sorted, 0.25),
      p50: quantileSorted(sorted, 0.5),
      p75: quantileSorted(sorted, 0.75),
      p90: quantileSorted(sorted, 0.9),
    };
  });
}

// Number formatting matching the Shiny app's summary table.
export function formatNumber(x) {
  if (x === null || x === undefined || Number.isNaN(x)) return "";
  if (x === 0) return "0";
  if (!Number.isFinite(x)) return x > 0 ? "Inf" : "-Inf";
  if (Math.abs(x) >= 1e6 || Math.abs(x) < 1e-4) {
    return x.toExponential(2);
  }
  return x.toFixed(3);
}
