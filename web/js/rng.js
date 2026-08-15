// Seeded random number generation and distribution samplers.
//
// Uses mulberry32 for the uniform stream so results are reproducible for a
// given seed. Note: this is NOT the same generator as R (Mersenne Twister),
// so the same seed gives different draws than the Shiny app — distributions
// match, individual draws do not.

export function createRng(seed) {
  let state = (seed >>> 0) || 1;
  let spareNormal = null;

  // mulberry32; returns values in the open interval (0, 1) so log(u) is safe.
  function uniform() {
    state = (state + 0x6d2b79f5) | 0;
    let t = Math.imul(state ^ (state >>> 15), 1 | state);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return (((t ^ (t >>> 14)) >>> 0) + 0.5) / 4294967296;
  }

  // Box-Muller with spare caching.
  function normal() {
    if (spareNormal !== null) {
      const value = spareNormal;
      spareNormal = null;
      return value;
    }
    const u1 = uniform();
    const u2 = uniform();
    const radius = Math.sqrt(-2 * Math.log(u1));
    spareNormal = radius * Math.sin(2 * Math.PI * u2);
    return radius * Math.cos(2 * Math.PI * u2);
  }

  // Marsaglia-Tsang; shape > 0, scale = 1.
  function gammaUnit(shape) {
    if (shape < 1) {
      return gammaUnit(shape + 1) * Math.pow(uniform(), 1 / shape);
    }
    const d = shape - 1 / 3;
    const c = 1 / Math.sqrt(9 * d);
    for (;;) {
      let x;
      let v;
      do {
        x = normal();
        v = 1 + c * x;
      } while (v <= 0);
      v = v * v * v;
      const u = uniform();
      if (u < 1 - 0.0331 * x * x * x * x) {
        return d * v;
      }
      if (Math.log(u) < 0.5 * x * x + d * (1 - v + Math.log(v))) {
        return d * v;
      }
    }
  }

  function gamma(shape, rate) {
    return gammaUnit(shape) / rate;
  }

  function beta(a, b) {
    const x = gammaUnit(a);
    const y = gammaUnit(b);
    return x / (x + y);
  }

  function studentT(df) {
    const chi2 = 2 * gammaUnit(df / 2);
    return normal() / Math.sqrt(chi2 / df);
  }

  function cauchy(location, scale) {
    return location + scale * Math.tan(Math.PI * (uniform() - 0.5));
  }

  function exponential(rate) {
    return -Math.log(uniform()) / rate;
  }

  return { uniform, normal, gamma, beta, studentT, cauchy, exponential };
}
