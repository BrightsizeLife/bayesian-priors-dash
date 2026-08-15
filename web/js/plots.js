// Canvas chart helpers: dark theme, recessive grid, single accent series.

import { quantileSorted, sd as sdFn } from "./summaries.js";

export const THEME = {
  surface: "#141824",
  grid: "#2a3140",
  axisText: "#cbd5e1",
  axisTitle: "#e6e8f0",
  accent: "#7dd3fc",
  reference: "#6b7280",
  subtitle: "#9aa4b5",
  font: "12px 'Segoe UI', system-ui, sans-serif",
  smallFont: "11px 'Segoe UI', system-ui, sans-serif",
};

// Scale a canvas for the device pixel ratio; returns logical width/height.
function sizeCanvas(canvas) {
  const dpr = window.devicePixelRatio || 1;
  const width = canvas.clientWidth || 500;
  const height = canvas.clientHeight || 320;
  canvas.width = Math.round(width * dpr);
  canvas.height = Math.round(height * dpr);
  const ctx = canvas.getContext("2d");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  return { ctx, width, height };
}

// "Nice" tick positions covering [lo, hi].
function niceTicks(lo, hi, count = 5) {
  if (!Number.isFinite(lo) || !Number.isFinite(hi) || lo === hi) {
    return [lo];
  }
  const span = hi - lo;
  const rawStep = span / count;
  const magnitude = Math.pow(10, Math.floor(Math.log10(rawStep)));
  let step = magnitude;
  for (const mult of [1, 2, 2.5, 5, 10]) {
    if (magnitude * mult >= rawStep) {
      step = magnitude * mult;
      break;
    }
  }
  const ticks = [];
  for (let t = Math.ceil(lo / step) * step; t <= hi + step * 1e-9; t += step) {
    ticks.push(Math.abs(t) < step * 1e-9 ? 0 : t);
  }
  return ticks;
}

function formatTick(value, percent) {
  if (percent) return `${Math.round(value * 100)}%`;
  if (value === 0) return "0";
  const abs = Math.abs(value);
  if (abs >= 1e5 || abs < 1e-3) return value.toExponential(0);
  if (abs >= 100) return value.toFixed(0);
  if (abs >= 1) return String(parseFloat(value.toFixed(2)));
  return String(parseFloat(value.toFixed(3)));
}

// Build a plot frame: margins, scales, grid, axes, labels.
export function makeFrame(canvas, opts) {
  const { ctx, width, height } = sizeCanvas(canvas);
  const margin = {
    top: opts.subtitle ? 28 : 12,
    right: 14,
    bottom: 36,
    left: 54,
  };
  const plotW = width - margin.left - margin.right;
  const plotH = height - margin.top - margin.bottom;

  let [xLo, xHi] = opts.xDomain;
  let [yLo, yHi] = opts.yDomain;
  if (xLo === xHi) [xLo, xHi] = [xLo - 1, xHi + 1];
  if (yLo === yHi) [yLo, yHi] = [yLo - 1, yHi + 1];

  const logY = Boolean(opts.logY);
  const yLoT = logY ? Math.log10(yLo) : yLo;
  const yHiT = logY ? Math.log10(yHi) : yHi;

  const xScale = (v) => margin.left + ((v - xLo) / (xHi - xLo)) * plotW;
  const yScale = (v) => {
    const t = logY ? Math.log10(v) : v;
    return margin.top + plotH - ((t - yLoT) / (yHiT - yLoT)) * plotH;
  };

  ctx.fillStyle = THEME.surface;
  ctx.fillRect(0, 0, width, height);

  const xTicks = niceTicks(xLo, xHi, 6);
  const yTicks = logY
    ? niceTicks(yLoT, yHiT, 5).map((t) => Math.pow(10, t))
    : niceTicks(yLo, yHi, 5);

  ctx.strokeStyle = THEME.grid;
  ctx.lineWidth = 1;
  for (const t of xTicks) {
    const px = xScale(t);
    ctx.beginPath();
    ctx.moveTo(px, margin.top);
    ctx.lineTo(px, margin.top + plotH);
    ctx.stroke();
  }
  for (const t of yTicks) {
    const py = yScale(t);
    ctx.beginPath();
    ctx.moveTo(margin.left, py);
    ctx.lineTo(margin.left + plotW, py);
    ctx.stroke();
  }

  ctx.fillStyle = THEME.axisText;
  ctx.font = THEME.smallFont;
  ctx.textAlign = "center";
  ctx.textBaseline = "top";
  for (const t of xTicks) {
    ctx.fillText(formatTick(t, false), xScale(t), margin.top + plotH + 6);
  }
  ctx.textAlign = "right";
  ctx.textBaseline = "middle";
  for (const t of yTicks) {
    ctx.fillText(formatTick(t, opts.yPercent), margin.left - 7, yScale(t));
  }

  ctx.fillStyle = THEME.axisTitle;
  ctx.font = THEME.font;
  ctx.textAlign = "center";
  ctx.textBaseline = "alphabetic";
  if (opts.xLabel) {
    ctx.fillText(opts.xLabel, margin.left + plotW / 2, height - 6);
  }
  if (opts.yLabel) {
    ctx.save();
    ctx.translate(12, margin.top + plotH / 2);
    ctx.rotate(-Math.PI / 2);
    ctx.fillText(opts.yLabel, 0, 0);
    ctx.restore();
  }
  if (opts.subtitle) {
    ctx.fillStyle = THEME.subtitle;
    ctx.textAlign = "left";
    ctx.fillText(opts.subtitle, margin.left, 16);
  }

  return { ctx, width, height, margin, plotW, plotH, xScale, yScale, xLo, xHi, yLo, yHi };
}

function domain(values) {
  let lo = Infinity;
  let hi = -Infinity;
  for (const v of values) {
    if (!Number.isFinite(v)) continue;
    if (v < lo) lo = v;
    if (v > hi) hi = v;
  }
  if (lo === Infinity) return [0, 1];
  return [lo, hi];
}

// Single-series line chart with an optional dashed reference line and a
// crosshair tooltip (nearest x).
export function drawLineChart(canvas, tooltipEl, data, opts) {
  const { x, y } = data;
  const finiteY = Array.from(y).filter(Number.isFinite);
  const yDom = domain(finiteY);
  if (opts.refY !== undefined) {
    yDom[0] = Math.min(yDom[0], opts.refY);
    yDom[1] = Math.max(yDom[1], opts.refY);
  }
  const pad = (yDom[1] - yDom[0]) * 0.06 || 0.5;
  const frame = makeFrame(canvas, {
    xDomain: domain(x),
    yDomain: opts.logY ? yDom : [yDom[0] - pad, yDom[1] + pad],
    xLabel: opts.xLabel,
    yLabel: opts.yLabel,
    yPercent: opts.yPercent,
    logY: opts.logY,
    subtitle: opts.subtitle,
  });
  const { ctx } = frame;

  if (opts.refY !== undefined) {
    ctx.strokeStyle = THEME.reference;
    ctx.setLineDash([5, 4]);
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(frame.margin.left, frame.yScale(opts.refY));
    ctx.lineTo(frame.margin.left + frame.plotW, frame.yScale(opts.refY));
    ctx.stroke();
    ctx.setLineDash([]);
  }

  ctx.strokeStyle = THEME.accent;
  ctx.lineWidth = 2;
  ctx.lineJoin = "round";
  ctx.beginPath();
  let started = false;
  for (let i = 0; i < x.length; i++) {
    if (!Number.isFinite(y[i])) continue;
    const px = frame.xScale(x[i]);
    const py = frame.yScale(y[i]);
    if (started) {
      ctx.lineTo(px, py);
    } else {
      ctx.moveTo(px, py);
      started = true;
    }
  }
  ctx.stroke();

  attachCrosshair(canvas, tooltipEl, frame, data, opts);
}

// Crosshair + tooltip: redraws the line chart with a marker on hover.
function attachCrosshair(canvas, tooltipEl, frame, data, opts) {
  if (!tooltipEl) return;
  canvas.onmousemove = (event) => {
    const rect = canvas.getBoundingClientRect();
    const mx = event.clientX - rect.left;
    if (mx < frame.margin.left || mx > frame.margin.left + frame.plotW) {
      tooltipEl.hidden = true;
      redraw();
      return;
    }
    let best = 0;
    let bestDist = Infinity;
    for (let i = 0; i < data.x.length; i++) {
      const d = Math.abs(frame.xScale(data.x[i]) - mx);
      if (d < bestDist) {
        bestDist = d;
        best = i;
      }
    }
    redraw(best);
    const yVal = data.y[best];
    const yText = opts.yPercent
      ? `${(yVal * 100).toFixed(1)}%`
      : String(parseFloat(yVal.toPrecision(4)));
    tooltipEl.textContent = `${opts.xLabel || "x"} = ${formatTick(data.x[best], false)} · ${
      opts.tooltipLabel || opts.yLabel || "y"
    } = ${yText}`;
    tooltipEl.hidden = false;
    const px = frame.xScale(data.x[best]);
    const py = frame.yScale(yVal);
    const parent = canvas.parentElement.getBoundingClientRect();
    tooltipEl.style.left = `${Math.min(rect.left - parent.left + px + 12, parent.width - 170)}px`;
    tooltipEl.style.top = `${rect.top - parent.top + Math.max(py - 34, 4)}px`;
  };
  canvas.onmouseleave = () => {
    tooltipEl.hidden = true;
    redraw();
  };

  function redraw(markerIdx) {
    drawLineOnly(canvas, frame, data, opts, markerIdx);
  }
}

// Redraw pass used by the crosshair (avoids recomputing the frame).
function drawLineOnly(canvas, frame, data, opts, markerIdx) {
  const fresh = makeFrame(canvas, {
    xDomain: [frame.xLo, frame.xHi],
    yDomain: [frame.yLo, frame.yHi],
    xLabel: opts.xLabel,
    yLabel: opts.yLabel,
    yPercent: opts.yPercent,
    logY: opts.logY,
    subtitle: opts.subtitle,
  });
  const { ctx } = fresh;
  if (opts.refY !== undefined) {
    ctx.strokeStyle = THEME.reference;
    ctx.setLineDash([5, 4]);
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(fresh.margin.left, fresh.yScale(opts.refY));
    ctx.lineTo(fresh.margin.left + fresh.plotW, fresh.yScale(opts.refY));
    ctx.stroke();
    ctx.setLineDash([]);
  }
  ctx.strokeStyle = THEME.accent;
  ctx.lineWidth = 2;
  ctx.beginPath();
  let started = false;
  for (let i = 0; i < data.x.length; i++) {
    if (!Number.isFinite(data.y[i])) continue;
    const px = fresh.xScale(data.x[i]);
    const py = fresh.yScale(data.y[i]);
    if (started) ctx.lineTo(px, py);
    else {
      ctx.moveTo(px, py);
      started = true;
    }
  }
  ctx.stroke();
  if (markerIdx !== undefined && Number.isFinite(data.y[markerIdx])) {
    const px = fresh.xScale(data.x[markerIdx]);
    const py = fresh.yScale(data.y[markerIdx]);
    ctx.strokeStyle = THEME.grid;
    ctx.lineWidth = 1;
    ctx.setLineDash([3, 3]);
    ctx.beginPath();
    ctx.moveTo(px, fresh.margin.top);
    ctx.lineTo(px, fresh.margin.top + fresh.plotH);
    ctx.stroke();
    ctx.setLineDash([]);
    ctx.fillStyle = THEME.accent;
    ctx.beginPath();
    ctx.arc(px, py, 4.5, 0, 2 * Math.PI);
    ctx.fill();
    ctx.strokeStyle = THEME.surface;
    ctx.lineWidth = 2;
    ctx.stroke();
  }
}

// Spaghetti plot: one translucent curve per simulation.
export function drawCurves(canvas, x, curves, opts) {
  let all = [];
  for (const c of curves) for (const v of c) all.push(v);
  let yDom = domain(all);
  let capped = null;
  if (opts.capQuantile) {
    const sorted = Float64Array.from(all.filter(Number.isFinite)).sort();
    capped = quantileSorted(sorted, opts.capQuantile);
    yDom = [yDom[0], Math.min(yDom[1], capped)];
  }
  const frame = makeFrame(canvas, {
    xDomain: domain(x),
    yDomain: yDom,
    xLabel: opts.xLabel || "x",
    yLabel: opts.yLabel,
  });
  const { ctx } = frame;
  ctx.strokeStyle = THEME.accent;
  ctx.globalAlpha = Math.max(0.03, Math.min(1, 40 / curves.length));
  ctx.lineWidth = 1;
  for (const curve of curves) {
    ctx.beginPath();
    for (let i = 0; i < x.length; i++) {
      const v = capped === null ? curve[i] : Math.min(curve[i], capped);
      const px = frame.xScale(x[i]);
      const py = frame.yScale(v);
      if (i === 0) ctx.moveTo(px, py);
      else ctx.lineTo(px, py);
    }
    ctx.stroke();
  }
  ctx.globalAlpha = 1;
}

// Point cloud (linear / multilevel implied data).
export function drawScatter(canvas, xs, ys, opts) {
  const frame = makeFrame(canvas, {
    xDomain: domain(xs),
    yDomain: domain(ys),
    xLabel: opts.xLabel || "x",
    yLabel: opts.yLabel || "y",
  });
  const { ctx } = frame;
  ctx.fillStyle = THEME.accent;
  ctx.globalAlpha = opts.alpha || 0.05;
  for (let i = 0; i < xs.length; i++) {
    if (!Number.isFinite(ys[i])) continue;
    ctx.beginPath();
    ctx.arc(frame.xScale(xs[i]), frame.yScale(ys[i]), 1.3, 0, 2 * Math.PI);
    ctx.fill();
  }
  ctx.globalAlpha = 1;
}

// Gaussian KDE with R's nrd0 bandwidth, evaluated over a binned grid.
export function kde(values, nOut = 256, cut = 3) {
  const finite = Float64Array.from(values).filter(Number.isFinite);
  const n = finite.length;
  if (n < 2) return { x: [0], y: [0] };
  const sorted = Float64Array.from(finite).sort();
  const iqr = quantileSorted(sorted, 0.75) - quantileSorted(sorted, 0.25);
  const s = sdFn(finite);
  let spread = Math.min(s, iqr / 1.34);
  if (!(spread > 0)) spread = s > 0 ? s : 1;
  const h = 0.9 * spread * Math.pow(n, -0.2);

  const lo = sorted[0] - cut * h;
  const hi = sorted[n - 1] + cut * h;

  // Linear binning, then convolve bins with the Gaussian kernel.
  const nBins = 1024;
  const bins = new Float64Array(nBins);
  const binW = (hi - lo) / (nBins - 1);
  for (const v of finite) {
    const pos = (v - lo) / binW;
    const b = Math.floor(pos);
    const frac = pos - b;
    if (b >= 0 && b < nBins) bins[b] += 1 - frac;
    if (b + 1 >= 0 && b + 1 < nBins) bins[b + 1] += frac;
  }

  const x = new Float64Array(nOut);
  const y = new Float64Array(nOut);
  const norm = 1 / (n * h * Math.sqrt(2 * Math.PI));
  const window = Math.ceil((4 * h) / binW);
  for (let i = 0; i < nOut; i++) {
    const xi = lo + ((hi - lo) * i) / (nOut - 1);
    const center = Math.round((xi - lo) / binW);
    let total = 0;
    for (let b = Math.max(0, center - window); b <= Math.min(nBins - 1, center + window); b++) {
      if (bins[b] === 0) continue;
      const z = (xi - (lo + b * binW)) / h;
      total += bins[b] * Math.exp(-0.5 * z * z);
    }
    x[i] = xi;
    y[i] = total * norm;
  }
  return { x, y };
}

// Density area chart for one parameter's draws.
export function drawDensity(canvas, values, opts = {}) {
  let data = Float64Array.from(values).filter(Number.isFinite);
  if (opts.trim !== false && data.length > 10) {
    const sorted = Float64Array.from(data).sort();
    const lo = quantileSorted(sorted, 0.005);
    const hi = quantileSorted(sorted, 0.995);
    data = data.filter((v) => v >= lo && v <= hi);
  }
  const dens = kde(data);
  const frame = makeFrame(canvas, {
    xDomain: domain(dens.x),
    yDomain: [0, Math.max(...dens.y) * 1.05 || 1],
    xLabel: opts.xLabel || "Value",
    yLabel: opts.yLabel,
  });
  const { ctx } = frame;
  ctx.beginPath();
  ctx.moveTo(frame.xScale(dens.x[0]), frame.yScale(0));
  for (let i = 0; i < dens.x.length; i++) {
    ctx.lineTo(frame.xScale(dens.x[i]), frame.yScale(dens.y[i]));
  }
  ctx.lineTo(frame.xScale(dens.x[dens.x.length - 1]), frame.yScale(0));
  ctx.closePath();
  ctx.fillStyle = "rgba(125, 211, 252, 0.22)";
  ctx.fill();
  ctx.strokeStyle = THEME.accent;
  ctx.lineWidth = 2;
  ctx.beginPath();
  for (let i = 0; i < dens.x.length; i++) {
    const px = frame.xScale(dens.x[i]);
    const py = frame.yScale(dens.y[i]);
    if (i === 0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  }
  ctx.stroke();
}
