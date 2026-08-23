// Help tab: one card per distribution with a shape preview, parameter
// glossary, example use, and sources. Mirrors R/help_ui.R + R/help_server.R.

import { DIST_SPECS, getDistHelp, drawDist } from "./dist.js";
import { createRng } from "./rng.js";
import { drawDensity } from "./plots.js";
import { el } from "./domUtils.js";

export function buildHelpTab(container) {
  const grid = el("div", { class: "help-grid" });

  for (const [key, spec] of Object.entries(DIST_SPECS)) {
    const help = getDistHelp(key);
    const canvas = el("canvas", {
      class: "chart chart-preview",
      role: "img",
      "aria-label": `Density preview of the ${spec.label} distribution.`,
    });

    const paramRows = Object.entries(help.params || {}).map(([name, desc]) =>
      el("div", { class: "help-param" },
        el("span", { class: "help-param-name" }, name),
        el("span", { class: "help-param-desc" }, desc)));

    const sourceItems = (help.sources || []).map((src) =>
      el("li", {},
        el("a", { href: src.url, target: "_blank", rel: "noopener noreferrer" }, src.label)));

    grid.appendChild(
      el("div", { class: "soft-card help-card" },
        el("div", { class: "card-header" }, spec.label),
        el("div", { class: "card-body" },
          canvas,
          el("div", { class: "help-text" }, help.description),
          help.example
            ? el("div", { class: "help-example" },
                el("span", { class: "help-example-label" }, "Example:"),
                el("span", { class: "help-example-text" }, help.example))
            : null,
          paramRows.length > 0 ? el("div", { class: "help-params" }, ...paramRows) : null,
          sourceItems.length > 0
            ? el("div", { class: "help-sources" },
                el("div", { class: "help-sources-label" }, "Sources"),
                el("ul", { class: "help-sources-list" }, ...sourceItems))
            : null)));
  }

  container.appendChild(grid);

  function run() {
    const canvases = grid.querySelectorAll("canvas");
    let i = 0;
    for (const key of Object.keys(DIST_SPECS)) {
      const canvas = canvases[i++];
      const rng = createRng(1234 + i);
      const values =
        key === "flat"
          ? drawDist(rng, "uniform", { min: -3, max: 3 }, 4000)
          : drawDist(rng, key, DIST_SPECS[key].params, 4000);
      drawDensity(canvas, values, { trim: key !== "lkj_corr", xLabel: "" });
    }
  }

  return { run };
}
