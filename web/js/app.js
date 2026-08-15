// App shell: tab navigation and lazy per-tab initialization.

import { buildModelTab } from "./modelTab.js";
import { buildPriorStrengthTab } from "./priorStrengthTab.js";
import { buildHelpTab } from "./helpTab.js";

const TABS = [
  { key: "linear", label: "Linear" },
  { key: "logistic", label: "Logistic" },
  { key: "poisson", label: "Poisson" },
  { key: "gamma", label: "Gamma" },
  { key: "negbin", label: "NegBin" },
  { key: "multilevel", label: "Multilevel" },
  { key: "prior_strength", label: "Prior Strength" },
  { key: "help", label: "Help" },
];

const nav = document.getElementById("tab-nav");
const panelsHolder = document.getElementById("tab-panels");
const built = {};

const buttons = {};
const panels = {};

for (const tab of TABS) {
  const button = document.createElement("button");
  button.type = "button";
  button.role = "tab";
  button.id = `tab-${tab.key}`;
  button.setAttribute("aria-controls", `panel-${tab.key}`);
  button.setAttribute("aria-selected", "false");
  button.textContent = tab.label;
  button.addEventListener("click", () => activate(tab.key));
  nav.appendChild(button);
  buttons[tab.key] = button;

  const panel = document.createElement("section");
  panel.id = `panel-${tab.key}`;
  panel.role = "tabpanel";
  panel.setAttribute("aria-labelledby", `tab-${tab.key}`);
  panel.hidden = true;
  panelsHolder.appendChild(panel);
  panels[tab.key] = panel;
}

function activate(key) {
  for (const tab of TABS) {
    const selected = tab.key === key;
    buttons[tab.key].setAttribute("aria-selected", String(selected));
    buttons[tab.key].classList.toggle("active", selected);
    panels[tab.key].hidden = !selected;
  }
  if (!built[key]) {
    if (key === "prior_strength") {
      built[key] = buildPriorStrengthTab(panels[key]);
    } else if (key === "help") {
      built[key] = buildHelpTab(panels[key]);
    } else {
      built[key] = buildModelTab(panels[key], key);
    }
    // First visit renders with defaults, like the Shiny app's initial run.
    built[key].run();
  }
}

activate("linear");
