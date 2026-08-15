// Small DOM helpers. All text is set via textContent (no HTML injection).

let idCounter = 0;

export function el(tag, attrs = {}, ...children) {
  const node = document.createElement(tag);
  for (const [key, value] of Object.entries(attrs)) {
    if (value === undefined || value === null) continue;
    if (key === "hidden") {
      node.hidden = Boolean(value);
    } else if (key === "class") {
      node.className = value;
    } else {
      node.setAttribute(key, value);
    }
  }
  for (const child of children) {
    if (child === null || child === undefined) continue;
    node.append(child);
  }
  return node;
}

export function card(header, body) {
  return el("div", { class: "soft-card" },
    el("div", { class: "card-header" }, header),
    el("div", { class: "card-body" }, body));
}

export function numberField(label, value, step) {
  const id = `field-${idCounter++}`;
  const input = el("input", {
    type: "number",
    id,
    value: String(value),
    step: String(step),
  });
  const wrap = el("div", { class: "field" }, el("label", { for: id }, label), input);
  return { wrap, input };
}

export function selectField(label, options, selected) {
  const id = `field-${idCounter++}`;
  const select = el("select", { id },
    ...options.map((opt) =>
      el("option", { value: opt.value, selected: opt.value === selected ? "" : undefined },
        opt.label)));
  const wrap = el("div", { class: "field" }, el("label", { for: id }, label), select);
  return { wrap, select };
}

export function clamp(value, bounds) {
  return Math.min(Math.max(value, bounds.min), bounds.max);
}

export function safeNumber(raw, fallback) {
  const value = typeof raw === "number" ? raw : parseFloat(raw);
  return Number.isFinite(value) ? value : fallback;
}
