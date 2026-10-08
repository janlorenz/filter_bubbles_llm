/**
 * Network Controls: Interactive UI for the network explorer.
 * 
 * Handles filter menus, dropdowns, button interactions, slider bindings,
 * manifest loading, and file upload.
 * 
 * Requires: NetworkViz instance (passed as `viz` parameter)
 */

// ── Pane → native SVG ───────────────────────────────────────────────
// Draws the open worldview/statement pane as plain SVG <rect>/<text>.
// Unlike <foreignObject>, this renders in browsers AND in Inkscape,
// Illustrator, Figma, etc., and does not depend on CSS classes/variables.
function buildPaneSVG(pane, colors, width) {
  const NS = "http://www.w3.org/2000/svg";
  const FONT = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif';
  const ctx = document.createElement("canvas").getContext("2d");
  const measure = (t, size, weight) => { ctx.font = `${weight} ${size}px ${FONT}`; return ctx.measureText(t).width; };
  const C = {
    bg: colors["--color-panel-bg"] || "#ffffff",
    border: colors["--color-border"] || "#cccccc",
    text: colors["--color-text"] || "#222222",
    muted: colors["--color-muted"] || "#777777",
    accent: colors["--color-accent"] || "#2255aa",
    stmt: colors["--color-stmt"] || "#2255aa",
    agent: colors["--color-agent"] || "#aa5522",
  };
  const PAD = 12, innerW = width - 2 * PAD;
  const g = document.createElementNS(NS, "g");
  const el = (tag, attrs, parent) => {
    const e = document.createElementNS(NS, tag);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    (parent || g).appendChild(e);
    return e;
  };
  const wrap = (text, size, weight, firstOffset) => {
    const words = String(text).replace(/\s+/g, " ").trim().split(" ").filter(Boolean);
    const lines = []; let line = "";
    words.forEach(w => {
      const limit = innerW - (lines.length === 0 ? firstOffset : 0);
      const test = line ? line + " " + w : w;
      if (line && measure(test, size, weight) > limit) { lines.push(line); line = w; }
      else line = test;
    });
    if (line || !lines.length) lines.push(line);
    return lines;
  };
  // "ID  text…" block with a bold coloured ID and wrapped text; returns new y
  const idBlock = (y, id, text, size, idColor, textColor, textWeight) => {
    const lh = size + 4;
    const off = id ? measure(id, size, 700) + 5 : 0;
    const lines = wrap(text, size, textWeight, off);
    lines.forEach((ln, i) => {
      const t = el("text", { x: PAD, y: y + lh * (i + 1) - 3, "font-family": FONT, "font-size": size, fill: textColor });
      if (i === 0 && id) {
        const a = el("tspan", { "font-weight": 700, fill: idColor }, t); a.textContent = id;
        const b = el("tspan", { dx: 5, "font-weight": textWeight }, t); b.textContent = ln;
      } else t.textContent = ln;
      if (i > 0 && id) t.setAttribute("x", PAD);
    });
    return y + lh * lines.length;
  };

  let y = 8;
  const isStmt = pane.id === "statement-pane";
  const titleEl = pane.querySelector(".worldview-pane__title, [id$='-title']");
  if (titleEl) {
    const sid = titleEl.querySelector(".statement-id");
    const stx = titleEl.querySelector(".statement-text");
    if (isStmt && (sid || stx)) {
      y = idBlock(y, sid ? sid.textContent.trim() : "", stx ? stx.textContent.trim() : "", 11, C.stmt, C.stmt, 400);
    } else {
      y = idBlock(y, "", titleEl.textContent.trim(), 11, C.accent, C.accent, 600);
    }
  }
  y += 6;
  el("line", { x1: 0, x2: width, y1: y, y2: y, stroke: C.border, "stroke-width": 1 });
  y += 8;

  const info = pane.querySelector(".statement-agents-info");
  if (info) {
    y = idBlock(y, "", info.textContent.trim(), 10, C.muted, C.muted, 400) + 4;
  }
  const chips = pane.querySelectorAll(".statement-agents-list .agent-id");
  if (chips.length) {
    let x = PAD; const H = 16, GAP = 6;
    chips.forEach(c => {
      const label = c.textContent.trim();
      const w = measure(label, 10, 600) + 12;
      if (x > PAD && x + w > PAD + innerW) { x = PAD; y += H + GAP; }
      el("rect", { x, y, width: w, height: H, rx: 3, fill: C.agent, "fill-opacity": 0.12 });
      const t = el("text", { x: x + 6, y: y + 11.5, "font-family": FONT, "font-size": 10, "font-weight": 600, fill: C.agent });
      t.textContent = label;
      x += w + GAP;
    });
    y += H + 6;
  }
  pane.querySelectorAll(".worldview-pane__item").forEach(item => {
    const id = (item.querySelector(".worldview-pane__item-id") || {}).textContent || "";
    const tx = (item.querySelector(".worldview-pane__item-text") || {}).textContent || item.textContent;
    y = idBlock(y, id.trim(), tx.trim(), 10, C.stmt, C.text, 400) + 2;
  });
  const empty = pane.querySelector(".worldview-pane__empty");
  if (empty) y = idBlock(y, "", empty.textContent.trim(), 10, C.muted, C.muted, 400);

  const height = y + 10;
  const box = document.createElementNS(NS, "rect");
  [["x", 0.5], ["y", 0.5], ["width", width - 1], ["height", height - 1], ["rx", 6],
   ["fill", C.bg], ["stroke", C.border], ["stroke-width", 1]].forEach(([k, v]) => box.setAttribute(k, v));
  g.insertBefore(box, g.firstChild);
  return { g, height };
}

// ── SVG Download ────────────────────────────────────────────────────
function downloadSVG(viz) {
  const svgEl = viz.svg.node();
  if (!svgEl) return;

  const root = document.getElementById("network-root");
  const rs = getComputedStyle(root);
  function cv(name) { return rs.getPropertyValue(name).trim(); }

  const colorMap = {
    "--color-agent": cv("--color-agent"),
    "--color-agent-stroke": cv("--color-agent-stroke"),
    "--color-stmt": cv("--color-stmt"),
    "--color-stmt-stroke": cv("--color-stmt-stroke"),
    "--color-edge": cv("--color-edge"),
    "--color-edge-hi": cv("--color-edge-hi"),
    "--color-muted": cv("--color-muted"),
    "--color-text": cv("--color-text"),
    "--color-accent": cv("--color-accent"),
    "--color-border": cv("--color-border"),
    "--color-panel-bg": cv("--color-panel-bg"),
  };

  // ── Compute tight bounding box of the network content in SVG coords ──
  // The root <g> has a transform from zoom/pan. We need the bounding box
  // of the content in screen pixels, then convert to a viewBox.
  const rootG = viz.root.node();
  const contentBBox = rootG.getBBox();
  // Get the current zoom transform
  const transform = d3.zoomTransform(svgEl);
  // Map content bbox through the zoom transform to screen coords
  const pad = 20; // padding around network content
  const netX = transform.applyX(contentBBox.x) - pad;
  const netY = transform.applyY(contentBBox.y) - pad;
  const netW = contentBBox.width * transform.k + 2 * pad;
  const netH = contentBBox.height * transform.k + 2 * pad;

  // ── Check for active pane ──
  const worldviewPane = document.getElementById("worldview-pane");
  const statementPane = document.getElementById("statement-pane");
  const activePane = worldviewPane?.classList.contains("worldview-pane--visible")
    ? worldviewPane
    : statementPane?.classList.contains("worldview-pane--visible")
      ? statementPane
      : null;

  // Measure pane position relative to the canvas wrapper (same coord space as the SVG)
  let paneRect = null;
  let canvasRect = null;
  let paneSvg = null;
  if (activePane) {
    canvasRect = svgEl.getBoundingClientRect();
    paneRect = activePane.getBoundingClientRect();
    paneSvg = buildPaneSVG(activePane, colorMap, paneRect.width);
  }

  // ── Build the output viewBox ──
  // Crop tightly around all visible content (network + pane if open).
  let vbX, vbY, vbW, vbH;

  if (paneRect && canvasRect) {
    const paneLeft = paneRect.left - canvasRect.left;
    const paneTop = paneRect.top - canvasRect.top;
    const paneH = paneSvg.height;

    vbX = Math.min(netX, paneLeft);
    vbY = Math.min(netY, paneTop);
    vbW = Math.max(netX + netW, paneLeft + paneRect.width) - vbX;
    vbH = Math.max(netY + netH, paneTop + paneH) - vbY;
  } else {
    vbX = netX;
    vbY = netY;
    vbW = netW;
    vbH = netH;
  }

  // ── Clone the SVG ──
  const clone = svgEl.cloneNode(true);
  clone.removeAttribute("id");
  clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  clone.setAttribute("xmlns:xlink", "http://www.w3.org/1999/xlink");
  clone.setAttribute("viewBox", `${vbX} ${vbY} ${vbW} ${vbH}`);
  clone.setAttribute("width", vbW);
  clone.setAttribute("height", vbH);

  // White background covering the viewBox
  const bg = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  bg.setAttribute("x", vbX);
  bg.setAttribute("y", vbY);
  bg.setAttribute("width", vbW);
  bg.setAttribute("height", vbH);
  bg.setAttribute("fill", cv("--color-bg") || "#ffffff");
  clone.insertBefore(bg, clone.firstChild);

  // ── Inline styles on network elements ──
  // Resolve fill/stroke CSS variables on circles
  clone.querySelectorAll(".node__circle").forEach(c => {
    const fill = c.getAttribute("fill") || "";
    const stroke = c.getAttribute("stroke") || "";
    for (const [varName, val] of Object.entries(colorMap)) {
      if (fill.includes(varName)) c.setAttribute("fill", val);
      if (stroke.includes(varName)) c.setAttribute("stroke", val);
    }
  });

  // Edges
  clone.querySelectorAll(".edge").forEach(line => {
    const isHi = line.classList.contains("edge--highlighted");
    line.setAttribute("stroke", isHi ? colorMap["--color-edge-hi"] : colorMap["--color-edge"]);
    line.setAttribute("stroke-opacity", isHi ? "1" : "0.55");
    line.setAttribute("stroke-width", isHi
      ? (line.style.strokeWidth || "1.8px")
      : (line.style.strokeWidth || "1px"));
  });

  // Labels
  clone.querySelectorAll(".node__label").forEach(t => {
    t.setAttribute("fill", colorMap["--color-muted"]);
    t.setAttribute("font-size", "8px");
  });
  clone.querySelectorAll(".node__label-id").forEach(t => {
    t.setAttribute("fill", colorMap["--color-stmt"]);
  });

  // Highlighted/dimmed nodes (persistent selection state)
  clone.querySelectorAll(".node--highlighted .node__circle").forEach(c => {
    c.setAttribute("stroke-width", "2.5px");
  });
  clone.querySelectorAll(".node--dimmed .node__circle").forEach(c => {
    c.setAttribute("opacity", "0.12");
  });
  clone.querySelectorAll(".node--dimmed .node__label").forEach(t => {
    t.setAttribute("opacity", "0.12");
  });

  // Hidden labels
  clone.querySelectorAll(".node--labels-hidden .node__label").forEach(t => {
    t.setAttribute("display", "none");
  });

  // Unconnected nodes
  clone.querySelectorAll(".node--unconnected").forEach(g => {
    g.setAttribute("display", "none");
  });

  // ── Add pane (native SVG) at its on-screen position ──
  if (paneSvg && paneRect && canvasRect) {
    paneSvg.g.setAttribute("transform",
      `translate(${paneRect.left - canvasRect.left}, ${paneRect.top - canvasRect.top})`);
    clone.appendChild(paneSvg.g);
  }

  // ── Serialize and trigger download ──
  const serializer = new XMLSerializer();
  let svgStr = serializer.serializeToString(clone);
  if (!svgStr.startsWith("<?xml")) {
    svgStr = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n' + svgStr;
  }
  const blob = new Blob([svgStr], { type: "image/svg+xml;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;

  const loadedName = document.getElementById("loaded-name");
  const fname = loadedName?.textContent?.trim().replace(/[^a-zA-Z0-9_\-]/g, "_") || "network";
  a.download = fname + ".svg";

  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

// Configuration: Initial network selection
const INITIAL_NETWORK_FILTERS = {
  N: 100,
  M: 5,
  openmindedness: 1,
  post: "FS",
  seed: 354,
  t: 100,
  timestamp: "260630-1208"
};

let allManifestEntries = [];
let viz = null; // Will be set by initControls()

// ── URL Query Param Override ─────────────────────────────────────────
// Allows linking directly to a specific network via e.g.
// networkexplorer.html?net=260924-210228_A100_M10_tmax100_op3_S_seed400_gpt_t100
// (filename without the "networkjson/" path prefix or ".json" extension).
function getRequestedNetworkName() {
  const params = new URLSearchParams(window.location.search);
  return params.get("net");
}

function findEntryByName(name) {
  if (!name) return null;
  return allManifestEntries.find(e => {
    const base = e.file.split("/").pop().replace(/\.json$/i, "");
    return base === name;
  }) || null;
}

// ── Manifest Loading ───────────────────────────────────────────────
function loadManifest(manifestPath) {
  fetch(manifestPath)
    .then(r => {
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      return r.json();
    })
    .then(data => {
      allManifestEntries = Array.isArray(data) ? data : [];
      const statusEl = document.getElementById("filter-status");
      if (allManifestEntries.length === 0) {
        if (statusEl) statusEl.textContent = "Manifest is empty.";
      } else {
        populateFilters();
        loadInitialNetwork();  // This will sync filters to the entry to load
        updateRunOptions(false);  // Then populate filter-run with the synced values
      }
    })
    .catch(err => {
      const statusEl = document.getElementById("filter-status");
      if (statusEl) statusEl.textContent = `Manifest failed to load: ${err.message}`;
    });
}

// ── Filter Helpers ────────────────────────────────────────────────
function fillSelect(id, values, current) {
  const sel = document.getElementById(id);
  if (!sel) return;
  sel.innerHTML = "";
  values.forEach(val => {
    const opt = document.createElement("option");
    opt.value = val;
    opt.textContent = val;
    sel.appendChild(opt);
  });
  if (current !== null && values.includes(current)) sel.value = current;
  else if (values.length > 0) sel.value = values[0];
  return sel.value;
}

function getTopFilters() {
  const v = id => {
    const el = document.getElementById(id);
    return el ? el.value : "";
  };
  return {
    N: v("filter-N"),
    M: v("filter-M"),
    open: v("filter-openmindedness"),
    post: v("filter-post"),
    seed: v("filter-seed"),
    t: v("filter-t")
  };
}

function matchesTopFilters(e, f) {
  return (f.N === "" || e.N == f.N) &&
    (f.M === "" || e.M == f.M) &&
    (f.open === "" || e.openmindedness == f.open) &&
    (f.post === "" || e.post === f.post) &&
    (f.seed === "" || e.seed == f.seed) &&
    (f.t === "" || e.t == f.t);
}

function matchesInitialFilters(entry) {
  const f = INITIAL_NETWORK_FILTERS;
  if (f.N !== undefined && f.N !== null && entry.N !== f.N) return false;
  if (f.M !== undefined && f.M !== null && entry.M !== f.M) return false;
  if (f.openmindedness !== undefined && f.openmindedness !== null && entry.openmindedness !== f.openmindedness) return false;
  if (f.seed !== undefined && f.seed !== null && entry.seed !== f.seed) return false;
  if (f.t !== undefined && f.t !== null && entry.t !== f.t) return false;
  if (f.post !== undefined && f.post !== null && entry.post !== f.post) return false;
  if (f.timestamp !== undefined && f.timestamp !== null && entry.timestamp !== f.timestamp) return false;
  return true;
}

function populateFilters() {
  const uniq = key => [...new Set(allManifestEntries.map(e => e[key]))].sort((a, b) => a - b);

  fillSelect("filter-N", uniq("N"), null);
  fillSelect("filter-M", uniq("M"), null);
  fillSelect("filter-openmindedness", uniq("openmindedness"), null);
  fillSelect("filter-seed", uniq("seed"), null);
  fillSelect("filter-t", uniq("t"), null);
  
  // Populate post dropdown with available values (S, F, FS only)
  const postValues = [...new Set(allManifestEntries.map(e => e.post))].sort();
  fillSelect("filter-post", postValues, postValues.length > 0 ? postValues[0] : null);

  ["filter-N", "filter-M", "filter-openmindedness", "filter-post", "filter-seed", "filter-t"].forEach(id => {
    const el = document.getElementById(id);
    if (el) el.addEventListener("change", () => updateRunOptions(true));
  });

  const runEl = document.getElementById("filter-run");
  if (runEl) runEl.addEventListener("change", tryAutoLoad);
}

function updateRunOptions(autoLoad = true) {
  const f = getTopFilters();
  const candidates = allManifestEntries.filter(e => matchesTopFilters(e, f));
  const runs = [...new Set(candidates.map(e => e.timestamp))].sort();

  const prev = document.getElementById("filter-run")?.value;
  fillSelect("filter-run", runs, prev);

  const statusEl = document.getElementById("filter-status");
  if (statusEl) {
    if (candidates.length === 0) {
      statusEl.textContent = "No files found";
      statusEl.style.color = "#d00";
    } else {
      statusEl.textContent = `${runs.length} run${runs.length !== 1 ? "s" : ""} available`;
      statusEl.style.color = "#888";
    }
  }

  if (autoLoad) tryAutoLoad();
}

function loadInitialNetwork() {
  const requestedName = getRequestedNetworkName();
  if (requestedName) {
    const requestedEntry = findEntryByName(requestedName);
    if (requestedEntry) {
      syncFiltersToEntry(requestedEntry);
      fetchAndLoad(requestedEntry);
      return;
    }
    console.warn(`No network matching ?net=${requestedName} found; falling back to INITIAL_NETWORK_FILTERS.`);
  }

  const entry = allManifestEntries.find(matchesInitialFilters);
  if (entry) {
    syncFiltersToEntry(entry);
    fetchAndLoad(entry);
  } else if (allManifestEntries.length > 0) {
    console.warn("No network matching INITIAL_NETWORK_FILTERS found; loading first entry.");
    syncFiltersToEntry(allManifestEntries[0]);
    fetchAndLoad(allManifestEntries[0]);
  }
}

function resolveEntry() {
  const f = getTopFilters();
  const run = document.getElementById("filter-run")?.value;
  return allManifestEntries.find(e => matchesTopFilters(e, f) && e.timestamp === run) || null;
}

function fetchAndLoad(entry) {
  const statusBar = document.getElementById("status-bar");
  if (statusBar) statusBar.textContent = "Loading…";

  fetch(entry.file)
    .then(r => {
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      return r.json();
    })
    .then(data => viz.loadData(data, entry))
    .catch(err => {
      if (statusBar) statusBar.textContent = `Failed to load: ${err.message}`;
    });
}

function tryAutoLoad() {
  const entry = resolveEntry();
  if (entry) fetchAndLoad(entry);
}

function syncFiltersToEntry(entry) {
  if (entry.N !== undefined) {
    const el = document.getElementById("filter-N");
    if (el) el.value = entry.N;
  }
  if (entry.M !== undefined) {
    const el = document.getElementById("filter-M");
    if (el) el.value = entry.M;
  }
  if (entry.openmindedness !== undefined) {
    const el = document.getElementById("filter-openmindedness");
    if (el) el.value = entry.openmindedness;
  }
  if (entry.seed !== undefined) {
    const el = document.getElementById("filter-seed");
    if (el) el.value = entry.seed;
  }
  if (entry.t !== undefined) {
    const el = document.getElementById("filter-t");
    if (el) el.value = entry.t;
  }
  if (entry.post !== undefined) {
    const el = document.getElementById("filter-post");
    if (el) el.value = entry.post;
  }
  if (entry.timestamp !== undefined) {
    const el = document.getElementById("filter-run");
    if (el) el.value = entry.timestamp;
  }
}

// ── File Upload ────────────────────────────────────────────────────
function setupFileUpload() {
  const fileInput = document.getElementById("file-input");
  if (fileInput) {
    fileInput.addEventListener("change", function() {
      const file = this.files[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = e => {
        try {
          viz.loadData(JSON.parse(e.target.result), null);
        } catch (err) {
          alert(`Could not parse JSON: ${err.message}`);
        }
      };
      reader.readAsText(file);
      this.value = "";
    });
  }
}

// ── Slider Bindings ────────────────────────────────────────────────
function bindSlider(slId, vlId, dec, fn) {
  const sl = document.getElementById(slId);
  const vl = document.getElementById(vlId);
  if (!sl || !vl) return;
  sl.addEventListener("input", () => {
    vl.textContent = (+sl.value).toFixed(dec);
    fn(+sl.value);
  });
}

function setupSliders() {
  bindSlider("sl-spread", "vl-spread", 0, v => {
    if (viz.sim) {
      viz.sim.force("charge", d3.forceManyBody().strength(-v));
      viz.sim.alpha(0.4).restart();
    }
  });

  bindSlider("sl-edgelen", "vl-edgelen", 0, v => {
    if (viz.sim) {
      viz.sim.force("link").distance(v);
      viz.sim.alpha(0.4).restart();
    }
  });

  bindSlider("sl-attract", "vl-attract", 2, () => {
    if (viz.sim) {
      viz.sim.force("link").strength(viz.linkStrength.bind(viz)());
      viz.sim.alpha(0.4).restart();
    }
  });

  bindSlider("sl-minweight", "vl-minweight", 0, () => {
    if (viz.netType !== "AS") viz.applyWeightFilter();
  });
}

// ── Button Setup ───────────────────────────────────────────────────
function setupButtons() {
  // Reset
  const btnReset = document.getElementById("btn-reset");
  if (btnReset) {
    btnReset.addEventListener("click", () => {
      viz.frozen = false;
      const btnFreeze = document.getElementById("btn-freeze");
      if (btnFreeze) btnFreeze.textContent = "⏸ Freeze";
      viz.DATA.nodes.forEach(d => {
        d.x = undefined;
        d.y = undefined;
        d.vx = 0;
        d.vy = 0;
        d.fx = null;
        d.fy = null;
      });
      viz.buildSim();
      viz.sim.alpha(1).restart();
    });
  }

  // Freeze
  const btnFreeze = document.getElementById("btn-freeze");
  if (btnFreeze) {
    btnFreeze.addEventListener("click", function() {
      viz.frozen = !viz.frozen;
      if (viz.frozen) {
        viz.sim.stop();
        this.textContent = "▶ Unfreeze";
      } else {
        viz.sim.alpha(0.3).restart();
        this.textContent = "⏸ Freeze";
      }
    });
  }

  // Labels
  const btnLabels = document.getElementById("btn-labels");
  if (btnLabels) {
    btnLabels.addEventListener("click", function() {
      viz.labelsOn = !viz.labelsOn;
      if (viz.nodeSel) viz.nodeSel.classed("node--labels-hidden", !viz.labelsOn);
      this.textContent = viz.labelsOn ? "Labels on" : "Labels off";
      this.classList.toggle("btn--active", viz.labelsOn);
    });
  }

  // Size by degree
  const btnSize = document.getElementById("btn-size");
  if (btnSize) {
    btnSize.addEventListener("click", function() {
      viz.sizeByDeg = !viz.sizeByDeg;
      this.classList.toggle("btn--active", viz.sizeByDeg);
      this.textContent = viz.sizeByDeg ? "Statement size: degree" : "Statement size: uniform";
      if (viz.circles) {
        viz.circles.filter(d => d.type === "statement")
          .transition().duration(400)
          .attr("r", d => viz.nodeR(d, viz.sizeByDeg));
      }
      if (viz.sim) {
        viz.sim.force("collide", d3.forceCollide(d => viz.nodeR(d, viz.sizeByDeg) + 2));
        viz.sim.alpha(0.15).restart();
      }
    });
  }

  // Network type
  const filterNettype = document.getElementById("filter-nettype");
  if (filterNettype) {
    filterNettype.addEventListener("change", () => {
      const type = filterNettype.value;
      viz.updateNetworkType(type);
    });
  }

  // Weight forces
  const btnWeightForces = document.getElementById("btn-weight-forces");
  if (btnWeightForces) {
    btnWeightForces.addEventListener("click", function() {
      viz.weightedForces = !viz.weightedForces;
      this.textContent = viz.weightedForces ? "Weight forces: on" : "Weight forces: off";
      this.classList.toggle("btn--active", viz.weightedForces);

      if (viz.weightedForces) {
        const slAttract = document.getElementById("sl-attract");
        if (slAttract) {
          viz.attractBeforeWeightOn = +slAttract.value;
          const newA = Math.min(+slAttract.value * 4, 1);
          slAttract.value = newA;
          const vlAttract = document.getElementById("vl-attract");
          if (vlAttract) vlAttract.textContent = newA.toFixed(3).replace(/\.?0+$/, "");
        }
      } else {
        const slAttract = document.getElementById("sl-attract");
        if (slAttract) {
          const restoreA = viz.attractBeforeWeightOn !== null ? viz.attractBeforeWeightOn : +slAttract.value / 4;
          viz.attractBeforeWeightOn = null;
          slAttract.value = restoreA;
          const vlAttract = document.getElementById("vl-attract");
          if (vlAttract) vlAttract.textContent = restoreA.toFixed(3).replace(/\.?0+$/, "");
        }
      }

      if (viz.sim) {
        viz.sim.force("link").strength(viz.linkStrength.bind(viz)());
        viz.sim.alpha(0.4).restart();
      }
    });
  }

  // Defaults
  const btnDefaults = document.getElementById("btn-defaults");
  if (btnDefaults) {
    btnDefaults.addEventListener("click", () => {
      const d = viz.NET_DEFAULTS[viz.netType] || viz.NET_DEFAULTS.AS;
      const attract = viz.weightedForces ? Math.min(d.attract * 4, 1) : d.attract;
      viz.setSliders({ ...d, attract });
      viz.attractBeforeWeightOn = viz.weightedForces ? d.attract : null;
      if (viz.sim) {
        viz.sim.force("charge", d3.forceManyBody().strength(-d.spread));
        viz.sim.force("link").distance(d.edgelen).strength(viz.linkStrength.bind(viz)());
        viz.sim.alpha(0.4).restart();
      }
    });
  }

  // Zoom in
  const btnZoomIn = document.getElementById("btn-zoom-in");
  if (btnZoomIn && viz.svg) {
    btnZoomIn.addEventListener("click", () => {
      viz.svg.transition().duration(300).call(viz.zoomBehavior.scaleBy, 1.3);
    });
  }

  // Zoom out
  const btnZoomOut = document.getElementById("btn-zoom-out");
  if (btnZoomOut && viz.svg) {
    btnZoomOut.addEventListener("click", () => {
      viz.svg.transition().duration(300).call(viz.zoomBehavior.scaleBy, 1 / 1.3);
    });
  }

  // Download SVG
  const btnDownload = document.getElementById("btn-download-svg");
  if (btnDownload) {
    btnDownload.addEventListener("click", () => downloadSVG(viz));
  }

  // Worldview pane close
  const worldviewClose = document.getElementById("worldview-close");
  if (worldviewClose) {
    worldviewClose.addEventListener("click", e => {
      e.stopPropagation();
      const worldviewPane = document.getElementById("worldview-pane");
      if (worldviewPane) worldviewPane.classList.remove("worldview-pane--visible");
      viz.clearSelection();
      viz.DATA.nodes.forEach(d => {
        d.fx = null;
        d.fy = null;
      });
    });
  }

  // Statement pane close
  const statementClose = document.getElementById("statement-close");
  if (statementClose) {
    statementClose.addEventListener("click", e => {
      e.stopPropagation();
      const statementPane = document.getElementById("statement-pane");
      if (statementPane) statementPane.classList.remove("worldview-pane--visible");
      viz.clearSelection();
      viz.DATA.nodes.forEach(d => {
        d.fx = null;
        d.fy = null;
      });
    });
  }
}

// ── Main Initialization ────────────────────────────────────────────
function initControls(vizInstance, manifestPath = "manifest.json") {
  viz = vizInstance;

  // Measure header height
  document.addEventListener("DOMContentLoaded", () => {
    const h = document.querySelector(".header");
    if (h) {
      document.documentElement.style.setProperty("--header-height", h.offsetHeight + "px");
    }
  });

  // Setup UI components
  setupSliders();
  setupButtons();
  setupFileUpload();

  // Load manifest and auto-select initial network
  loadManifest(manifestPath);
}
