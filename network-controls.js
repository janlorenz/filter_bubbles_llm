/**
 * Network Controls: Interactive UI for the network explorer.
 * 
 * Handles filter menus, dropdowns, button interactions, slider bindings,
 * manifest loading, and file upload.
 * 
 * Requires: NetworkViz instance (passed as `viz` parameter)
 */

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

  // Worldview pane close
  const worldviewClose = document.getElementById("worldview-close");
  if (worldviewClose) {
    worldviewClose.addEventListener("click", e => {
      e.stopPropagation();
      const worldviewPane = document.getElementById("worldview-pane");
      if (worldviewPane) worldviewPane.classList.remove("worldview-pane--visible");
      if (viz.nodeSel) {
        viz.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
      }
      if (viz.linkSel) viz.linkSel.classed("edge--highlighted", false);
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
      if (viz.nodeSel) {
        viz.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
      }
      if (viz.linkSel) viz.linkSel.classed("edge--highlighted", false);
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
