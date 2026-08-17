/**
 * NetworkViz: Core visualization module for bipartite network graphs.
 * 
 * Handles D3 visualization, force-directed simulation, user interactions (hover/click),
 * and state management. Can be used standalone or with control modules.
 */
function forceHorizontalBias(strength = 0.08) {
  let nodes;

  function force(alpha) {
    if (!nodes || nodes.length < 2) return;

    // Weighted centroid
    let cx = 0, cy = 0;
    for (const d of nodes) { cx += d.x; cy += d.y; }
    cx /= nodes.length; cy /= nodes.length;

    // Covariance matrix
    let Sxx = 0, Syy = 0, Sxy = 0;
    for (const d of nodes) {
      const dx = d.x - cx, dy = d.y - cy;
      Sxx += dx * dx; Syy += dy * dy; Sxy += dx * dy;
    }

    // Skip near-circular layouts — angle is noisy/meaningless there
    const eccentricity = Math.abs(Sxx - Syy) / (Sxx + Syy + 1e-6);
    if (eccentricity < 0.05) return;

    // Angle of major axis, already smallest-rotation form (range -90°..90°)
    const theta = 0.5 * Math.atan2(2 * Sxy, Sxx - Syy);

    // Small rotation step toward theta = 0 (horizontal)
    const dtheta = -theta * strength;

    for (const d of nodes) {
      const dx = d.x - cx, dy = d.y - cy;
      d.vx += -dtheta * dy;
      d.vy += dtheta * dx;
    }
  }

  force.initialize = (_nodes) => { nodes = _nodes; };
  return force;
}

class NetworkViz {
  constructor(containerId) {
    this.containerId = containerId;
    this.container = document.getElementById(containerId);
    if (!this.container) throw new Error(`Container #${containerId} not found`);

    // Data
    this.DATA = { nodes: [], links: [] };
    this.rawData = { nodes: [], links: [] };

    // Indices
    this.degree = {};
    this.neighbours = {};
    this.nodeById = {};

    // State
    this.frozen = false;
    this.labelsOn = false;
    this.sizeByDeg = false;
    this.netType = "AS";
    this.weightedForces = false;
    this.attractBeforeWeightOn = null;
    this.minWeight = 1;

    // DOM references
    this.svg = null;
    this.root = null;
    this.linkGroup = null;
    this.nodeGroup = null;
    this.linkSel = null;
    this.nodeSel = null;
    this.circles = null;
    this.sim = null;
    this.zoomBehavior = null;

    // Constants
    this.AGENT_R = 5;
    this.STMT_BASE_R = 7;
    this.NET_DEFAULTS = {
      AS: { spread: 100, edgelen: 10, attract: 0.75 },
      AA: { spread: 200, edgelen: 30, attract: 0.075 },
      SS: { spread: 200, edgelen: 30, attract: 0.075 },
    };

    this.init();
  }

  init() {
    // Setup SVG: look for #graph within container first, then globally
    let svgEl = this.container.querySelector("#graph");
    if (!svgEl) {
      // If not found, look globally (for backward compatibility)
      svgEl = document.querySelector("#graph");
    }
    
    if (!svgEl) {
      throw new Error(`SVG #graph not found in container #${this.containerId}`);
    }

    this.svg = d3.select(svgEl);
    this.root = this.svg.append("g");

    this.zoomBehavior = d3.zoom()
      .scaleExtent([0.04, 10])
      .on("zoom", e => this.root.attr("transform", e.transform));
    
    this.svg.call(this.zoomBehavior)
      .on("dblclick.zoom", null);

    this.linkGroup = this.root.append("g");
    this.nodeGroup = this.root.append("g");

    // Setup resize listener
    window.addEventListener("resize", () => this.onResize());
  }

  // ── Helpers ────────────────────────────────────────────────────────
  eid(x) {
    return (x !== null && typeof x === "object") ? x.id : x;
  }

  buildIndex() {
    Object.keys(this.degree).forEach(k => delete this.degree[k]);
    Object.keys(this.neighbours).forEach(k => delete this.neighbours[k]);
    this.nodeById = {};

    this.DATA.nodes.forEach(n => {
      this.degree[n.id] = 0;
      this.neighbours[n.id] = new Set();
      this.nodeById[n.id] = n;
    });

    this.DATA.links.forEach(l => {
      const s = l._s, t = l._t;
      if (s === undefined || t === undefined) return;
      this.degree[s] = (this.degree[s] || 0) + 1;
      this.degree[t] = (this.degree[t] || 0) + 1;
      this.neighbours[s].add(t);
      this.neighbours[t].add(s);
    });
  }

  tagLinks() {
    this.DATA.links.forEach(l => {
      l._s = this.eid(l.source);
      l._t = this.eid(l.target);
    });
  }

  nodeR(d, byDeg) {
    if (d.type === "agent") return this.AGENT_R;
    if (!byDeg) return this.STMT_BASE_R;
    const deg = this.degree[d.id] || 0;
    return deg === 0 ? this.STMT_BASE_R : Math.max(4, Math.min(Math.sqrt(deg) * 5, 30));
  }

  getStatusBar() {
    return document.getElementById("status-bar");
  }

  getSliderValue(id) {
    const el = document.getElementById(id);
    return el ? +el.value : 0;
  }

  getCanvasDimensions() {
    return [this.container.clientWidth, this.container.clientHeight];
  }

  // ── Simulation ─────────────────────────────────────────────────────
  linkStrength() {
    const base = this.getSliderValue("sl-attract");
    if (!this.weightedForces || this.netType === "AS") return base;
    const maxW = d3.max(this.DATA.links, l => l.weight || 1) || 1;
    return d => Math.min((d.weight || 1) / maxW, 1) * base;
  }

  buildSim() {
    const [W, H] = this.getCanvasDimensions();
    if (this.sim) this.sim.stop();

    this.sim = d3.forceSimulation(this.DATA.nodes)
      .force("link", d3.forceLink(this.DATA.links)
        .id(d => d.id)
        .distance(this.getSliderValue("sl-edgelen"))
        .strength(this.linkStrength.bind(this)))
      .force("charge", d3.forceManyBody().strength(-this.getSliderValue("sl-spread")))
      .force("center", d3.forceCenter(W / 2, H / 2))
      .force("collide", d3.forceCollide(d => this.nodeR(d, this.sizeByDeg) + 2))
      .force("aspect", forceHorizontalBias(0.9))   // <-- add here
      .on("tick", () => this.onTick())
      .on("end", () => {
        const statusBar = this.getStatusBar();
        if (statusBar) statusBar.textContent = "Converged.";
      });

    const statusBar = this.getStatusBar();
    if (statusBar) statusBar.textContent = "Simulating…";
  }
  onTick() {
    if (!this.linkSel || !this.nodeSel) return;
    this.linkSel
      .attr("x1", d => d.source.x).attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x).attr("y2", d => d.target.y);
    this.nodeSel.attr("transform", d => `translate(${d.x},${d.y})`);
  }

  // ── Drawing ────────────────────────────────────────────────────────
  updateLegend() {
    const nAgents = this.DATA.nodes.filter(n => n.type === "agent").length;
    const nStmts = this.DATA.nodes.filter(n => n.type === "statement").length;
    
    const agentEl = document.getElementById("count-agents");
    const stmtEl = document.getElementById("count-stmts");
    if (agentEl) agentEl.textContent = nAgents;
    if (stmtEl) stmtEl.textContent = nStmts;

    const legendAgents = document.getElementById("legend-agents");
    const legendStmts = document.getElementById("legend-stmts");
    if (legendAgents) legendAgents.style.display = this.netType === "SS" ? "none" : "";
    if (legendStmts) legendStmts.style.display = this.netType === "AA" ? "none" : "";
  }

  redrawLinks() {
    this.linkGroup.selectAll("line").remove();
    this.linkSel = this.linkGroup.selectAll("line")
      .data(this.DATA.links)
      .join("line")
      .attr("class", "edge")
      .style("stroke-width", d => {
        if (this.netType !== "AS" && d.weight != null) {
          return Math.max(0.5, Math.min(Math.sqrt(d.weight) * 1.5, 12)) + "px";
        }
        return null;
      });
  }

  redraw() {
    this.redrawLinks();
    this.nodeGroup.selectAll("g").remove();

    this.nodeSel = this.nodeGroup.selectAll("g")
      .data(this.DATA.nodes, d => d.id)
      .join("g")
      .attr("class", "node")
      .call(d3.drag()
        .on("start", (e, d) => {
          if (!this.frozen) {
            d._didDrag = false;
            d.fx = d.x;
            d.fy = d.y;
          }
        })
        .on("drag", (e, d) => {
          if (!this.frozen) {
            if (!d._didDrag) {
              this.sim.alphaTarget(0.3).restart();
              d._didDrag = true;
            }
            d.fx = e.x;
            d.fy = e.y;
          }
        })
        .on("end", (e, d) => {
          if (!this.frozen) {
            if (!e.active) this.sim.alphaTarget(0);
            d.fx = null;
            d.fy = null;
            delete d._didDrag;
          }
        })
      )
      .on("mouseover", (e, d) => this.onHover(e, d))
      .on("mousemove", (e) => this.onMove(e))
      .on("mouseout", () => this.onOut())
      .on("click", (e, d) => this.onClick(e, d));

    this.circles = this.nodeSel.append("circle")
      .attr("class", "node__circle")
      .attr("r", d => this.nodeR(d, this.sizeByDeg))
      .attr("fill", d => d.type === "agent" ? "var(--color-agent)" : "var(--color-stmt)")
      .attr("stroke", d => d.type === "agent" ? "var(--color-agent-stroke)" : "var(--color-stmt-stroke)");

    this.nodeSel.append("text")
      .attr("class", "node__label")
      .attr("dy", d => d.type === "agent" ? -8 : -10)
      .attr("text-anchor", "middle")
      .html(d => {
        if (d.type === "statement") {
          return `<tspan class="node__label-id">${d.id}</tspan> ${d.label}`;
        }
        return d.label;
      });

    this.nodeSel.classed("node--labels-hidden", !this.labelsOn);
    this.updateLegend();
  }

  // ── Hover & Interaction ────────────────────────────────────────────
  onHover(event, d) {
    const nb = new Set([d.id, ...(this.neighbours[d.id] || [])]);
    this.linkSel.classed("edge--highlighted", l => l._s === d.id || l._t === d.id);
    this.linkSel.filter(function() { return d3.select(this).classed("edge--highlighted"); }).raise();
    this.nodeSel.classed("node--highlighted", n => nb.has(n.id))
      .classed("node--dimmed", n => !nb.has(n.id));

    const cursorTip = document.getElementById("cursor-tooltip");
    if (!cursorTip) return;

    if (d.type === "agent") {
      const count = (this.neighbours[d.id] || new Set()).size;
      cursorTip.textContent = d.label + " · " + count + " link" + (count !== 1 ? "s" : "");
    } else {
      const deg = (this.neighbours[d.id] || new Set()).size;
      cursorTip.innerHTML = `<span class="cursor-tip-id">${d.id}</span> ${d.label}${d.topic ? " · " + d.topic : ""} · ${deg} link${deg !== 1 ? "s" : ""}`;
      const worldviewList = document.getElementById("worldview-list");
      if (worldviewList) {
        worldviewList.querySelectorAll(".worldview-pane__item").forEach(el =>
          el.classList.toggle("worldview-pane__item--highlighted", el.dataset.id === d.id)
        );
      }
    }
    cursorTip.style.display = "block";
  }

  onMove(event) {
    const cursorTip = document.getElementById("cursor-tooltip");
    if (!cursorTip) return;

    const r = this.container.getBoundingClientRect();
    const tw = cursorTip.offsetWidth;
    const th = cursorTip.offsetHeight;
    let x = event.clientX - r.left + 16;
    let y = event.clientY - r.top - 10;
    if (x + tw > this.container.clientWidth - 10) x -= tw + 32;
    if (y + th > this.container.clientHeight - 10) y = this.container.clientHeight - th - 10;
    cursorTip.style.left = x + "px";
    cursorTip.style.top = y + "px";
  }

  onOut() {
    const cursorTip = document.getElementById("cursor-tooltip");
    if (cursorTip) cursorTip.style.display = "none";
    if (this.linkSel) this.linkSel.classed("edge--highlighted", false);
    if (this.nodeSel) this.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
    const worldviewList = document.getElementById("worldview-list");
    if (worldviewList) {
      worldviewList.querySelectorAll(".worldview-pane__item").forEach(el =>
        el.classList.remove("worldview-pane__item--highlighted")
      );
    }
  }

  openWorldview(d) {
    const stmts = Array.from(this.neighbours[d.id] || [])
      .map(id => this.nodeById[id])
      .filter(n => n && n.type === "statement")
      .sort((a, b) => a.id.localeCompare(b.id, undefined, { numeric: true }));

    const worldviewTitle = document.getElementById("worldview-title");
    const worldviewList = document.getElementById("worldview-list");
    const worldviewPane = document.getElementById("worldview-pane");

    if (!worldviewTitle || !worldviewList || !worldviewPane) return;

    worldviewTitle.textContent = d.label + "'s Worldview";
    worldviewList.innerHTML = stmts.length === 0
      ? '<div class="worldview-pane__empty">No connections</div>'
      : stmts.map(n => {
        const sub = n.topic ? ` <span class="worldview-pane__item-sub">${n.topic}</span>` : '';
        // return `<div class="worldview-pane__item" data-id="${n.id}"><span class="worldview-pane__item-id">${n.id}</span><span class="worldview-pane__item-text">${n.label}${sub}</span></div>`;
        return `<div class="worldview-pane__item" data-id="${n.id}"><span class="worldview-pane__item-id">${n.id}</span><span class="worldview-pane__item-text">${n.label}${sub}</span></div>`;
      }).join("");

    // Hover over agent title
    worldviewTitle.onmouseenter = () => {
      const nb = new Set([d.id, ...(this.neighbours[d.id] || [])]);
      this.nodeSel.classed("node--highlighted", n => nb.has(n.id))
        .classed("node--dimmed", n => !nb.has(n.id));
      this.linkSel.classed("edge--highlighted", l => l._s === d.id || l._t === d.id);
      this.linkSel.filter(function() { return d3.select(this).classed("edge--highlighted"); }).raise();
    };

    worldviewTitle.onmouseleave = () => {
      this.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
      this.linkSel.classed("edge--highlighted", false);
    };

    // Hover over items
    worldviewList.querySelectorAll(".worldview-pane__item").forEach(el => {
      el.onmouseenter = () => {
        const sid = el.dataset.id;
        this.nodeSel.classed("node--highlighted", n => n.id === sid || n.id === d.id)
          .classed("node--dimmed", n => n.id !== sid && n.id !== d.id);
        this.linkSel.classed("edge--highlighted", l =>
          (l._s === d.id && l._t === sid) || (l._t === d.id && l._s === sid));
        this.linkSel.filter(function() { return d3.select(this).classed("edge--highlighted"); }).raise();
      };
      el.onmouseleave = () => {
        this.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
        this.linkSel.classed("edge--highlighted", false);
      };
      const idSpan = el.querySelector(".worldview-pane__item-id");
      if (idSpan) {
        idSpan.onclick = (event) => {
          event.stopPropagation();
          const stmtNode = this.nodeById[el.dataset.id];
          if (stmtNode) {
            worldviewPane.classList.remove("worldview-pane--visible");
            this.openStatementPane(stmtNode);
          }
        };
      }
    });

    worldviewPane.classList.add("worldview-pane--visible");
  }

  openStatementPane(d) {
    const agents = Array.from(this.neighbours[d.id] || [])
      .map(id => this.nodeById[id])
      .filter(n => n && n.type === "agent")
      .sort((a, b) => a.id.localeCompare(b.id, undefined, { numeric: true }));

    const titleEl = document.getElementById("statement-title");
    const statementList = document.getElementById("statement-list");
    const statementPane = document.getElementById("statement-pane");

    if (!titleEl || !statementList || !statementPane) return;

    titleEl.innerHTML = `<span class="statement-id">${d.id}</span> <span class="statement-text">${d.label}</span>`;
    const countInfo = agents.length === 0
      ? '<div class="statement-agents-info">0 agents connected</div>'
      : `<div class="statement-agents-info">${agents.length} agents connected</div>`;

    statementList.innerHTML = countInfo + (agents.length === 0
      ? '<div class="worldview-pane__empty">No connections</div>'
      : `<div class="statement-agents-list">${agents.map(n =>
        `<span class="agent-id" data-agent-id="${n.id}">${n.id}</span>`
      ).join('')}</div>`);

    titleEl.onmouseenter = () => {
      const nb = new Set([d.id, ...(this.neighbours[d.id] || [])]);
      this.nodeSel.classed("node--highlighted", n => nb.has(n.id))
        .classed("node--dimmed", n => !nb.has(n.id));
      this.linkSel.classed("edge--highlighted", l => l._s === d.id || l._t === d.id);
      this.linkSel.filter(function() { return d3.select(this).classed("edge--highlighted"); }).raise();
    };

    titleEl.onmouseleave = () => {
      this.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
      this.linkSel.classed("edge--highlighted", false);
    };

    statementList.querySelectorAll(".agent-id").forEach(el => {
      el.onmouseenter = () => {
        const agentId = el.dataset.agentId;
        this.nodeSel.classed("node--highlighted", n => n.id === agentId || n.id === d.id)
          .classed("node--dimmed", n => n.id !== agentId && n.id !== d.id);
        this.linkSel.classed("edge--highlighted", l =>
          (l._s === d.id && l._t === agentId) || (l._t === d.id && l._s === agentId));
        this.linkSel.filter(function() { return d3.select(this).classed("edge--highlighted"); }).raise();
      };
      el.onmouseleave = () => {
        this.nodeSel.classed("node--highlighted", false).classed("node--dimmed", false);
        this.linkSel.classed("edge--highlighted", false);
      };
      el.onclick = (event) => {
        event.stopPropagation();
        const agentNode = this.nodeById[el.dataset.agentId];
        if (agentNode) {
          statementPane.classList.remove("worldview-pane--visible");
          this.openWorldview(agentNode);
        }
      };
    });

    statementPane.classList.add("worldview-pane--visible");
  }

  onClick(event, d) {
    event.stopPropagation();
    if (this.netType !== "AS") return;
    if (d.type === "agent") {
      const stmtPane = document.getElementById("statement-pane");
      if (stmtPane) stmtPane.classList.remove("worldview-pane--visible");
      this.openWorldview(d);
    } else {
      const worldviewPane = document.getElementById("worldview-pane");
      if (worldviewPane) worldviewPane.classList.remove("worldview-pane--visible");
      this.openStatementPane(d);
    }
  }

  // ── Data Management ────────────────────────────────────────────────
  loadData(newData, sourceEntry) {
    this.rawData = newData;
    const filename = sourceEntry ? sourceEntry.file.split("/").pop() : "data.json";
    const loadedName = document.getElementById("loaded-name");
    if (loadedName) loadedName.textContent = filename.replace(/\.json$/i, "");

    const worldviewPane = document.getElementById("worldview-pane");
    const statementPane = document.getElementById("statement-pane");
    if (worldviewPane) worldviewPane.classList.remove("worldview-pane--visible");
    if (statementPane) statementPane.classList.remove("worldview-pane--visible");

    this.frozen = false;
    const btnFreeze = document.getElementById("btn-freeze");
    if (btnFreeze) btnFreeze.textContent = "⏸ Freeze";

    const netTypeSelect = document.getElementById("filter-nettype");
    if (netTypeSelect) netTypeSelect.value = "AS";

    if (sourceEntry) this.syncFiltersToEntry(sourceEntry);
    this.updateNetworkType("AS");
  }

  syncFiltersToEntry(entry) {
    const fields = {
      "filter-N": "N",
      "filter-M": "M",
      "filter-openmindedness": "openmindedness",
      "filter-seed": "seed",
      "filter-t": "t",
      "filter-post": "post",
      "filter-run": "timestamp"
    };
    Object.entries(fields).forEach(([id, key]) => {
      const el = document.getElementById(id);
      if (el && entry[key] !== undefined) el.value = entry[key];
    });
  }

  updateNetworkType(type) {
    this.netType = type;

    const wfBtn = document.getElementById("btn-weight-forces");
    const mwSlider = document.getElementById("sl-minweight");
    const mwLabel = document.getElementById("vl-minweight");

    this.minWeight = 1;
    if (mwSlider) mwSlider.value = 1;
    if (mwLabel) mwLabel.textContent = "1";

    if (this.netType === "AS") {
      this.weightedForces = false;
      if (wfBtn) {
        wfBtn.disabled = true;
        wfBtn.classList.remove("btn--active");
        wfBtn.textContent = "Weight forces: off";
      }
      if (mwSlider) mwSlider.disabled = true;
      this.DATA.nodes = this.rawData.nodes ? this.rawData.nodes.slice() : [];
      this.DATA.links = this.rawData.links ? this.rawData.links.filter(l => !l.type || l.type === "AS") : [];
    } else if (this.netType === "AA") {
      if (wfBtn) wfBtn.disabled = false;
      if (mwSlider) mwSlider.disabled = false;
      this.DATA.nodes = this.rawData.nodes ? this.rawData.nodes.filter(n => n.type === "agent") : [];
      this.DATA.links = this.rawData.links ? this.rawData.links.filter(l => l.type === "AA" && (l.weight || 1) >= this.minWeight) : [];
    } else { // SS
      if (wfBtn) wfBtn.disabled = false;
      if (mwSlider) mwSlider.disabled = false;
      this.DATA.nodes = this.rawData.nodes ? this.rawData.nodes.filter(n => n.type === "statement") : [];
      this.DATA.links = this.rawData.links ? this.rawData.links.filter(l => l.type === "SS" && (l.weight || 1) >= this.minWeight) : [];
    }

    if (this.netType !== "AS") {
      const worldviewPane = document.getElementById("worldview-pane");
      const statementPane = document.getElementById("statement-pane");
      if (worldviewPane) worldviewPane.classList.remove("worldview-pane--visible");
      if (statementPane) statementPane.classList.remove("worldview-pane--visible");
    }

    this.setSliders(this.NET_DEFAULTS[this.netType]);
    this.tagLinks();
    this.buildIndex();
    this.DATA.nodes.forEach(d => {
      d.x = undefined;
      d.y = undefined;
      d.vx = 0;
      d.vy = 0;
      d.fx = null;
      d.fy = null;
    });
    this.redraw();
    this.buildSim();
    this.sim.alpha(1).restart();
  }

  setSliders(config) {
    const sliders = [
      ["sl-spread", "vl-spread", config.spread],
      ["sl-edgelen", "vl-edgelen", config.edgelen],
      ["sl-attract", "vl-attract", config.attract]
    ];
    sliders.forEach(([slId, vlId, val]) => {
      const sl = document.getElementById(slId);
      const vl = document.getElementById(vlId);
      if (sl) sl.value = val;
      if (vl) {
        if (vlId === "vl-attract") {
          vl.textContent = val.toFixed(3).replace(/\.?0+$/, "");
        } else {
          vl.textContent = val;
        }
      }
    });
  }

  applyWeightFilter() {
    this.minWeight = this.getSliderValue("sl-minweight");
    const typeFilter = this.netType === "AA" ? l => l.type === "AA" : l => l.type === "SS";
    this.DATA.links = (this.rawData.links || [])
      .filter(typeFilter)
      .filter(l => (l.weight || 1) >= this.minWeight);
    this.tagLinks();
    this.buildIndex();
    this.redrawLinks();
    this.sim.force("link", d3.forceLink(this.DATA.links)
      .id(d => d.id)
      .distance(this.getSliderValue("sl-edgelen"))
      .strength(this.linkStrength.bind(this)));
    this.sim.alpha(0.3).restart();
  }

  // ── Public API ─────────────────────────────────────────────────────
  getState() {
    return {
      frozen: this.frozen,
      labelsOn: this.labelsOn,
      sizeByDeg: this.sizeByDeg,
      netType: this.netType
    };
  }

  setState(state) {
    if (state.frozen !== undefined) this.frozen = state.frozen;
    if (state.labelsOn !== undefined) this.labelsOn = state.labelsOn;
    if (state.sizeByDeg !== undefined) this.sizeByDeg = state.sizeByDeg;
    if (state.netType !== undefined) this.updateNetworkType(state.netType);
  }

  rebuild() {
    this.redraw();
    this.buildSim();
  }

  onResize() {
    if (!this.frozen && this.sim) {
      const [W, H] = this.getCanvasDimensions();
      this.sim.force("center", d3.forceCenter(W / 2, H / 2));
      this.sim.alpha(0.2).restart();
    }
  }
}
