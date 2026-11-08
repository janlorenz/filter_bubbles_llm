/**
 * SimulationOverview: sortable, searchable table of all simulations in the
 * manifests (one row per simulation, not per time step). Clicking a row (or a
 * t chip) loads it in the NetworkExplorer.
 *
 * Columns are configured in NETWORK_CONFIG.overviewColumns.
 *
 * Usage: SimulationOverview.render("sim-overview", explorer)  // after explorer.init()
 */
const SimulationOverview = (() => {

  const simKey = entry => entry.file.split("/").pop().replace(/_t\d+\.json$/i, "");

  function groupSimulations(entries) {
    const sims = new Map();
    entries.forEach(e => {
      const k = simKey(e);
      if (!sims.has(k)) sims.set(k, { ...e, key: k, byT: new Map() });
      sims.get(k).byT.set(e.t, e);
    });
    sims.forEach(s => { s.ts = [...s.byT.keys()].sort((a, b) => a - b); });
    return [...sims.values()];
  }

  function render(containerId, explorer) {
    const host = document.getElementById(containerId);
    if (!host) return;
    const cols = explorer.config.overviewColumns;
    const sims = groupSimulations(explorer.entries);
    const models = [...new Set(sims.map(s => s.model))].sort();
    const ui = { model: null, query: "", sortCol: null, sortDir: 1 };

    host.classList.add("network-ui", "sim-overview");
    host.innerHTML = "";

    // ── Toolbar ──
    const bar = el("div", "sim-overview__bar");
    const summary = el("span", "sim-overview__summary");
    const search = el("input", "sim-overview__search");
    search.type = "search";
    search.placeholder = "Filter simulations…";
    search.addEventListener("input", () => { ui.query = search.value.toLowerCase(); draw(); });
    const chips = ["All", ...models].map(m => {
      const b = el("button", "sim-overview__chip", m);
      b.type = "button";
      b.addEventListener("click", () => { ui.model = m === "All" ? null : m; draw(); });
      return b;
    });
    bar.append(...chips, search, summary);

    // ── Table ──
    const scroll = el("div", "sim-overview__scroll");
    const table = el("table");
    const thead = el("thead"), tbody = el("tbody");
    const headRow = el("tr");
    cols.forEach((c, i) => {
      const th = el("th", c.numeric ? "is-numeric" : "", c.label);
      th.addEventListener("click", () => {
        ui.sortDir = ui.sortCol === i ? -ui.sortDir : 1;
        ui.sortCol = i;
        draw();
      });
      headRow.appendChild(th);
    });
    headRow.appendChild(el("th", "", "Time steps (click to open)"));
    thead.appendChild(headRow);
    table.append(thead, tbody);
    scroll.appendChild(table);
    host.append(bar, scroll);

    const currentKey = () => explorer.currentEntry ? simKey(explorer.currentEntry) : null;
    const currentT = () => explorer.currentEntry ? explorer.currentEntry.t : null;

    function open(sim, t) {
      const want = t !== undefined ? t : currentT();
      const best = sim.ts.reduce((a, b) => Math.abs(b - want) < Math.abs(a - want) ? b : a);
      explorer.select(sim.byT.get(best));
    }

    function draw() {
      chips.forEach((b, i) => b.setAttribute("aria-pressed", String((i === 0 ? null : models[i - 1]) === ui.model)));
      headRow.querySelectorAll("th").forEach((th, i) =>
        th.dataset.sort = i === ui.sortCol ? (ui.sortDir > 0 ? "asc" : "desc") : "");

      let rows = sims.filter(s =>
        (!ui.model || s.model === ui.model) &&
        (!ui.query || cols.map(c => c.value(s)).join(" ").toLowerCase().includes(ui.query)));
      if (ui.sortCol !== null) {
        const f = cols[ui.sortCol].value;
        rows = rows.slice().sort((a, b) => ui.sortDir * cmp(f(a), f(b)));
      }
      summary.textContent = `${rows.length} of ${sims.length} simulations`;

      const active = currentKey(), activeT = currentT();
      tbody.innerHTML = "";
      rows.forEach(s => {
        const tr = el("tr", s.key === active ? "is-active" : "");
        cols.forEach(c => {
          const v = c.value(s);
          tr.appendChild(el("td", c.numeric ? "is-numeric" : "", v === undefined || v === null ? "" : v));
        });
        const tdT = el("td");
        s.ts.forEach(t => {
          const chip = el("span", "sim-overview__t" + (s.key === active && t === activeT ? " is-active" : ""), t);
          chip.addEventListener("click", e => { e.stopPropagation(); open(s, t); scrollToExplorer(); });
          tdT.appendChild(chip);
        });
        tr.appendChild(tdT);
        tr.addEventListener("click", () => { open(s); scrollToExplorer(); });
        tbody.appendChild(tr);
      });
    }

    window.addEventListener("network:loaded", draw);
    draw();
  }

  function scrollToExplorer() {
    const root = document.getElementById("network-root");
    if (root) root.scrollIntoView({ behavior: "smooth", block: "start" });
  }

  function cmp(a, b) {
    if (a === b) return 0;
    if (a === undefined || a === null || a === "") return 1;
    if (b === undefined || b === null || b === "") return -1;
    return typeof a === "number" && typeof b === "number"
      ? a - b : String(a).localeCompare(String(b), undefined, { numeric: true });
  }

  function el(tag, cls = "", text) {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    return e;
  }

  return { render, groupSimulations };
})();
