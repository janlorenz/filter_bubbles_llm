/**
 * NetworkExplorer: config-driven filters and color controls for the joint
 * (multi-model) explorer page.
 *
 * Needs: NETWORK_CONFIG (network-config.js), NetworkViz (network-core.js) and
 * initInteractions (network-controls.js). Expected DOM placeholders:
 *   #filters-row       receives the filter dropdowns
 *   #color-row         receives the color-by dropdowns
 *   #filter-status     (optional) text status
 *
 * Filters cascade in config order: each dropdown only lists values that exist
 * given the filters above it, so every combination resolves to a file.
 * Filters whose `models` list excludes the selected model are hidden and
 * ignored; their last value is remembered for when the model is switched back.
 */
class NetworkExplorer {
  constructor(viz, config = NETWORK_CONFIG) {
    this.viz = viz;
    this.config = config;
    this.entries = [];
    this.state = { ...config.initial };
    this.currentEntry = null;
    this.selects = {};        // filter key -> <select>
    this.groups = {};         // filter key -> wrapper element
    this.colorSelects = {};   // node type -> <select>

    viz.computedFields = config.computedFields || {};
    viz.fieldStyles = config.fieldStyles || {};
    if (config.reservedNodeKeys) viz.reservedNodeKeys = config.reservedNodeKeys;
  }

  // ── Setup ──────────────────────────────────────────────────────────
  async init() {
    initInteractions(this.viz);
    this.buildFilterControls();
    this.buildColorControls();
    window.addEventListener("network:loaded", e => this.onLoaded(e.detail.entry));

    const lists = await Promise.all(this.config.manifests.map(path =>
      fetch(path)
        .then(r => { if (!r.ok) throw new Error(`${path}: HTTP ${r.status}`); return r.json(); })
        .catch(err => { this.setStatus(`Manifest failed: ${err.message}`, true); return []; })
    ));
    this.entries = lists.flat();
    if (!this.entries.length) { this.setStatus("No manifest entries.", true); return this; }

    const requested = new URLSearchParams(location.search).get("net");
    const match = requested && this.entries.find(e => this.baseName(e) === requested);
    if (match) this.state = { ...this.state, ...this.stateFromEntry(match) };

    this.refresh();
    return this;
  }

  baseName(entry) { return entry.file.split("/").pop().replace(/\.json$/i, ""); }

  stateFromEntry(entry) {
    const s = {};
    this.config.filters.forEach(f => { if (entry[f.key] !== undefined) s[f.key] = entry[f.key]; });
    return s;
  }

  buildFilterControls() {
    const host = document.getElementById("filters-row");
    this.config.filters.forEach(f => {
      const group = document.createElement("div");
      group.className = "filter-group";
      const label = document.createElement("label");
      label.className = "filter-label";
      label.textContent = f.label + ":";
      label.htmlFor = "filter-" + f.key;
      const sel = document.createElement("select");
      sel.className = "filter-select";
      sel.id = "filter-" + f.key;
      sel.addEventListener("change", () => {
        this.state[f.key] = sel.selectedOptions[0].dataset.raw;
        this.refresh();
      });
      group.append(label, sel);
      host.appendChild(group);
      this.selects[f.key] = sel;
      this.groups[f.key] = group;
    });
  }

  buildColorControls() {
    const host = document.getElementById("color-row");
    if (!host) return;
    [["agent", "Agents"], ["statement", "Statements"]].forEach(([type, text]) => {
      const group = document.createElement("div");
      group.className = "filter-group";
      const label = document.createElement("label");
      label.className = "filter-label";
      label.textContent = text + ":";
      const sel = document.createElement("select");
      sel.className = "filter-select";
      label.htmlFor = sel.id = "color-" + type;
      sel.addEventListener("change", () => this.viz.setColorBy(type, sel.value || null));
      group.append(label, sel);
      host.appendChild(group);
      this.colorSelects[type] = sel;
    });
  }

  // ── Filter cascade ─────────────────────────────────────────────────
  applies(f) { return !f.models || f.models.includes(this.state.model); }

  /** Recompute options of every visible filter, then load the resolved file. */
  refresh() {
    let pool = this.entries;
    this.config.filters.forEach(f => {
      const group = this.groups[f.key];
      const visible = this.applies(f);
      group.hidden = !visible;
      if (!visible) return;

      const options = this.uniqueSorted(pool.map(e => e[f.key]));
      const wanted = options.find(v => String(v) === String(this.state[f.key]));
      const value = wanted !== undefined ? wanted : options[0];
      this.state[f.key] = value;
      this.fillSelect(this.selects[f.key], options, value, f.labels);
      pool = pool.filter(e => String(e[f.key]) === String(value));
    });

    const entry = pool[0] || null;
    this.setStatus(entry
      ? `${this.entries.filter(e => e.model === this.state.model).length} snapshots for this model`
      : "No file found", !entry);
    if (entry && (!this.currentEntry || entry.file !== this.currentEntry.file)) this.load(entry);
  }

  uniqueSorted(values) {
    const u = [...new Set(values.filter(v => v !== undefined && v !== null))];
    return u.every(v => typeof v === "number")
      ? u.sort((a, b) => a - b)
      : u.sort((a, b) => String(a).localeCompare(String(b), undefined, { numeric: true }));
  }

  fillSelect(sel, options, value, labels = {}) {
    sel.innerHTML = "";
    options.forEach(v => {
      const o = document.createElement("option");
      o.textContent = labels[v] !== undefined ? labels[v] : v;
      o.value = v;
      o.dataset.raw = v;
      o.selected = String(v) === String(value);
      sel.appendChild(o);
    });
  }

  load(entry) {
    this.currentEntry = entry;
    const bar = document.getElementById("status-bar");
    if (bar) bar.textContent = "Loading…";
    fetch(entry.file)
      .then(r => { if (!r.ok) throw new Error(`HTTP ${r.status}`); return r.json(); })
      .then(data => this.viz.loadData(data, entry))
      .catch(err => { if (bar) bar.textContent = `Failed to load: ${err.message}`; });
  }

  /** Public: jump to a snapshot, e.g. from the simulation overview. */
  select(entry) {
    this.state = { ...this.state, ...this.stateFromEntry(entry) };
    this.refresh();
  }

  setStatus(text, isError = false) {
    const el = document.getElementById("filter-status");
    if (!el) return;
    el.textContent = text;
    el.classList.toggle("filter-status--error", isError);
  }

  // ── Coloring ───────────────────────────────────────────────────────
  /** After each load: refresh color field options; drop selections that no longer exist. */
  onLoaded() {
    Object.entries(this.colorSelects).forEach(([type, sel]) => {
      const fields = this.viz.nodeFields(type);
      const current = this.viz.colorBy[type];
      const keep = fields.some(f => f.key === current) ? current : null;
      sel.innerHTML = "";
      sel.appendChild(new Option("default", ""));
      fields.forEach(f => sel.appendChild(new Option(f.label, f.key)));
      sel.value = keep || "";
      if (keep !== current) this.viz.setColorBy(type, keep);
    });
  }
}
