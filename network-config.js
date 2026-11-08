/**
 * Network Explorer configuration.
 *
 * This is the single place to edit when you add a model, a filter control,
 * a node attribute to color by, or a column to the simulation overview.
 * No other file needs to change for these cases.
 */
const NETWORK_CONFIG = {

  // Manifests are concatenated. Every entry needs at least: file, model, timestamp.
  manifests: ["manifest.json", "manifest_jev.json"],
 
  // Preferred starting selection. Values that don't exist in the manifest
  // fall back to the first available option (see NetworkExplorer.refresh).
  initial: { model: "gpt", N: 100, M: 5, openmindedness: 1, post: "FS", seed: 354, t: 100 },

  // Filter dropdowns, in cascade order: each filter's options only list values
  // that exist given all filters above it.
  //   key     manifest field
  //   label   text next to the dropdown
  //   models  only shown for (and applied to) these models; omit = all models
  //   labels  optional value -> display text
  filters: [
    { key: "model", label: "Model", labels: { gpt: "GPT (LLM)", jev: "Jev (numeric)" } },
    { key: "N", label: "N" },
    { key: "M", label: "M" },
    { key: "openmindedness", label: "Open", models: ["gpt"] },
    { key: "probability_acceptance_threshold", label: "Accept thr.", models: ["jev"] },
    { key: "post", label: "Post", labels: { F: "Feed", S: "Social", FS: "Both", Ffix: "Feed (fixed)" } },
    { key: "seed", label: "Seed" },
    { key: "timestamp", label: "Run" },
    { key: "t", label: "t" },
    // Example for later: { key: "tmax", label: "tmax" },
  ],

  // Node attributes that are derived in the browser (not stored in the JSONs).
  //   value(node, viz) -> number | string
  computedFields: {
    degree: { label: "Degree", types: ["agent", "statement"], value: (d, viz) => viz.degree[d.id] || 0 },
  },

  // Optional styling for node attributes, whether stored in the JSON or computed.
  // Fields without an entry are auto-detected: numbers -> sequential Viridis,
  // anything else -> categorical Tableau10.
  //   kind    "numeric" | "categorical" (override auto-detection)
  //   scheme  d3 interpolator name without "interpolate" (numeric), e.g. "RdBu"
  //   domain  [min, max] (numeric)
  //   colors  { value: cssColor } (categorical)
  //   label   text in the dropdown
  fieldStyles: {
    // opinion: { kind: "numeric", scheme: "RdBu", domain: [-1, 1], label: "Opinion" },
    // group:   { kind: "categorical", colors: { A: "#0077b6", B: "#d81b60" } },
  },

  // Node JSON keys that are never offered as color fields.
  reservedNodeKeys: ["id", "label", "type", "index", "x", "y", "vx", "vy", "fx", "fy"],

  // Columns of the simulation overview table. `sim` is the first manifest
  // entry of a simulation plus `sim.ts` (sorted list of available t).
  overviewColumns: [
    { label: "Model", value: s => s.model },
    { label: "Run", value: s => s.timestamp },
    { label: "N", value: s => s.N, numeric: true },
    { label: "M", value: s => s.M, numeric: true },
    { label: "tmax", value: s => s.tmax, numeric: true },
    { label: "Open / Accept thr.", numeric: true,
      value: s => s.model === "jev" ? s.probability_acceptance_threshold : s.openmindedness },
    { label: "Post", value: s => s.post },
    { label: "Seed", value: s => s.seed, numeric: true },
    { label: "LLM", value: s => s.model_name || "" },
  ],
};
