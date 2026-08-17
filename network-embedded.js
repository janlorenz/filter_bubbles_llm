/**
 * Network Embedded: Simplified initialization for embedding networks in figures/presentations.
 * 
 * Provides a single function `initEmbedded()` that loads a network with minimal UI
 * (zoom controls only, no filters or sliders).
 * 
 * Requires: NetworkViz class from network-core.js
 */

/**
 * Initialize an embedded network visualization with minimal controls.
 * 
 * @param {string|HTMLElement} containerId - ID of canvas-wrapper element or element itself
 * @param {Object} config - Configuration object
 * @param {string} config.file - Path to network JSON file
 * @param {string} [config.type="AS"] - Network type: "AS", "AA", or "SS"
 * @param {number} [config.zoom=1] - Initial zoom multiplier (e.g., 1.3)
 * @param {Object} [config.layout] - Layout parameters
 * @param {number} [config.layout.spread] - Spread (repulsion) force
 * @param {number} [config.layout.edgelen] - Edge length (spring rest distance)
 * @param {number} [config.layout.attract] - Attraction force strength
 */


function initEmbedded(containerId, config) {
  // Resolve container
  const container = typeof containerId === 'string'
    ? document.getElementById(containerId)
    : containerId;

  if (!container) {
    console.error(`Embedded network: container not found (${containerId})`);
    return;
  }

  // Create NetworkViz instance
  let viz;
  try {
    viz = new NetworkViz(containerId);
  } catch (err) {
    console.error(`Failed to initialize NetworkViz: ${err.message}`);
    return;
  }

  // Default config
  const cfg = {
    file: config.file,
    type: config.type || "AS",
    zoom: config.zoom || 1,
    layout: config.layout || { spread: 100, edgelen: 10, attract: 0.75 }
  };

  if (!cfg.file) {
    console.error("Embedded network: config.file is required");
    return;
  }

  // Helper: find element inside container first, then globally
  function findEl(id) {
    return container.querySelector('#' + id) || document.getElementById(id);
  }

  // Wire zoom buttons (normally done by network-controls.js, not loaded in embedded mode)
  const btnZoomIn = findEl('btn-zoom-in');
  if (btnZoomIn) {
    btnZoomIn.addEventListener('click', () => {
      viz.svg.transition().duration(300).call(viz.zoomBehavior.scaleBy, 1.3);
    });
  }

  const btnZoomOut = findEl('btn-zoom-out');
  if (btnZoomOut) {
    btnZoomOut.addEventListener('click', () => {
      viz.svg.transition().duration(300).call(viz.zoomBehavior.scaleBy, 1 / 1.3);
    });
  }

  // Wire pane close buttons
  const worldviewClose = findEl('worldview-close');
  if (worldviewClose) {
    worldviewClose.addEventListener('click', e => {
      e.stopPropagation();
      const pane = findEl('worldview-pane');
      if (pane) pane.classList.remove('worldview-pane--visible');
    });
  }

  const statementClose = findEl('statement-close');
  if (statementClose) {
    statementClose.addEventListener('click', e => {
      e.stopPropagation();
      const pane = findEl('statement-pane');
      if (pane) pane.classList.remove('worldview-pane--visible');
    });
  }

  // Status update
  const statusBar = findEl("status-bar");
  if (statusBar) statusBar.textContent = "Loading…";

  // Fetch and load network
  fetch(cfg.file)
    .then(r => {
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      return r.json();
    })
    .then(data => {
      // Load data and build visualization
      viz.loadData(data, null);

      // updateNetworkType builds the sim but reads slider values via getSliderValue(),
      // which returns 0 when sliders are absent in embedded mode.
      // Call it first to get nodes/links indexed and drawn, then override forces directly.
      viz.updateNetworkType(cfg.type);

      // Apply configured layout forces directly, bypassing missing slider elements
      if (viz.sim && cfg.layout) {
        const { spread, edgelen, attract } = cfg.layout;
        viz.sim.force("charge", d3.forceManyBody().strength(-spread));
        viz.sim.force("link", d3.forceLink(viz.DATA.links)
            .id(d => d.id)
            .distance(edgelen)
            .strength(attract));
        viz.sim.force("aspect", forceHorizontalBias(0.08)); // <-- cheap bias
        viz.sim.alpha(1).restart();
      }

      // Apply initial zoom if requested
      if (cfg.zoom !== 1 && viz.svg) {
        viz.svg.transition().duration(750).call(viz.zoomBehavior.scaleBy, cfg.zoom);
      }

      if (statusBar) statusBar.textContent = "Ready";
    })
    .catch(err => {
      if (statusBar) statusBar.textContent = `Failed: ${err.message}`;
      console.error(`Failed to load network: ${err.message}`);
    });
}
