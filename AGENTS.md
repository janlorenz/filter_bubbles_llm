# Filter Bubbles LLM — Project Memory

## Recent Changes (August 2026)

**REFACTORED network explorer for modularity:**
- Created `network-core.js`: Core visualization logic (NetworkViz class)
- Created `network-controls.js`: Interactive controls (filters, buttons, sliders, manifest loading)
- Created `network-embedded.js`: Minimal embedding interface for figures/presentations
- Refactored `networkexplorer.qmd`: Now uses three external JS modules instead of 800+ lines of inline code
- Updated `network-scoped.css`: Added `.network-mode--embedded` class for minimal UI
- Created `network-figure.qmd`: Template for embedding networks in presentations/reports
- Updated `presentation.qmd`: Added example slide with embedded network

**Benefits:**
- Code is now modular and reusable
- Networks can easily be embedded in reveal.js slides or HTML figures
- Reduced networkexplorer.qmd from 999 lines to ~200 lines
- No breaking changes: explorer works exactly as before

## Research Overview

Agent-based model of opinion dynamics using LLMs. Agents hold a **worldview** (a list of up to `M` textual statements from the World Values Survey). In each time step, agents receive new statements and use an LLM prompt to decide whether to integrate or reject them (optionally dropping an existing statement). The research question: do **filter bubbles** emerge from this process, analogous to numerical opinion dynamics models?

Key parameters per run:
- `N` — number of agents (100 in all runs so far)
- `M` — max statements per agent (max memory)
- `openmindedness` — 0–10 scale controlling acceptance rate (appears in LLM prompt)
- `t` the time step of a simulation
- `tmax` — total number of time steps in a simulation (only used to identify simulation runs)
- `social_posting` — whether agents share statements with friends (drives bubble formation)
- `model_name` — LLM used (e.g. `google/gemini-2.0-flash-001`, `openai/gpt-oss-20b`)
- `seed` — random seed

## File Structure

```
rawdata/experiments/<run_folder>/   # Raw CSVs per simulation run
  decisions.csv
  memory.csv
  social_network.csv
  statements.csv
runs_config.csv                     # Index of all runs (also at rawdata/experiments/)
parquet/                            # Cleaned/merged parquet files (from data_preparation.r)
  decisions.parquet
  memory.parquet
  social_network.parquet
  statements.parquet
networkjson/                        # D3-ready JSON files for network explorer
manifest.json                       # Index of available network JSONs for the explorer UI
wvs_statements.csv / .parquet       # 100 World Values Survey statements used as the statement pool
```

## Data Schemas

**`runs_config.csv`** (212 rows): one row per simulation run. Key columns: `timestamp`, `run_id`, `run_folder`, `num_agents`, `max_memory_per_agent`, `open_mindedness_level`, `num_time_steps`, `social_posting`, `model_name`, `random_seed`.

**`parquet/memory.parquet`** (895,345 rows): agent–statement membership over time.
Columns: `run_id`, `A_id` (int), `S_id` (statement id), `t` (time step).

**`parquet/decisions.parquet`**: LLM integration/drop decisions.
Columns include `run_id`, `A_id`, `t`, `source_statement`, and decision fields.

**`parquet/social_network.parquet`**: friendship edges per run.

**`parquet/statements.parquet`**: statement text per run.
Columns: `run_id`, `S_id`, `text`.

**`networkjson/*.json`**: D3 bipartite graph format.
`{ nodes: [{id, label, type}], links: [{source, target}] }`
- `type` is `"agent"` or `"statement"`
- Filename pattern: `{timestamp}_A{N}_M{M}_tmax{tmax}_op{openmindedness}_seed{seed}_{model}_t{t}.json`

## Key R Objects (data_preparation.r session)

| Variable | Description |
|---|---|
| `runs` | All 212 runs from runs_config.csv |
| `runs_core` | Subset of runs |
| `memory` | Full memory.parquet loaded |
| `statements` | Statement text (distinct, from last core run) |
| `agents` | Agent id/type tibble (A1–A100) |

## Data Preparation Workflow (`data_preparation.r`)

1. Read `rawdata/experiments/runs_config.csv` → `runs`, filter to recent runs → `runs_core`
2. For each run in `runs_core`, read raw CSVs and bind → write to `parquet/`
3. Load `parquet/memory.parquet` and `parquet/statements.parquet`
4. Write json files for each run and some time steps
5. Output goes to `networkjson/` and `manifest.json` is updated to be used for a filter menu in Networf Explorer

## Network Explorer (`networkexplorer.qmd`)

Interactive D3 force-directed visualization of the bipartite agent–statement network. Built as a Quarto HTML page:
- Loads `manifest.json` at runtime to populate filter dropdown menus for available runs
- Supports local JSON upload
- Nodes: agents (small, one color) and statements (larger, another color)
- Edges: agent holds that statement in worldview
- Controls: spread/edge-length/attraction sliders, freeze, labels toggle, size-by-degree
- Side panels: clicking an agent shows its worldview; clicking a statement shows which agents hold it

## Quarto Site

`_quarto.yml` configures the site. Main pages: `index.qmd` (paper draft) and `networkexplorer.qmd`.
Uses `network-scoped.css` for explorer styling and `styles.css` for general styles.
