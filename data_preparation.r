library(tidyverse)
library(jsonlite)
library(arrow)
library(glue)

runs <- read_csv("rawdata/experiments/runs_config.csv") |>
  select(
    timestamp,
    seed = random_seed,
    N = num_agents,
    M = max_memory_per_agent,
    openmindedness = open_mindedness_level,
    tmax = num_time_steps,
    social_posting,
    model_name,
    run_id,
    run_folder
  ) |>
  mutate(
    model_short = word(model_name, sep = "/", start = 2) |>
      word(sep = "-", start = 1),
    timestamp = str_sub(timestamp, 3, 13),
    datetime = as.POSIXct(timestamp, format = "%y%m%d-%H%M", tz = "UTC"),
    run_id_desc = glue(
      "{timestamp}_A{N}_M{M}_tmax{tmax}_op{openmindedness}_seed{seed}_{model_short}"
    )
  )
runs_core <- runs |>
  filter(datetime >= "2026-03-25", model_short == "gpt") |>
  select(
    run_id_desc,
    run_id,
    N,
    M,
    openmindedness,
    social_posting,
    seed,
    timestamp,
    model_name,
    run_folder,
    datetime
  )
runs_core |> write_parquet("parquet/runs_core.parquet")

# Make csvs of decisions, memory, and social networks
runs_core |>
  mutate(
    decisions = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/decisions.csv"),
        show_col_types = FALSE
      ) |>
        mutate(source_statement = as.character(source_statement))
    })
  ) |>
  pull(decisions) |>
  reduce(bind_rows) |>
  write_parquet("parquet/decisions.parquet")
runs_core |>
  mutate(
    memory = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/memory.csv"),
        show_col_types = FALSE
      )
    })
  ) |>
  pull(memory) |>
  reduce(bind_rows) |>
  write_parquet("parquet/memory.parquet")
runs_core |>
  mutate(
    social_network = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/social_network.csv"),
        show_col_types = FALSE
      )
    })
  ) |>
  pull(social_network) |>
  reduce(bind_rows) |>
  write_parquet("parquet/social_network.parquet")
runs_core |>
  mutate(
    statements = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/statements.csv"),
        show_col_types = FALSE
      ) |>
        mutate(text = as.character(text))
    })
  ) |>
  pull(statements) |>
  reduce(bind_rows) |>
  write_parquet("parquet/statements.parquet")

memory <- read_parquet("parquet/memory.parquet")
statements <- read_parquet("parquet/statements.parquet") |>
  filter(run_id %in% runs_core$run_id[nrow(runs_core)]) |>
  select(id = S_id, label = text) |>
  mutate(type = "statement") |>
  distinct()
agents <- tibble(
  id = paste0("A", 1:max(memory$A_id)),
  type = "agent",
)

for (id in runs_core$run_id) {
  for (t in c(5, 10, 20, 50, 100)) {
    memory_run <- memory |> filter(run_id == id, .data$t == {{ t }})
    statements_run <- memory_run |>
      select(S_id) |>
      distinct() |>
      left_join(statements, by = c("S_id" = "id")) |>
      rename(id = S_id)
    agents_run <- agents |>
      filter(id %in% paste0("A", unique(memory_run$A_id))) |>
      mutate(label = id)
    # Overlap links for Statement and Agent projection networks
    overlap_agents_run <- memory_run |>
      select(S_id, A_id) |>
      inner_join(memory_run, by = "S_id", relationship = "many-to-many") |>
      filter(A_id.x < A_id.y) |> # avoid self-pairs & duplicate (a,b)/(b,a)
      count(A_id.x, A_id.y, name = "weight") |>
      mutate(
        source = paste0("A", A_id.x),
        target = paste0("A", A_id.y),
        type = "AA",
        weight,
        .keep = "none"
      )
    overlap_statements_run <- memory_run |>
      select(S_id, A_id) |>
      inner_join(memory_run, by = "A_id", relationship = "many-to-many") |>
      filter(S_id.x < S_id.y) |> # avoid self-pairs & duplicate (a,b)/(b,a), this also works for character via lexicographic ordering
      count(S_id.x, S_id.y, name = "weight") |>
      rename(source = S_id.x, target = S_id.y) |>
      mutate(type = "SS")
    # ---- 2. PREPARE NODES AND EDGES -------------------------------
    nodes <- bind_rows(agents_run, statements_run) |>
      mutate(across(everything(), as.character)) |>
      mutate(across(everything(), \(x) replace_na(x, "")))
    edges <- memory_run |>
      mutate(
        source = paste0("A", A_id),
        target = S_id,
        type = "AS",
        weight = 1,
        .keep = "none"
      ) |>
      rbind(overlap_agents_run, overlap_statements_run)
    message(
      "Nodes: ",
      nrow(nodes),
      " | Edges: ",
      nrow(edges),
      " | t: ",
      t
    ) # ---- 4. SERIALISE ------------------------------------------
    graph_json <- toJSON(
      list(nodes = nodes, links = edges),
      auto_unbox = TRUE,
      pretty = FALSE
    )
    run_id_desc <- runs_core |> filter(run_id == id) |> pull(run_id_desc)
    output_file <- glue("networkjson/{run_id_desc}_t{t}.json")
    write_file(graph_json, output_file)
  }
}

# Generate manifest by scanning actual JSON files and extracting parameters from filenames
# Pattern: {timestamp}_A{N}_M{M}_tmax{tmax}_op{openmindedness}_seed{seed}_{model}_t{t}.json

json_files <- dir("networkjson/", full.names = FALSE)
manifest <- tibble(
  filename = json_files,
  file = paste0("networkjson/", json_files)
) |>
  mutate(
    # Extract parameters from filename using regex
    timestamp = str_extract(filename, "^[0-9]{6}-[0-9]{4}"),
    N = as.numeric(str_extract(filename, "(?<=_A)\\d+")),
    M = as.numeric(str_extract(filename, "(?<=_M)\\d+(?=_)")),
    tmax = as.numeric(str_extract(filename, "(?<=tmax)\\d+")),
    openmindedness = as.numeric(str_extract(filename, "(?<=op)\\d+")),
    seed = as.numeric(str_extract(filename, "(?<=seed)\\d+")),
    model = str_extract(filename, "(?<=_)[a-z]+(?=_t)"),
    t = as.numeric(str_extract(filename, "(?<=_t)\\d+(?=\\.json)"))
  ) |>
  select(file, timestamp, N, M, tmax, openmindedness, seed, model, t) |>
  mutate(
    social_posting = TRUE # All current runs have this as TRUE
  ) |>
  arrange(timestamp, N, M, openmindedness, seed, t)

write(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), "manifest.json")
