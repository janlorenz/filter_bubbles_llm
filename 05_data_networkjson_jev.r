library(tidyverse)
library(jsonlite)
library(arrow)
library(glue)
source("simulation_jev/jev_functions.r")

## Network Data in json
runs_core_jev <- read_parquet(
  "simulation_jev/runs/runs_core_jev_batch1.parquet"
)
statements <- read_parquet("parquet/statements_meta.parquet") |>
  select(id = S_id, label = Worldview_Statement) |>
  mutate(type = "statement") |>
  distinct()
dec_files <- list.files(
  "simulation_jev/runs/",
  pattern = "\\.parquet$",
  full.names = TRUE
) |>
  keep(\(f) !str_detect(f, "runs_core_jev_batch1"))
decisions <- open_dataset(dec_files, unify_schemas = TRUE) |>
  mutate(source_file = add_filename()) |>
  collect() |>
  mutate(
    run_id_desc = tools::file_path_sans_ext(basename(source_file)),
    .keep = "unused"
  )
memory <- runs_core_jev$run_id_desc |>
  map(\(r) decisions |> filter(run_id_desc == r) |> decisions_to_memory()) |>
  reduce(bind_rows)
agents <- tibble(
  id = paste0("A", 1:max(memory$A_id)),
  type = "agent",
)
for (id in runs_core_jev$run_id_desc) {
  for (t in c(5, 10, 20, 50, 100)) {
    memory_run <- memory |> filter(run_id_desc == id, .data$t == {{ t }})
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
    output_file <- glue("networkjson/{id}_t{t}.json")
    write_file(graph_json, output_file)
  }
}

# Generate manifest by scanning actual JSON files and extracting parameters from filenames
# Pattern: {timestamp}_A{N}_M{M}_tmax{tmax}_op{openmindedness}_seed{seed}_{model}_t{t}.json

json_files <- dir("networkjson/", full.names = FALSE) |> str_subset("jev")
manifest <- tibble(
  filename = json_files,
  file = paste0("networkjson/", json_files)
) |>
  mutate(
    # Extract parameters from filename using regex
    timestamp = str_extract(filename, "^[0-9]{6}-[0-9]{6}"),
    N = as.numeric(str_extract(filename, "(?<=_A)\\d+")),
    M = as.numeric(str_extract(filename, "(?<=_M)\\d+(?=_)")),
    tmax = as.numeric(str_extract(filename, "(?<=tmax)\\d+")),
    probability_acceptance_threshold = as.numeric(str_extract(
      filename,
      "(?<=ac)\\d+"
    )) /
      100,
    post = str_extract(filename, "(?<=_)(FS|F|Ffix|S)(?=_)"),
    seed = as.numeric(str_extract(filename, "(?<=seed)\\d+")),
    model = str_extract(filename, "(?<=_)[a-z]+(?=_t)"),
    t = as.numeric(str_extract(filename, "(?<=_t)\\d+(?=\\.json)"))
  ) |>
  select(
    file,
    timestamp,
    N,
    M,
    tmax,
    probability_acceptance_threshold,
    post,
    seed,
    model,
    t
  ) |>
  arrange(timestamp, N, M, probability_acceptance_threshold, post, seed, t)

write(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), "manifest_jev.json")
