library(jsonlite)
library(tidyverse)
library(arrow)
library(glue)

read_malformed_jsonl <- function(file_path) {
  content <- readChar(file_path, file.info(file_path)$size)
  # Helper to convert all vectors in a list to character, preserving NA
  to_character_na <- function(x) {
    if (is.list(x)) {
      lapply(x, to_character_na)
    } else if (is.atomic(x)) {
      # Convert to character first
      result <- as.character(x)
      # Restore NA where "NA" string appears
      result[result == "NA"] <- NA_character_
      result
    } else {
      x
    }
  }
  tibble(raw = strsplit(content, "\\}\\s*\\{")[[1]]) |>
    mutate(
      idx = row_number(),
      raw = case_when(
        idx == 1 ~ paste0(raw, "}"),
        idx == n() ~ paste0("{", raw, "}") |> sub("\\}$", "", x = _),
        .default = paste0("{", raw, "}")
      )
    ) |>
    pull(raw) |>
    map(fromJSON) |>
    map(to_character_na)
}

runs_core <- read_parquet("parquet/runs_core.parquet")

write_parquet_decisions_from_json <- function(f) {
  d <- read_malformed_jsonl(glue("rawdata/experiments/{f}/simulation.jsonl"))
  df <- rbind(
    if (!as.logical(d[[1]]$feed$enabled)) {
      NULL
    } else {
      d |> # Feed
        map(\(x) {
          as_tibble(x$feed$interactions) |>
            mutate(time_step = x$time_step, source = "feed", .before = 1)
        }) |>
        list_rbind() |>
        select(
          agent_id,
          time_step,
          source,
          memory_ids,
          shown_candidates,
          decision,
          selected_statement_id,
          dropped_statement_id
        ) |>
        mutate(incoming = list(NULL), .before = shown_candidates)
    },
    if (length(d[[1]]$network) == 0) {
      NULL
    } else {
      d |> # Social
        map(\(x) {
          as_tibble(x$network) |>
            mutate(time_step = x$time_step, source = "social", .before = 1)
        }) |>
        list_rbind() |>
        select(
          agent_id = target_agent_id,
          time_step,
          source,
          memory_ids,
          incoming,
          shown_candidates,
          decision,
          selected_statement_id = chosen_statement_id,
          dropped_statement_id
        )
    }
  ) |>
    mutate(
      time_step = time_step |> as.integer(),
      agent_id = agent_id |> as.integer(),
      incoming = map(incoming, function(x) {
        x_int <- suppressWarnings(as.integer(x))
        x_int[!is.na(x_int)]
      }),
      memory_ids = pmap(
        list(memory_ids, selected_statement_id, dropped_statement_id),
        \(x, y, z) x |> setdiff(y) |> union(z) |> discard(is.na)
      )
    ) |>
    arrange(agent_id, time_step, source) |>
    write_parquet(glue("rawdata/experiments/{f}/decisions_from_json.parquet"))
}
runs_core$run_folder |> walk(\(f) write_parquet_decisions_from_json(f))


r <- runs_core |>
  mutate(
    decision = run_folder |>
      map(\(f) {
        read_parquet(glue(
          "rawdata/experiments/{f}/decisions_from_json.parquet"
        ))
      })
  )

decision_all <- unnest(r, cols = c(decision)) |>
  select(-run_id, -timestamp, -model_name, -run_folder, -datetime) |>
  write_parquet("parquet/decisions_from_json.parquet")

# # Documentation:
# # incoming: matrix with col1 agent-id, col2 statement-id, it shows which statements come from which network neighbor
# # shown_candidate: vector with the statements put in the prompt (removing duplicates), also the order is random
# # memory_id: looks like this is memory AFTER decision. So, for original the dropped statement must be added and the selected one removed
# # Just social
# d <- read_malformed_jsonl(
#   "rawdata/experiments/20260924-210228_N100_T100_Mem10_Model-gpt-oss-20b_feed-shared_13dffd96/simulation.jsonl"
# )
# Just Feed
# d <- read_malformed_jsonl(
#   "rawdata/experiments/20260814-104605_N100_T100_Mem10_Model-gpt-oss-20b_feed-shared_049f0582/simulation.jsonl"
# )
# # Both
# d <- read_malformed_jsonl(
#   "rawdata/experiments/20260326-112135_N100_T100_Mem10_Model-gpt-oss-20b_feed-shared_452deab0/simulation.jsonl"
# )
