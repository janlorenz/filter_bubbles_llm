readRenviron(".Renviron")
library(tidyverse)
library(glue)
library(arrow)
library(httr2)

prepare_data_sim_run <- function(r) {
  runs_core <- read_parquet("parquet/runs_core.parquet")
  f <- runs_core |> filter(run_id_desc == r) |> pull(run_folder)
  decisions <- read_parquet(glue(
    "rawdata/experiments/{f}/decisions_from_json.parquet"
  )) |>
    arrange(time_step, source, agent_id) |>
    select(
      time_step,
      agent_id,
      memory_prompt = memory_ids,
      source,
      everything()
    ) |>
    # produce feed by aggregating over all in timestep
    mutate(
      feed = case_when(
        source == "feed" ~ list(
          shown_candidates |>
            reduce(c) |>
            unique()
        ),
        .default = NULL
      ),
      .by = c(time_step, source),
      .after = source
    ) |>
    mutate(
      posted = "",
      shown_candidates = list(character()),
      .after = incoming
    ) |>
    # Placeholders for variables needed for computing decisions including jev and
    mutate(,
      jev_model = "",
      jev_input_token = 0,
      jev_output_token = 0,
      jev_probs = list(numeric()),
      decision = "",
      selected_statement_id = "",
      dropped_statement_id = "",
      .after = shown_candidates
    ) |>
    # Initialize memory_prompt and add memory_new
    mutate(
      memory_prompt = ifelse(
        time_step == 1 & source == "feed",
        memory_prompt,
        list(character())
      ),
      memory_new = list(character())
    )
}

payload_integrate_decision <- function(
  memory,
  candidates,
  statements,
  openmindedness = NA
) {
  memory_text <- tibble(S_id = memory) |>
    left_join(statements, by = join_by(S_id)) |>
    pull(text) |>
    str_glue("{num}. {s}", s = _, num = 1:length(memory)) |>
    paste0(collapse = "\\n")
  openmindedness_text <- ifelse(
    is.na(openmindedness),
    "",
    glue(
      "\\nOn a scale from 0 to 10, where 0 means you almost never accept new statements and 10 means you almost always accept new statements, your openmindedness is {openmindedness}."
    )
  )
  num_statements_text <- if_else(
    length(memory) == 1,
    "one statement",
    glue("{xfun::n2w(length(memory))} statements")
  )
  state <- list(
    condition = glue(
      "You are an individual with a worldview represented by {num_statements_text}: \\n {memory_text}{openmindedness_text}"
    )
  )
  questions <- tibble(S_id = candidates) |>
    left_join(statements, by = join_by(S_id)) |>
    pull(text) |>
    str_glue("Does this statement fit into your worldview?\\n{s}", s = _) |>
    as.list() |>
    map(\(x) list(type = "noul", instructions = x)) |>
    set_names(paste0("Integrate", seq(1, length(candidates))))
  payload <- list(
    model = "jev-latest",
    state = state,
    questions = questions
  )
}
payload_drop_decision <- function(
  memory,
  statments
) {
  state <- list(
    condition = glue(
      "You are an individual with a worldview represented by a list of statements."
    )
  )
  questions <- list()
  questions$Drop <-
    list(
      type = "choice",
      instructions = "Which statement from your would you drop to integrate a new one?",
      criteria = tibble(S_id = memory) |>
        left_join(statements, by = join_by(S_id)) |>
        pull(text) |>
        as.list() |>
        set_names(memory)
    )
  payload <- list(
    model = "jev-latest",
    state = state,
    questions = questions
  )
}

call_jev <- function(payload) {
  response <- request("https://api.typesafe.ai/v1/systemone") |>
    req_headers(
      "Authorization" = paste("Bearer", Sys.getenv("TYPESAFEAI_KEY")),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(payload) |>
    req_method("POST") |>
    req_retry(max_tries = 5, backoff = \(n) min(2^n, 60)) |>
    req_perform()
  result <- resp_body_json(response)
}

decisions_to_memory <- function(d) {
  bind_rows(
    d |>
      filter(time_step == 1, source == "feed") |>
      select(time_step, agent_id, memory = memory_prompt) |>
      mutate(time_step = 0),
    d |>
      filter(source == "social") |>
      select(time_step, agent_id, memory = memory_new)
  ) |>
    unnest(cols = c(memory))
}
