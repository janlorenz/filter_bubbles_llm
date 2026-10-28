readRenviron(".Renviron")
library(tidyverse)
library(glue)
library(arrow)
library(httr2)

statements <- read_parquet("parquet/statements_meta.parquet") |>
  select(S_id, text = Worldview_Statement)

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

create_payload_integrate_decision <- function(
  memory,
  candidates,
  statements,
  openmindedness = NA,
  state_form = "pure",
  model = "jev-latest"
) {
  memory_text <- tibble(S_id = memory) |>
    left_join(statements, by = join_by(S_id)) |>
    pull(text) |>
    str_glue("{num}. {s}", s = _, num = 1:length(memory)) |>
    paste0(collapse = "\n")
  openmindedness_text <- ifelse(
    is.na(openmindedness),
    "",
    glue(
      "\nOn a scale from 0 to 10, where 0 means you almost never accept new statements and 10 means you almost always accept new statements, your openmindedness is {openmindedness}."
    )
  )
  num_statements_text <- if_else(
    length(memory) == 1,
    "one statement",
    glue("{xfun::n2w(length(memory))} statements")
  )
  if (state_form == "condition") {
    state <- list(
      condition = glue(
        "You are an individual with a worldview represented by {num_statements_text}: \n {memory_text}{openmindedness_text}"
      )
    )
  }
  if (state_form == "pure") {
    state <- glue(
      "You are an individual with a worldview represented by {num_statements_text}: \n {memory_text}{openmindedness_text}"
    )
  }
  if (state_form == "structured") {
    state <- list(
      identity = glue(
        "You are an individual with a worldview represented by {num_statements_text}.{openmindedness_text}"
      ),
      statements = tibble(S_id = memory) |>
        left_join(statements, by = join_by(S_id)) |>
        pull(text)
    )
  }
  questions <- tibble(S_id = candidates) |>
    left_join(statements, by = join_by(S_id)) |>
    pull(text) |>
    str_glue("Does this statement fit into your worldview?\n{s}", s = _) |>
    as.list() |>
    map(\(x) list(type = "noul", instructions = x)) |>
    set_names(paste0("Integrate", seq(1, length(candidates))))
  payload <- list(
    model = model,
    state = state,
    questions = questions
  )
}


create_payload_integrate_decision_mult <- function(
  memory, # must be a list of memory vectors
  candidates, # must be a list of candidate vectors
  statements,
  openmindedness = NA,
  state_form = "pure",
  agent_ids = seq_along(memory), # used to build the named state keys
  model = "jev-latest"
) {
  stopifnot(length(memory) == length(candidates))
  stopifnot(length(agent_ids) == length(memory), !anyDuplicated(agent_ids))

  # Named keys (agent_<id>) resolve far more reliably than array positions
  # when many similar agents share one state.
  agent_keys <- paste0("agent_", agent_ids)

  agent_entry <- function(mem) {
    texts <- tibble(S_id = mem) |>
      left_join(statements, by = join_by(S_id)) |>
      pull(text)
    num_statements_text <- if (length(mem) == 1) {
      "one statement"
    } else {
      glue("{xfun::n2w(length(mem))} statements")
    }
    openmindedness_text <- if (is.na(openmindedness)) {
      ""
    } else {
      glue(
        "\nOn a scale from 0 to 10, where 0 means you almost never accept new statements and 10 means you almost always accept new statements, your openmindedness is {openmindedness}."
      )
    }
    if (state_form == "structured") {
      list(
        identity = as.character(glue(
          "You are an individual with a worldview represented by {num_statements_text}.{openmindedness_text}"
        )),
        statements = as.list(texts)
      )
    } else {
      numbered <- paste0(seq_along(texts), ". ", texts, collapse = "\n")
      as.character(glue(
        "You are an individual with a worldview represented by {num_statements_text}: \n {numbered}{openmindedness_text}"
      ))
    }
  }

  state <- list(agents = set_names(map(memory, agent_entry), agent_keys))

  # One noul question per agent x candidate; the instruction points at the
  # agent's state entry via a backticked path. Question ids stay positional
  # (Integrate_a<row>_c<k>); they are not seen by the model.
  questions <- imap(candidates, \(cand, i) {
    texts <- tibble(S_id = cand) |>
      left_join(statements, by = join_by(S_id)) |>
      pull(text)
    path <- paste0("`agents.", agent_keys[i], "`")
    qs <- map(texts, \(s) {
      instructions <- as.character(glue(
        "Does this statement fit into the worldview of {path}? Statement: {s}"
      ))
      list(type = "noul", instructions = instructions)
    })
    set_names(qs, paste0("Integrate_a", i, "_c", seq_along(cand)))
  }) |>
    list_flatten(name_spec = "{inner}")

  list(
    model = model,
    state = state,
    questions = questions
  )
}


create_payload_drop_decision <- function(
  memory,
  statements,
  model = "jev-latest"
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
    model = model,
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

df_mutate_jev_result <- function(df, state_form = "pure", model = "jev-latest") {
  df |>
    mutate(
      jev_result = map2(
        memory_prompt,
        shown_candidates,
        \(mem, cand) {
          if (length(cand) == 0) {
            NULL
          } else {
            create_payload_integrate_decision(
              mem,
              cand,
              statements,
              openmindedness = NA,
              state_form = state_form,
              model = model
            ) |>
              call_jev()
          }
        }
      )
    )
}

df_unnest_jev_results <- function(df) {
  df |>
    mutate(
      jev_model = jev_result |>
        map_chr(\(x) if (is.null(x)) NA_character_ else x$model),
      jev_input_token = jev_result |>
        map_dbl(\(x) if (is.null(x)) NA_real_ else x$usage$input_tokens),
      jev_output_token = jev_result |>
        map_dbl(\(x) if (is.null(x)) NA_real_ else x$usage$output_tokens),
      jev_probs = jev_result |>
        map(\(x) if (is.null(x)) numeric() else map_dbl(x$answers, "noul")),
      decision = map_chr(jev_probs, \(y) {
        case_when(
          length(y) == 0 ~ "already_in_memory",
          all(y < 0.5) ~ "none_selected",
          .default = "selected"
        )
      }),
      selected_statement_id = if_else(
        decision == "selected",
        map2_chr(shown_candidates, jev_probs, \(x, y) {
          if (length(y) == 0) NA_character_ else x[which.max(y)]
        }),
        NA_character_
      )
    ) |>
    select(-jev_result)
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
