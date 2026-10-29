readRenviron(".Renviron")
library(tidyverse)
library(glue)
library(arrow)
library(httr2)

# Read the statement pool
statements <- read_parquet("parquet/statements_meta.parquet") |>
  select(S_id, text = Worldview_Statement)

## Data preparation functions

#' Prepare an existing dataset for a jev simulation
#' It loads an existing decisions table from a run identified in runs_core
#'
#' @param r The run_id_desc in runs_core
#'
#' @returns A decsion dataframe cleaned from the existing entries leaving only
#' - the initialized agent statements
#' - the incoming order from the network partners (in social posting), and
#' - the feed posted statements in place
#' @example
#' d <- prepare_data_sim_run("260630-120838_A100_M5_tmax100_op1_FS_seed354_gpt")
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
        time_step == 1 & source == first(source),
        memory_prompt,
        list(character())
      ),
      memory_new = list(character())
    )
}


## Core simulation functions

#' Run a simulation built on a prepared decision data frame
#' The decision dataframe is produced with prepare_data_sim_run
#'
#' @param dec The decision table produced with prepare_data_sim_run
#' @param max_memory The run's M: maximal number of statements per agent
#' @param probability_acceptance_threshold
#' @param openmindedness The openmindedness parameter encoded in prompt, not use so far, replaced by probability_acceptance_threshold
#' @param verbose
#' @param checkpoint_path Where data is or should be saved
#' @param checkpoint_every
#' @param resume Whether a resume under the existing checkpoint shall be tried
#' @param start_t Whether a certain start point for a resume of computation shall be used
#' @param state_form state refers to the creation of the jev payload
#' @param use_mult Use the multiple agents per jev call setting
#' @param batch_size Maximal number of agents in one batch
#' @param max_tokens Maximal extimated number of tokens in one batch before a force split
#' @param model jev model id tag
#'
#' @returns A filled decision data frame
#'
#' @export
#' @examples
run_simulation <- function(
  dec,
  max_memory,
  probability_acceptance_threshold = 0.5,
  openmindedness = NA,
  verbose = TRUE,
  checkpoint_path = NULL,
  checkpoint_every = 1,
  resume = FALSE,
  start_t = NULL,
  state_form = "pure",
  use_mult = FALSE,
  batch_size = NULL,
  max_tokens = 40000,
  model = "jev-latest"
) {
  # Runs may have only a feed phase (F), only a social phase (S), or both (FS)
  has_feed <- "feed" %in% dec$source
  has_social <- "social" %in% dec$source
  # Source that receives memory at the start of a time step
  first_phase <- if (has_feed) "feed" else "social"
  # Source whose memory_new carries over to the next time step
  last_phase <- if (has_social) "social" else "feed"

  if (resume) {
    if (is.null(checkpoint_path)) {
      stop(
        "resume = TRUE requires checkpoint_path to point at an existing checkpoint file"
      )
    }
    dec <- read_parquet(checkpoint_path) |>
      # arrow round-trips list-columns as typed list_of<character>, which
      # rows_update() can't reconcile with the plain lists produced fresh by
      # sim_agent_timestep(); strip that typing back to base R lists
      mutate(across(where(is.list), ~ map(.x, identity)))
    if (is.null(start_t)) {
      # Last time step with every row processed (decision populated) + 1
      start_t <- dec |>
        filter(decision != "") |>
        pull(time_step) |>
        max(0, na.rm = TRUE) +
        1
    }
    if (start_t > 1 && start_t <= max(dec$time_step)) {
      # Rebuild the hand-off from the previous social phase. The run loop only
      # writes it when t < tmax, so a checkpoint from a shorter run (or a
      # manual rollback) may have an empty or stale feed memory for start_t.
      dec <- dec |>
        rows_update(
          handoff_memory(
            dec |> filter(time_step == start_t - 1, source == last_phase),
            start_t,
            first_phase
          ),
          by = c("time_step", "agent_id", "source")
        )
    }
    if (verbose) {
      message(glue(
        "Resuming from {checkpoint_path}: discarding any state for t >= {start_t}, restarting there"
      ))
    }
  } else if (is.null(start_t)) {
    start_t <- 1
  }
  tmax <- dec |> pull(time_step) |> max(na.rm = TRUE)

  if (start_t > tmax) {
    if (verbose) {
      message(glue(
        "Nothing to do: start_t ({start_t}) > tmax ({tmax}); checkpoint already complete"
      ))
    }
    return(dec)
  }

  for (t in seq(start_t, tmax)) {
    t_start <- Sys.time()
    tokens <- 0

    if (has_feed) {
      # --- feed phase ---
      dec_feed <- sim_agent_timestep(
        dec,
        t,
        "feed",
        max_memory = max_memory,
        probability_acceptance_threshold = probability_acceptance_threshold,
        openmindedness = openmindedness,
        state_form = state_form,
        use_mult = use_mult,
        batch_size = batch_size,
        max_tokens = max_tokens,
        model = model
      )
      dec <- dec |>
        rows_update(dec_feed, by = c("time_step", "agent_id", "source"))
      tokens <- tokens + sum(dec_feed$jev_input_token, na.rm = TRUE)

      if (has_social) {
        # feed's memory_new becomes social's (shuffled) memory_prompt, same time step
        dec <- dec |>
          rows_update(
            handoff_memory(dec_feed, t, "social"),
            by = c("time_step", "agent_id", "source")
          )
      } else if (t < tmax) {
        # feed-only run: feed's memory_new becomes next step's feed memory_prompt
        dec <- dec |>
          rows_update(
            handoff_memory(dec_feed, t + 1, "feed"),
            by = c("time_step", "agent_id", "source")
          )
      }
    }

    if (has_social) {
      # --- social phase ---
      dec_social <- sim_agent_timestep(
        dec,
        t,
        "social",
        max_memory = max_memory,
        probability_acceptance_threshold = probability_acceptance_threshold,
        openmindedness = openmindedness,
        state_form = state_form,
        use_mult = use_mult,
        batch_size = batch_size,
        max_tokens = max_tokens,
        model = model
      )
      dec <- dec |>
        rows_update(dec_social, by = c("time_step", "agent_id", "source"))
      tokens <- tokens + sum(dec_social$jev_input_token, na.rm = TRUE)

      # social's memory_new becomes next step's memory_prompt (feed if present, else social)
      if (t < tmax) {
        dec <- dec |>
          rows_update(
            handoff_memory(dec_social, t + 1, first_phase),
            by = c("time_step", "agent_id", "source")
          )
      }
    }

    if (verbose) {
      elapsed <- round(difftime(Sys.time(), t_start, units = "secs"), 1)
      message(glue(
        "Time step {t}/{tmax} done in {elapsed}s ({tokens} input tokens)"
      ))
    }

    if (
      !is.null(checkpoint_path) && (t %% checkpoint_every == 0 || t == tmax)
    ) {
      write_parquet(dec, checkpoint_path)
      if (verbose) {
        message(glue(
          "  checkpoint saved to {checkpoint_path} (through t = {t})"
        ))
      }
    }
  }
  dec
}

#' Computes one (part of a) simulation time step specified by time_step and source
#' Can use multiple agent jev calls (cheaper), or individual jev calls
#' The non-jev decsion if a statement is selected and which happens based on the parameter prob probability_acceptance_threshold
#' The function takes in the full decision dataframe in
#' It returns a decision dataframe only with the new decisions
#'
#' @param decs
#' @param t
#' @param s
#' @param max_memory The run's M: maximal number of statements per agent
#' @param probability_acceptance_threshold
#' @param openmindedness
#' @param state_form
#' @param use_mult
#' @param batch_size
#' @param max_tokens
#' @param model
#'
#' @returns full decision dataframe
sim_agent_timestep <- function(
  decs,
  t,
  s,
  max_memory,
  probability_acceptance_threshold = 0.5,
  openmindedness = NA,
  state_form = "pure",
  use_mult = FALSE,
  batch_size = NULL,
  max_tokens = 40000,
  model = "jev-latest"
) {
  decs <- decs |> filter(time_step == t, source == s)
  if (s == "feed") {
    decs <- decs |>
      mutate(
        shown_candidates = map2(feed, memory_prompt, \(x, y) {
          sample(x) |> setdiff(y)
        })
      )
  } else {
    decs <- decs |>
      mutate(posted = map_chr(memory_prompt, \(x) sample(x, 1))) |>
      mutate(
        shown_candidates = map2(incoming, memory_prompt, \(x, m) {
          posted[x] |> unique() |> setdiff(m)
        })
      )
  }
  decs <- if (use_mult) {
    jev_mult_timestep(
      decs,
      state_form,
      openmindedness,
      batch_size,
      max_tokens,
      model = model
    )
  } else {
    jev_single_timestep(decs, state_form, openmindedness, model = model)
  }
  # Now the non-jev decision based on probability_acceptance_threshold
  decs <- decs |>
    mutate(
      decision = map_chr(jev_probs, \(y) {
        case_when(
          length(y) == 0 ~ "already_in_memory",
          all(y < probability_acceptance_threshold) ~ "none_selected",
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
    mutate(
      dropped_statement_id = if_else(
        decision == "selected" & map_dbl(memory_prompt, length) == max_memory,
        map_chr(memory_prompt, \(x) sample(x, 1)),
        NA
      ),
      memory_new = pmap(
        list(memory_prompt, selected_statement_id, dropped_statement_id),
        \(x, y, z) x |> union(y) |> setdiff(z)
      )
    )
}

#' The multi agent in one time step jev-call (batched if specified or forced)
#'
#' Agents of this time step / source are sent to jev in batches (one call per
#' batch). Batches are capped by batch_size (NULL = no agent cap) and by an
#' estimated max_tokens input budget; the API rejects requests above 64k
#' tokens, so the default leaves a wide safety margin. Agents without
#' candidates are left out. Token usage is split evenly across the agents in a
#' call so per-row sums still add up to each call's total.
#'
#' @param decs
#' @param state_form
#' @param openmindedness
#' @param batch_size
#' @param max_tokens
#' @param model
#'
#' @returns
#'
#' @export
#' @examples
jev_mult_timestep <- function(
  decs,
  state_form,
  openmindedness,
  batch_size = NULL,
  max_tokens = 40000,
  model = "jev-latest"
) {
  has_cand <- lengths(decs$shown_candidates) > 0
  decs <- decs |>
    mutate(
      jev_model = NA_character_,
      jev_input_token = NA_real_,
      jev_output_token = NA_real_,
      jev_probs = map(seq_len(n()), \(i) numeric())
    )
  rows <- which(has_cand)
  if (length(rows) == 0) {
    return(decs)
  }
  costs <- pmap_dbl(
    list(
      decs$memory_prompt[rows],
      decs$shown_candidates[rows],
      decs$agent_id[rows]
    ),
    \(m, c, id) estimate_agent_tokens(m, c, id, state_form)
  )
  batches <- pack_batches(
    costs,
    batch_size = if (is.null(batch_size)) Inf else batch_size,
    max_tokens = max_tokens
  ) |>
    map(\(ix) rows[ix])

  for (batch_rows in batches) {
    called <- decs[batch_rows, ]
    result <- create_payload_integrate_decision_mult(
      memory = called$memory_prompt,
      candidates = called$shown_candidates,
      statements = statements,
      openmindedness = openmindedness,
      state_form = state_form,
      agent_ids = called$agent_id,
      model = model
    ) |>
      call_jev()
    n_called <- nrow(called)
    decs$jev_model[batch_rows] <- result$model
    decs$jev_input_token[batch_rows] <- result$usage$input_tokens / n_called
    decs$jev_output_token[batch_rows] <- result$usage$output_tokens / n_called
    decs$jev_probs[batch_rows] <- map2(
      seq_len(n_called),
      called$shown_candidates,
      \(r, cands) {
        map_dbl(seq_along(cands), \(k) {
          result$answers[[paste0("Integrate_a", r, "_c", k)]]$noul
        })
      }
    )
  }
  decs
}

# One jev call per agent (original behaviour)
jev_single_timestep <- function(
  decs,
  state_form,
  openmindedness,
  model = "jev-latest"
) {
  decs |>
    mutate(
      jev_result = map2(memory_prompt, shown_candidates, \(mem, cand) {
        if (length(cand) == 0) {
          NULL
        } else {
          create_payload_integrate_decision(
            mem,
            cand,
            statements,
            openmindedness = openmindedness,
            state_form = state_form,
            model = model
          ) |>
            call_jev()
        }
      }),
      jev_model = jev_result |>
        map_chr(\(x) if (is.null(x)) NA_character_ else x$model),
      jev_input_token = jev_result |>
        map_dbl(\(x) if (is.null(x)) NA_real_ else x$usage$input_tokens),
      jev_output_token = jev_result |>
        map_dbl(\(x) if (is.null(x)) NA_real_ else x$usage$output_tokens),
      jev_probs = jev_result |>
        map(\(x) if (is.null(x)) numeric() else map_dbl(x$answers, "noul"))
    ) |>
    select(-jev_result)
}

#' Create a payload as nested list as needed for call_jev
#' using jsonlite to produce json and httr2 to pass it to TypeSafe.at API
#'
#' This is the version for a single decision!
#'
#' @param memory
#' @param candidates
#' @param statements
#' @param openmindedness
#' @param state_form
#' @param model
#'
#' @returns Nested list to parse to json
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

#' Create a payload as nested list as needed for call_jev
#' using jsonlite to produce json and httr2 to pass it to TypeSafe.at API
#'
#' This is the version for decisions of multiple agents !
#'
#' @param memory must be a list of memory vectors
#' @param candidates must be a list of candidate vectors of the same length
#' @param statements
#' @param openmindedness
#' @param state_form
#' @param agent_ids used to build the named state keys
#' @param model
#'
#' @returns Nested list to parse to json
create_payload_integrate_decision_mult <- function(
  memory,
  candidates,
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

#' Send a payload as a list to jev using typesafe.ai API
#' payload to be created by create_payload_integrate_decision or
#' create_payload_integrate_decision_mult
#'
#' @param payload
#'
#' @returns The jev resonse already in reparsed json into an R objects
#'
#' @export
#' @examples
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

#' Still a stub. Planned to implement a drop decision using jev. Not used so far.
#'
#' @param memory
#' @param statements
#' @param model
#'
#' @returns
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


## Helpers

# Turns the memory_new of a phase's rows into memory_prompt rows for another
# time step / source, with the memory shuffled
handoff_memory <- function(rows, to_t, to_s) {
  rows |>
    mutate(
      time_step = to_t,
      agent_id = agent_id,
      source = to_s,
      memory_prompt = map(memory_new, sample),
      .keep = "none"
    )
}

# Rough input-token cost of one agent (worldview + all its questions) inside a
# mult payload, from the text jev actually reads (JSON keys and punctuation do
# not seem to count). Measured: about 3.7 characters per token, so / 3.5 is
# slightly conservative.
estimate_agent_tokens <- function(memory, candidates, agent_id, state_form) {
  payload <- create_payload_integrate_decision_mult(
    memory = list(memory),
    candidates = list(candidates),
    statements = statements,
    state_form = state_form,
    agent_ids = agent_id
  )
  chars <- sum(nchar(unlist(payload$state$agents))) +
    sum(nchar(map_chr(payload$questions, "instructions")))
  ceiling(chars / 3.5)
}

# Greedily pack consecutive agents into batches that hold at most batch_size
# agents and at most max_tokens estimated tokens. An agent that exceeds the
# budget on its own gets its own batch.
pack_batches <- function(costs, batch_size, max_tokens) {
  batch_id <- integer(length(costs))
  current <- 1L
  n_in <- 0L
  tokens_in <- 0
  for (i in seq_along(costs)) {
    full <- n_in > 0 &&
      (n_in >= batch_size || tokens_in + costs[i] > max_tokens)
    if (full) {
      current <- current + 1L
      n_in <- 0L
      tokens_in <- 0
    }
    batch_id[i] <- current
    n_in <- n_in + 1L
    tokens_in <- tokens_in + costs[i]
  }
  unname(split(seq_along(costs), batch_id))
}


#' Creates the memory form output
#'
#' @param d The decision form data frame
#'
#' @returns A data frame with the final memory of with each timestep-agent-statement combination as obs
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

#### Check if these are needed!! Probably not

# df_mutate_jev_result <- function(df, state_form = "pure", model = "jev-latest") {
#   df |>
#     mutate(
#       jev_result = map2(
#         memory_prompt,
#         shown_candidates,
#         \(mem, cand) {
#           if (length(cand) == 0) {
#             NULL
#           } else {
#             create_payload_integrate_decision(
#               mem,
#               cand,
#               statements,
#               openmindedness = NA,
#               state_form = state_form,
#               model = model
#             ) |>
#               call_jev()
#           }
#         }
#       )
#     )
# }

# #' Unnests a jev_results list column into a few more columns
# #'
# #' @param df A data frame with a list column jev_results
# #' @param probability_acceptance_threshold The probability acceptance threshold used for the call
# #'
# #' @returns The same df with some jev result based columns remove the jev_results column
# df_unnest_jev_results <- function(df, probability_acceptance_threshold) {
#   df |>
#     mutate(
#       jev_model = jev_result |>
#         map_chr(\(x) if (is.null(x)) NA_character_ else x$model),
#       jev_input_token = jev_result |>
#         map_dbl(\(x) if (is.null(x)) NA_real_ else x$usage$input_tokens),
#       jev_output_token = jev_result |>
#         map_dbl(\(x) if (is.null(x)) NA_real_ else x$usage$output_tokens),
#       jev_probs = jev_result |>
#         map(\(x) if (is.null(x)) numeric() else map_dbl(x$answers, "noul")),
#       decision = map_chr(jev_probs, \(y) {
#         case_when(
#           length(y) == 0 ~ "already_in_memory",
#           all(y < 0.5) ~ "none_selected",
#           .default = "selected"
#         )
#       }),
#       selected_statement_id = if_else(
#         decision == "selected",
#         map2_chr(shown_candidates, jev_probs, \(x, y) {
#           if (length(y) == 0) NA_character_ else x[which.max(y)]
#         }),
#         NA_character_
#       )
#     ) |>
#     select(-jev_result)
# }
