source("simulation_jev/jev_functions.r")

dec <- prepare_data_sim_run(
  "260326-112135_A100_M10_tmax100_op1_FS_seed355_gpt"
)

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

# Agents of this time step / source are sent to jev in batches (one call per
# batch). Batches are capped by batch_size (NULL = no agent cap) and by an
# estimated max_tokens input budget; the API rejects requests above 64k
# tokens, so the default leaves a wide safety margin. Agents without
# candidates are left out. Token usage is split evenly across the agents in a
# call so per-row sums still add up to each call's total.
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

sim_agent_timestep <- function(
  decs,
  t,
  s,
  rejection_threshold = 0.5,
  state_form = "pure",
  use_mult = FALSE,
  openmindedness = NA,
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
  decs <- decs |>
    mutate(
      decision = map_chr(jev_probs, \(y) {
        case_when(
          length(y) == 0 ~ "already_in_memory",
          all(y < rejection_threshold) ~ "none_selected",
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
        decision == "selected" & map_dbl(memory_prompt, length) == 10,
        map_chr(memory_prompt, \(x) sample(x, 1)),
        NA
      ),
      memory_new = pmap(
        list(memory_prompt, selected_statement_id, dropped_statement_id),
        \(x, y, z) x |> union(y) |> setdiff(z)
      )
    )
}

# dec_feed <- sim_agent_timestep(dec, 1, "feed")
# dec[1:100, ] <- dec_feed
# dec$memory_prompt[101:200] <- dec_feed$memory_new[1:100]
# dec_social <- sim_agent_timestep(dec, 1, "social")
# dec[101:200, ] <- dec_social

# Simulation run function

run_simulation <- function(
  dec,
  tmax = 100,
  rejection_threshold = 0.5,
  verbose = TRUE,
  checkpoint_path = NULL,
  checkpoint_every = 1,
  resume = FALSE,
  start_t = NULL,
  state_form = "pure",
  use_mult = FALSE,
  openmindedness = NA,
  batch_size = NULL,
  max_tokens = 40000,
  model = "jev-latest"
) {
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
          dec |>
            filter(time_step == start_t - 1, source == "social") |>
            mutate(
              time_step = start_t,
              agent_id = agent_id,
              source = "feed",
              memory_prompt = map(memory_new, sample),
              .keep = "none"
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

    # --- feed phase ---
    dec_feed <- sim_agent_timestep(
      dec,
      t,
      "feed",
      rejection_threshold = 0.5,
      state_form = state_form,
      use_mult = use_mult,
      openmindedness = openmindedness,
      batch_size = batch_size,
      max_tokens = max_tokens,
      model = model
    )
    dec <- dec |>
      rows_update(dec_feed, by = c("time_step", "agent_id", "source"))

    # feed's memory_new becomes social's (shuffled) memory_prompt, same time step
    dec <- dec |>
      rows_update(
        dec_feed |>
          mutate(
            time_step = time_step,
            agent_id = agent_id,
            source = "social",
            memory_prompt = map(memory_new, sample),
            .keep = "none"
          ),
        by = c("time_step", "agent_id", "source")
      )

    # --- social phase ---
    dec_social <- sim_agent_timestep(
      dec,
      t,
      "social",
      rejection_threshold = 0.5,
      state_form = state_form,
      use_mult = use_mult,
      openmindedness = openmindedness,
      batch_size = batch_size,
      max_tokens = max_tokens,
      model = model
    )
    dec <- dec |>
      rows_update(dec_social, by = c("time_step", "agent_id", "source"))

    # social's memory_new becomes next time step's (shuffled) feed memory_prompt
    if (t < tmax) {
      dec <- dec |>
        rows_update(
          dec_social |>
            mutate(
              time_step = t + 1,
              agent_id = agent_id,
              source = "feed",
              memory_prompt = map(memory_new, sample),
              .keep = "none"
            ),
          by = c("time_step", "agent_id", "source")
        )
    }

    if (verbose) {
      elapsed <- round(difftime(Sys.time(), t_start, units = "secs"), 1)
      tokens <- sum(
        dec_feed$jev_input_token,
        dec_social$jev_input_token,
        na.rm = TRUE
      )
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

# Single entry point: if a checkpoint already exists on disk, resume from it
# (auto-detecting the first incomplete time step) instead of restarting from
# t = 1. This makes re-sourcing the script after a hang/crash safe.
# To force a manual rollback (e.g. redo from t = 23 because you don't trust
# the last checkpointed step), set start_t explicitly below.
checkpoint_file <- "MyJevSim_maxbatch_prob04_jev-1.13.0.parquet"
d <- run_simulation(
  dec,
  tmax = 100,
  rejection_threshold = 0.4,
  checkpoint_path = checkpoint_file,
  checkpoint_every = 1,
  resume = file.exists(checkpoint_file),
  start_t = NULL, # e.g. set to 23 to force a manual rollback
  state_form = "pure", # "pure", "condition" or "structured"
  use_mult = TRUE, # batched jev calls per time step and source
  batch_size = 100, # all agents in one call, unless the token budget is hit
  max_tokens = 55000, # estimated input tokens per call; API limit is 64k
  model = "jev-1.13.0" # or e.g. "jev-1.13.0" to pin a version
)
write_parquet(d, checkpoint_file)

# d <- read_parquet("MyJevSim_maxbatch.parquet")
cost_token <- 0.042 / 10^6
d$jev_input_token |> sum(na.rm = TRUE) * cost_token
d$jev_model |> first()
# # Jev cost: $0.042 per 1,000,000 tokens
# sum(d$jev_input_token) * cost_token

## Check feed length
# dec |>
#   filter(source == "feed", !is.na(feed)) |>
#   mutate(feed_length = lengths(feed)) |>
#   count(feed_length)
