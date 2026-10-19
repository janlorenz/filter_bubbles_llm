source("simulation_jev/simulation_jev_functions.r")

statements <- read_parquet("parquet/statements_meta.parquet") |>
  select(S_id, text = Worldview_Statement)

dec <- prepare_data_sim_run(
  "260326-112135_A100_M10_tmax100_op1_FS_seed355_gpt"
)


# sdec$source[1] <- "social"
# sdec$incoming <- dec$incoming[101:200]

sim_agent_timestep <- function(decs, t, s) {
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
  decs <- decs |>
    mutate(
      jev_result = map2(
        memory_prompt,
        shown_candidates,
        \(mem, cand) {
          if (length(cand) == 0) {
            NULL
          } else {
            payload_integrate_decision(mem, cand, statements) |> call_jev()
          }
        }
      )
    ) |>
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
    select(-jev_result) |>
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
  verbose = TRUE,
  checkpoint_path = NULL,
  checkpoint_every = 1,
  resume = FALSE,
  start_t = NULL
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
    dec_feed <- sim_agent_timestep(dec, t, "feed")
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
    dec_social <- sim_agent_timestep(dec, t, "social")
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
checkpoint_file <- "MyFirstJevSim.parquet"
d <- run_simulation(
  dec,
  tmax = 100,
  checkpoint_path = checkpoint_file,
  checkpoint_every = 1,
  resume = file.exists(checkpoint_file),
  start_t = NULL # e.g. set to 23 to force a manual rollback
)
write_parquet(d, checkpoint_file)

d <- read_parquet("MyFirstJevSim_backup.parquet")
cost_token <- 0.042 / 10^6
d$jev_input_token |> sum(na.rm = TRUE) * cost_token

# # Jev cost: $0.042 per 1,000,000 tokens
# sum(d$jev_input_token) * cost_token

## Check feed length
# dec |>
#   filter(source == "feed", !is.na(feed)) |>
#   mutate(feed_length = lengths(feed)) |>
#   count(feed_length)
