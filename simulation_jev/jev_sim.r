source("simulation_jev/jev_functions.r")

# Run a batch of simulations based on runs_core subset with added jev parameters
# Goal encode info in filename analog to run_id_desc
# Note: Initial memories are identical for same seed w.r.t. M, S/F/FS, op runs differ in feed and maybe order of incoming
#
# Replace:
#     ac (for probability_acceptance_threshold) replaces op (for openmindedness)
#     gpt -> jev (the exact model name goes into variable model_name)
# Remove:
#     run_id (only use run_id_desc)
#     run_folder (run_id_desc will be filename)

runs_core_jev <- read_parquet("parquet/runs_core.parquet") |>
  filter(post != "Ffix" & openmindedness == 1) |>
  mutate(run_id_origin = run_id_desc, .after = run_id_desc) |>
  cross_join(tibble(
    probability_acceptance_threshold = c(0, 0.25, 0.5, 0.75)
  )) |>
  select(run_id_desc:M, probability_acceptance_threshold, everything()) |>
  select(-openmindedness, -run_id, -run_folder) |>
  mutate(tmax = as.integer(100), .after = M) |>
  mutate(
    datetime = "1970-01-01" |> as_datetime(),
    model_short = "jev",
    model_name = "jev-1.14.0"
  ) |>
  mutate(
    sameseed_id = as.integer(1),
    .after = seed
  ) |>
  mutate(
    run_id_desc = glue(
      "{timestamp}_A{N}_M{M}_tmax{tmax}_ac{100*probability_acceptance_threshold}_{post}_seed{seed}_{sameseed_id}_{model_short}"
    )
  ) |>
  select(-model_short)

runs_core_jev_batch1 <- runs_core_jev |>
  filter(post != "FS") |>
  group_by(post) |>
  slice(1:4)

# run simulations of batch1

# for (i in 1:nrow(runs_core_jev_batch1)) {
for (i in 1:2) {
  if (
    !file.exists(paste0(
      "simulation_jev/runs/",
      runs_core_jev_batch1$run_id_desc[i],
      ".parquet"
    ))
  ) {
    dec <- prepare_data_sim_run(runs_core_jev_batch1$run_id_origin[i])
    dec_sim <- run_simulation(
      dec,
      max_memory = runs_core_jev_batch1$M[i],
      probability_acceptance_threshold = runs_core_jev_batch1$probability_acceptance_threshold[
        i
      ],
      use_mult = TRUE,
      batch_size = 100,
      model = runs_core_jev_batch1$model_name[i]
    )
    dec_sim |>
      write_parquet(paste0(
        "simulation_jev/runs/",
        runs_core_jev_batch1$run_id_desc[i],
        ".parquet"
      ))
    runs_core_jev_batch1[i, "datetime"] <- now() |> as_datetime()
    runs_core_jev_batch1 |>
      write_parquet(paste0(
        "simulation_jev/runs/runs_core_jev_batch1.parquet"
      ))
  }
}

# dec1 <- prepare_data_sim_run(
#   "260701-102054_A100_M5_tmax100_op1_FS_seed355_gpt"
# )
# dec2 <- prepare_data_sim_run(
#   "260917-115644_A100_M5_tmax100_op1_S_seed400_gpt"
# )

# r <- "260917-115644_A100_M5_tmax100_op1_S_seed400_gpt"
# r <- "260814-165024_A100_M10_tmax100_op1_F_seed400_gpt"

# dec1 |> filter(time_step == 1) |> pull(memory_prompt) |> as_vector()
# dec2 |> filter(time_step == 1) |> pull(memory_prompt) |> as_vector()

# Guide to checkpointing:
# Single entry point: if a checkpoint already exists on disk, resume from it
# (auto-detecting the first incomplete time step) instead of restarting from
# t = 1. This makes re-sourcing the script after a hang/crash safe.
# To force a manual rollback (e.g. redo from t = 23 because you don't trust
# the last checkpointed step), set start_t explicitly below.

# checkpoint_file <- "MyJevSim_maxbatch_prob065_jev-1.14.0.parquet"
# d <- run_simulation(
#   dec,
#   max_memory = 5, # M of the run in dec (S seed354 M5)
#   probability_acceptance_threshold = 0.65,
#   checkpoint_path = checkpoint_file,
#   checkpoint_every = 1,
#   resume = file.exists(checkpoint_file),
#   start_t = NULL, # e.g. set to 23 to force a manual rollback
#   state_form = "pure", # "pure", "condition" or "structured"
#   use_mult = TRUE, # batched jev calls per time step and source
#   batch_size = 100,
#   max_tokens = 55000, # estimated input tokens per call; API limit is 64k
#   model = "jev-1.14.0" # or e.g. "jev-1.13.0" to pin a version
# )
# write_parquet(d, checkpoint_file)

# d <- read_parquet(
#   "simulation_jev/testsimdata/MyJevSim_batch20_jev-1.14.0.parquet"
# )
# cost_token <- 0.042 / 10^6
# d$jev_input_token |> sum(na.rm = TRUE) * cost_token
# d$jev_model |> first()
# # Jev cost: $0.042 per 1,000,000 tokens
# sum(d$jev_input_token) * cost_token
