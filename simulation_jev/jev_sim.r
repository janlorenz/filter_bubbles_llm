source("simulation_jev/jev_functions.r")

# Run a batch of simulations based on runs_core subset with added jev parameters
# Goal encode info in filename analog to run_id_desc
# 
# Replace: 
#     ac (for probability_acceptance_threshold) replaces op (for openmindedness)
#     gpt -> jev (the exact model name goes into variable model_name)
runs_core <- read_parquet("parquet/runs_core.parquet")

dec1 <- prepare_data_sim_run(
  "260701-102054_A100_M5_tmax100_op1_FS_seed355_gpt"
)
dec2 <- prepare_data_sim_run(
  "260917-115644_A100_M5_tmax100_op1_S_seed400_gpt"
)

r <-    "260917-115644_A100_M5_tmax100_op1_S_seed400_gpt"


dec1 |> filter(time_step==1) |> pull(memory_prompt) |> as_vector()
dec2 |> filter(time_step==1) |> pull(memory_prompt) |> as_vector()

# !!! Code seems to work only when started with feed posting not with socialposting only?








# Guide to checkpointing:
# Single entry point: if a checkpoint already exists on disk, resume from it
# (auto-detecting the first incomplete time step) instead of restarting from
# t = 1. This makes re-sourcing the script after a hang/crash safe.
# To force a manual rollback (e.g. redo from t = 23 because you don't trust
# the last checkpointed step), set start_t explicitly below.
checkpoint_file <- "MyJevSim_maxbatch_prob065_jev-1.14.0.parquet"
d <- run_simulation(
  dec,
  probability_acceptance_threshold = 0.65,
  checkpoint_path = checkpoint_file,
  checkpoint_every = 1,
  resume = file.exists(checkpoint_file),
  start_t = NULL, # e.g. set to 23 to force a manual rollback
  state_form = "pure", # "pure", "condition" or "structured"
  use_mult = TRUE, # batched jev calls per time step and source
  batch_size = 100, 
  max_tokens = 55000, # estimated input tokens per call; API limit is 64k
  model = "jev-1.14.0" # or e.g. "jev-1.13.0" to pin a version
)
write_parquet(d, checkpoint_file)




d <- read_parquet("simulation_jev/testsimdata/MyJevSim_batch20_jev-1.14.0.parquet")
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
