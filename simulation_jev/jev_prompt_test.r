# This script creates tests different prompt versions
# Goal: Monitor how different the jev probabilities turn out with different prompts
#       but also different calls with the same prompt

# Preliminaries: Source the functions and statement df
source("simulation_jev/jev_functions.r")
## Create test set of ten decisions
testdecisions <- read_parquet("MyFirstJevSim_backup.parquet") |>
  slice(sort(c(20, 53, 11779, 11783, 764, 763, 487, 448, 19993, 19992)))

# Explore different 1 agent prompts. Prompts differ in how state is delivered: with key "condition" or "pure".
#    In this case the worldview is added as list of statements as text
#    Or "structured" where state is delivered as an identity field plus an array of statements"

## Testset state versions
##   This is a bit hand made. Therefor commented out yet.
#    The testset_state has been produced 4 times. Then row_binded and saved.
# testset_state <- tibble(
#   state_type = c("condition", "pure", "structured"),
#   output = list(tibble())
# )
# testset_state[
#   testset_state$state_type == "condition",
#   "output"
# ] <- list(list(
#   testdecisions |>
#     df_mutate_jev_result(state_form = "condition") |>
#     df_unnest_jev_results()
# ))
# testset_state[
#   testset_state$state_type == "pure",
#   "output"
# ] <- list(list(
#   testdecisions |>
#     df_mutate_jev_result(state_form = "pure") |>
#     df_unnest_jev_results()
# ))
# testset_state[
#   testset_state$state_type == "structured",
#   "output"
# ] <- list(list(
#   testdecisions |>
#     df_mutate_jev_result(state_form = "structured") |>
#     df_unnest_jev_results()
# ))

# testset_state_cf <- testset_state |>
#   unnest(output) |>
#   arrange(time_step, agent_id, state_type) |>
#   select(-source, -feed, -posted, -incoming, -jev_model)
# testset_state_cf_4 <- testset_state_cf |> mutate(run = 4)

# cf <- bind_rows(testset_state_cf_1, testset_state_cf_2, testset_state_cf_3, testset_state_cf_4) |>
#   select(time_step, agent_id, state_type, run, everything()) |>
#   arrange(time_step, agent_id, state_type, run)
# write_parquet(cf, "jev_statetest.parquet")

jev_statetests <- read_parquet("jev_statetest.parquet")

# Compare:
#
# ## Prompt sending the 10 decisions out all together using
# one state with an array of agents and one noul question per candidate

# mult_input <- testdecisions |>
#   filter(map_int(shown_candidates, length) > 0)

do_jev_multtest <- function(testdecisions, state_form = "pure") {
  payload_mult <- create_payload_integrate_decision_mult(
    memory = testdecisions$memory_prompt,
    candidates = testdecisions$shown_candidates,
    statements = statements,
    openmindedness = NA,
    state_form = state_form
  )
  jev_mult <- call_jev(payload_mult)
  testdecisions_mult <- testdecisions |>
    mutate(
      jev_model = jev_mult$model,
      jev_input_token = jev_mult$usage$input_tokens / n(),
      jev_output_token = jev_mult$usage$output_tokens / n(),
      jev_probs = map2(
        seq_len(n()),
        shown_candidates,
        \(r, cands) {
          map_dbl(seq_along(cands), \(c) {
            jev_mult$answers[[paste0("Integrate_a", r, "_c", c)]]$noul
          })
        }
      )
    )
}

## This creates 4 multtests
# jev_multtest <- map(
#   1:4,
#   \(i) {
#     do_jev_multtest(testdecisions) |>
#       mutate(state_type = "pure", run = i, .after = agent_id)
#   }
# ) |>
#   list_rbind() |>
#   arrange(time_step, agent_id, run)
# jev_multtest |> write_parquet("jev_multtest.parquet")

jev_multtest <- read_parquet("jev_multtest.parquet")

jev_tests <- bind_rows(
  list(
    multtest = jev_multtest |>
      select(intersect(names(jev_multtest), names(jev_statetests))),
    statetests = jev_statetests |>
      select(intersect(names(jev_multtest), names(jev_statetests)))
  ),
  .id = "test_id"
)

# Diagnostic tables

## probability variation
jev_cf_probs <- jev_tests |>
  unnest(jev_probs) |>
  mutate(
    cand = 1:n(),
    .by = c(test_id, time_step, agent_id, state_type, run),
    .after = run
  ) |>
  select(time_step, agent_id, cand, test_id, state_type, run, jev_probs) |>
  arrange(time_step, agent_id, cand, state_type, run, test_id)
jev_cf_probs
jev_cf_probs |>
  # summarize(
  #   jev_probs = mean(jev_probs),
  #   .by = c(time_step, agent_id, cand, state_type, test_id)
  # ) |>
  ggplot(aes(
    x = interaction(time_step, agent_id, cand),
    y = jev_probs,
    color = test_id,
    shape = state_type
  )) +
  geom_point(size = 3, alpha = 0.5) +
  theme_minimal()


## token usage
jev_inputtoken <- jev_tests |>
  summarise(
    mean_jev_input_token = mean(jev_input_token),
    .by = c(test_id, state_type)
  )
jev_inputtoken
# Conclusion: multtest brings cost down to ~55%
