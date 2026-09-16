library(tidyverse)
library(arrow)
library(googlesheets4)
library(glue)
library(countrycode)

## Simulation runs overview
runs <- read_csv("rawdata/experiments/runs_config.csv") |>
  select(
    timestamp,
    seed = random_seed,
    N = num_agents,
    M = max_memory_per_agent,
    openmindedness = open_mindedness_level,
    tmax = num_time_steps,
    feed_posting,
    social_posting,
    model_name,
    run_id,
    run_folder
  ) |>
  mutate(
    model_short = word(model_name, sep = "/", start = 2) |>
      word(sep = "-", start = 1),
    post = paste0(
      ifelse(feed_posting == TRUE, "F", ""),
      ifelse(social_posting == TRUE, "S", "")
    ),
    timestamp = str_sub(timestamp, 3, 13),
    datetime = as.POSIXct(timestamp, format = "%y%m%d-%H%M", tz = "UTC"),
    run_id_desc = glue(
      "{timestamp}_A{N}_M{M}_tmax{tmax}_op{openmindedness}_{post}_seed{seed}_{model_short}"
    )
  )
runs_core <- runs |>
  filter(datetime >= "2026-03-25", model_short == "gpt") |>
  select(
    run_id_desc,
    run_id,
    N,
    M,
    openmindedness,
    post,
    seed,
    timestamp,
    model_name,
    run_folder,
    datetime
  )
runs_core |> write_parquet("parquet/runs_core.parquet")

# Make parquets of decisions, memory, and social networks
runs_core |>
  mutate(
    decisions = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/decisions.csv"),
        show_col_types = FALSE
      ) |>
        mutate(source_statement = as.character(source_statement))
    })
  ) |>
  pull(decisions) |>
  reduce(bind_rows) |>
  write_parquet("parquet/decisions.parquet")
runs_core |>
  mutate(
    memory = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/memory.csv"),
        show_col_types = FALSE
      )
    })
  ) |>
  pull(memory) |>
  reduce(bind_rows) |>
  write_parquet("parquet/memory.parquet")
runs_core |>
  mutate(
    social_network = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/social_network.csv"),
        show_col_types = FALSE
      )
    })
  ) |>
  pull(social_network) |>
  reduce(bind_rows) |>
  write_parquet("parquet/social_network.parquet")
runs_core |>
  mutate(
    statements = map(run_folder, \(f) {
      read_csv(
        glue("rawdata/experiments/{f}/statements.csv"),
        show_col_types = FALSE
      ) |>
        mutate(text = as.character(text))
    })
  ) |>
  pull(statements) |>
  reduce(bind_rows) |>
  write_parquet("parquet/statements.parquet")

## World Value Survey Data and Meta Data, Generated Statements selection and text
### Data/Documentation Link: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp
load("rawdata/wvs_data/WVS_Cross-National_Wave_7_Rdata_v6_0.rdata")
wvs_full <- `WVS_Cross-National_Wave_7_v6_0` |> as_tibble()
statements_meta <- read_sheet("1t-_1Q5i-Pij9axCR2IjZ54CCk-KlfUnyKQbGP2vcAYY") |>
  filter(Selection_ABMLLM == 1)
wvs_used <- wvs_full |>
  select(!!!statements_meta$Question_ID) |>
  mutate(across(everything(), \(x) ifelse(x < 0, NA, x)))
statements_meta_scale_qs <- statements_meta |> filter(!is.na(min_approval))
statements_meta_choice_qs <- statements_meta |> filter(is.na(min_approval))
wvs <- bind_cols(
  purrr::map_dfc(seq_len(nrow(statements_meta_scale_qs)), \(i) {
    tibble(
      !!statements_meta_scale_qs$S_id[i] := 2 *
        ((wvs_used[[statements_meta_scale_qs$Question_ID[i]]] -
          statements_meta_scale_qs$min_approval[i]) /
          (statements_meta_scale_qs$max_approval[i] -
            statements_meta_scale_qs$min_approval[i]) -
          0.5)
    )
  }),
  purrr::map_dfc(seq_len(nrow(statements_meta_choice_qs)), \(i) {
    tibble(
      !!statements_meta_choice_qs$S_id[i] := 2 *
        (as.numeric(
          wvs_used[[statements_meta_choice_qs$Question_ID[
            i
          ]]] ==
            statements_meta_choice_qs$max_approval[i]
        ) -
          0.5)
    )
  })
) |>
  bind_cols(wvs_full |> select(A_YEAR, B_COUNTRY_ALPHA, B_COUNTRY)) |>
  mutate(
    countryname = if_else(
      B_COUNTRY_ALPHA == "NIR",
      "Northern Ireland",
      countrycode(
        B_COUNTRY_ALPHA,
        origin = "iso3c",
        destination = "country.name.en",
        warn = FALSE
      )
    )
  )
wvs |> write_parquet("parquet/wvs.parquet")
statements_meta |> write_parquet("parquet/statements_meta.parquet")
