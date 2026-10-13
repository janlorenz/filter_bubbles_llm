library(tidyverse)
library(arrow)
library(scales)
library(patchwork)
library(igraph)

memory <- read_parquet("parquet/memory.parquet")
runs_core <- read_parquet("parquet/runs_core.parquet")
statements_meta <- read_parquet("parquet/statements_meta.parquet")
wvs <- read_parquet("parquet/wvs.parquet")

## PCAs
PCAall <- wvs |> na.omit() |> select(starts_with("Q")) |> prcomp(scale. = TRUE)
(PCAall$sdev[1])^2 / 100
save(PCAall, file = "parquet/PCAall.Rdata")
# EXPLORING
# wvs |> filter(B_COUNTRY_ALPHA=="UKR") |> na.omit() |> select(starts_with("Q")) |> prcomp() #prcomp(scale. = TRUE)
# check_zero_sd <- wvs |>
#   na.omit() |>
#   summarize(across(starts_with("Q"), sd), .by = B_COUNTRY_ALPHA)
# which(check_zero_sd == 0, arr.ind = TRUE)
wvs_standardized <- wvs |> # Do standardization independently of country PCAs
  na.omit() |>
  mutate(across(starts_with("Q"), \(x) x / sd(x)), .by = B_COUNTRY_ALPHA) |>
  mutate(across(starts_with("Q"), \(x) if_else(x == -Inf, 0, x))) # Account for two zero-variance cases in USA and UKR
PCAs <- wvs_standardized |>
  na.omit() |>
  select(B_COUNTRY_ALPHA) |>
  distinct() |>
  mutate(
    prcmp = B_COUNTRY_ALPHA |>
      map(\(x) {
        wvs_standardized |>
          filter(B_COUNTRY_ALPHA == x) |>
          select(starts_with("Q")) |>
          prcomp() #prcomp(scale. = TRUE) # check scale?
      }),
    expVarPC1 = prcmp |> map_dbl(\(x) x$sdev[1]^2 / 100),
    PC1 = prcmp |> map(\(x) x$rotation[, 1])
  )
PCAs |>
  ggplot(aes(expVarPC1, B_COUNTRY_ALPHA |> fct_reorder(expVarPC1))) +
  geom_col()
PC1s <- PCAs |>
  select(-prcmp) |>
  unnest(PC1) |>
  mutate(S_id = rep(row.names(PCAs$prcmp[[1]]$rotation), nrow(PCAs)))
PC1s |> ggplot(aes(PC1, S_id)) + geom_col() + facet_wrap(~B_COUNTRY_ALPHA)
PC1s |> write_parquet("PC1s.parquet")

## Overlap-base computations

# memory_run <- memory |> filter(t == 100, run_id == first(run_id))
# overlap_agents_run <- memory_run |>
#   select(S_id, A_id) |>
#   inner_join(memory_run, by = "S_id", relationship = "many-to-many") |>
#   filter(A_id.x < A_id.y) |> # avoid self-pairs & duplicate (a,b)/(b,a)
#   count(A_id.x, A_id.y, name = "weight") |>
#   mutate(
#     source = paste0("A", A_id.x),
#     target = paste0("A", A_id.y),
#     type = "AA",
#     weight,
#     .keep = "none"
#   )
# overlap_statements_run <- memory_run |>
#   select(S_id, A_id) |>
#   inner_join(memory_run, by = "A_id", relationship = "many-to-many") |>
#   filter(S_id.x < S_id.y) |> # avoid self-pairs & duplicate (a,b)/(b,a), this also works for character via lexicographic ordering
#   count(S_id.x, S_id.y, name = "weight") |>
#   rename(source = S_id.x, target = S_id.y)

# num_bub_agents <- function(memory_run) {
#   incidence <- memory_run |>
#     select(A_id, S_id) |>
#     mutate(present = 1L) |>
#     pivot_wider(
#       names_from = A_id,
#       values_from = present,
#       values_fill = 0L
#     ) |>
#     column_to_rownames("S_id") |>
#     as.matrix()
#   overlap <- incidence |> crossprod()
#   diag(overlap) <- 0
#   L <- diag(rowSums(overlap)) - overlap
#   ev <- L |> eigen(symmetric = TRUE) |> _$values |> sort()
#   plot(ev[1:4])
#   gaps <- ev |> diff()
#   gaps[1:4] |> which.max() # candidate number of bubbles
# }
# num_bub_stmts <- function(memory_run) {
#   incidence <- memory_run |>
#     select(A_id, S_id) |>
#     mutate(present = 1L) |>
#     pivot_wider(
#       names_from = S_id,
#       values_from = present,
#       values_fill = 0L
#     ) |>
#     column_to_rownames("A_id") |>
#     as.matrix()
#   overlap <- incidence |> crossprod() # raw shared-statement counts
#   deg <- colSums(incidence) # # statements per agent
#   cosine <- overlap / sqrt(outer(deg, deg)) # normalize
#   diag(cosine) <- 0
#   L <- diag(rowSums(cosine)) - cosine
#   ev <- L |> eigen(symmetric = TRUE) |> (\(x) x$values)() |> sort()
#   plot(ev[1:4])
#   gaps <- ev |> diff()
#   gaps[1:4] |> which.max() # candidate number of bubbles
# }
# num_bub_agents(memory_run)
# num_bub_stmts(memory_run)
# mm <- memory |>
#   filter(t %in% c(5, 10, 20, 50, 100)) |>
#   nest(.by = c(run_id, t)) |>
#   mutate(
#     num_bub_agents = data |> map_int(num_bub_agents),
#     num_bub_stmts = data |> map_int(num_bub_stmts)
#   ) |>
#   select(-data)

# runs_core |> left_join(mm, by = join_by(run_id))

# runs_core |>
#   left_join(mm, by = join_by(run_id)) |>
#   summarize(
#     mean_num_bub_agents = mean(num_bub_agents == 2),
#     mean_num_bub_stmts = mean(num_bub_stmts == 2),
#     n = n(),
#     sd_ag = sd(num_bub_agents == 2) / sqrt(n),
#     sg_st = sd(num_bub_stmts == 2) / sqrt(n),
#     .by = c(openmindedness, M, post)
#   )
