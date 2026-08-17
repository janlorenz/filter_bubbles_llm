library(tidyverse)
library(arrow)
library(googlesheets4)
library(scales)
library(patchwork)
library(igraph)

memory <- read_parquet("parquet/memory.parquet")
runs_core <- read_parquet("parquet/runs_core.parquet")

load("rawdata/wvs_data/WVS_Cross-National_Wave_7_Rdata_v6_0.rdata")
wvs_full <- `WVS_Cross-National_Wave_7_v6_0` |> as_tibble()
wvs_meta <- read_sheet("1t-_1Q5i-Pij9axCR2IjZ54CCk-KlfUnyKQbGP2vcAYY") |>
  filter(Selection_ABMLLM == 1)
wvs_used <- wvs_full |>
  select(!!!wvs_meta$Question_ID) |>
  mutate(across(everything(), \(x) ifelse(x < 0, NA, x)))
wvs_meta_scale_qs <- wvs_meta |> filter(!is.na(min_approval))
wvs_meta_choice_qs <- wvs_meta |> filter(is.na(min_approval))
wvs <- bind_cols(
  purrr::map_dfc(seq_len(nrow(wvs_meta_scale_qs)), \(i) {
    tibble(
      !!wvs_meta_scale_qs$S_id[i] := 2 *
        ((wvs_used[[wvs_meta_scale_qs$Question_ID[i]]] -
          wvs_meta_scale_qs$min_approval[i]) /
          (wvs_meta_scale_qs$max_approval[i] -
            wvs_meta_scale_qs$min_approval[i]) -
          0.5)
    )
  }),
  purrr::map_dfc(seq_len(nrow(wvs_meta_choice_qs)), \(i) {
    tibble(
      !!wvs_meta_choice_qs$S_id[i] := 2 *
        (as.numeric(
          wvs_used[[wvs_meta_choice_qs$Question_ID[
            i
          ]]] ==
            wvs_meta_choice_qs$max_approval[i]
        ) -
          0.5)
    )
  })
) |>
  bind_cols(wvs_full |> select(B_COUNTRY_ALPHA))

## PCA WVS
PCAwvs <- wvs |>
  filter(B_COUNTRY_ALPHA == "CAN") |>
  select(-B_COUNTRY_ALPHA) |>
  na.omit() |>
  prcomp()
sttmts <- wvs_meta |>
  left_join(
    PCAwvs$rotation |>
      as_tibble(rownames = "S_id") |>
      select(S_id, PC1) |>
      mutate(CAN_PC1 = PC1 / max(PC1)),
    by = join_by(S_id)
  )
sttmts |> ggplot(aes(x = WVS_PC1, y = CAN_PC1)) + geom_point()
write_parquet(sttmts, "sttmts.parquet")

## Popular Statements Sim at t=100
stats_pop <- memory |>
  left_join(runs_core, by = join_by(run_id)) |>
  left_join(wvs_meta, by = join_by(S_id)) |>
  filter(t == 100, post == "FS") |>
  count(run_id, S_id, Worldview_Statement, M) |>
  summarize(
    popularity = sum(n) / mean(M),
    .by = c(S_id, Worldview_Statement, M)
  ) |>
  arrange(M, desc(popularity)) |>
  mutate(
    S_id = fct_inorder(S_id) |> fct_rev(),
    Worldview_Statement = fct_inorder(Worldview_Statement) |> fct_rev()
  ) |>
  left_join(sttmts |> select(S_id, worldview), by = join_by(S_id)) |>
  pivot_wider(names_from = M, values_from = popularity) |>
  mutate(Total_pop = `5` + `10`) |>
  arrange(desc(Total_pop)) |>
  pivot_longer(c(`5`, `10`))
write_parquet(stats_pop, "stats_pop.parquet")

read_parquet("stats_pop.parquet") |>
  pivot_wider(names_from = M, values_from = popularity) |>
  mutate(Total_pop = `5` + `10`) |>
  arrange(desc(Total_pop)) |>
  select(-Total_pop) |>
  slice(1:15) |>
  pivot_longer(c(`5`, `10`)) |>
  mutate(name = paste("Memory", name)) |>
  ggplot(aes(value, Worldview_Statement, fill = worldview)) +
  geom_col() +
  facet_wrap(~name) +
  labs(y = "", x = "") +
  theme_minimal()

read_parquet("sttmts.parquet") |>
  arrange(desc(CAN_PC1)) |>
  mutate(Worldview_Statement = fct_inorder(Worldview_Statement)) |>
  ggplot(aes(CAN_PC1, Worldview_Statement, fill = worldview)) +
  geom_col() +
  theme_minimal()


## Overlap-base computations

memory_run <- memory |> filter(t == 100, run_id == first(run_id))
overlap_agents_run <- memory_run |>
  select(S_id, A_id) |>
  inner_join(memory_run, by = "S_id", relationship = "many-to-many") |>
  filter(A_id.x < A_id.y) |> # avoid self-pairs & duplicate (a,b)/(b,a)
  count(A_id.x, A_id.y, name = "weight") |>
  mutate(
    source = paste0("A", A_id.x),
    target = paste0("A", A_id.y),
    type = "AA",
    weight,
    .keep = "none"
  )
overlap_statements_run <- memory_run |>
  select(S_id, A_id) |>
  inner_join(memory_run, by = "A_id", relationship = "many-to-many") |>
  filter(S_id.x < S_id.y) |> # avoid self-pairs & duplicate (a,b)/(b,a), this also works for character via lexicographic ordering
  count(S_id.x, S_id.y, name = "weight") |>
  rename(source = S_id.x, target = S_id.y)

num_bub_agents <- function(memory_run) {
  incidence <- memory_run |>
    select(A_id, S_id) |>
    mutate(present = 1L) |>
    pivot_wider(
      names_from = A_id,
      values_from = present,
      values_fill = 0L
    ) |>
    column_to_rownames("S_id") |>
    as.matrix()
  overlap <- incidence |> crossprod()
  diag(overlap) <- 0
  L <- diag(rowSums(overlap)) - overlap
  ev <- L |> eigen(symmetric = TRUE) |> _$values |> sort()
  plot(ev[1:4])
  gaps <- ev |> diff()
  gaps[1:4] |> which.max() # candidate number of bubbles
}
num_bub_stmts <- function(memory_run) {
  incidence <- memory_run |>
    select(A_id, S_id) |>
    mutate(present = 1L) |>
    pivot_wider(
      names_from = S_id,
      values_from = present,
      values_fill = 0L
    ) |>
    column_to_rownames("A_id") |>
    as.matrix()
  overlap <- incidence |> crossprod() # raw shared-statement counts
  deg <- colSums(incidence) # # statements per agent
  cosine <- overlap / sqrt(outer(deg, deg)) # normalize
  diag(cosine) <- 0
  L <- diag(rowSums(cosine)) - cosine
  ev <- L |> eigen(symmetric = TRUE) |> (\(x) x$values)() |> sort()
  plot(ev[1:4])
  gaps <- ev |> diff()
  gaps[1:4] |> which.max() # candidate number of bubbles
}
num_bub_agents(memory_run)
num_bub_stmts(memory_run)
mm <- memory |>
  filter(t %in% c(5, 10, 20, 50, 100)) |>
  nest(.by = c(run_id, t)) |>
  mutate(
    num_bub_agents = data |> map_int(num_bub_agents),
    num_bub_stmts = data |> map_int(num_bub_stmts)
  ) |>
  select(-data)

runs_core |> left_join(mm, by = join_by(run_id))

runs_core |>
  left_join(mm, by = join_by(run_id)) |>
  filter(t == 100, post == "FS") |>
  summarize(
    mean_num_bub_agents = mean(num_bub_agents == 2),
    mean_num_bub_stmts = mean(num_bub_stmts == 2),
    n = n(),
    sd_ag = sd(num_bub_agents == 2) / sqrt(n),
    sg_st = sd(num_bub_stmts == 2) / sqrt(n),
    .by = c(openmindedness, M)
  )

# PCAplot <- function(
#   pr
# ) {
#   rel_pcs <- sum(pr$sdev > 1)
#   g1 <- tibble(
#     explVar = pr$sdev^2 / sum(pr$sdev^2),
#     cumexplVar = cumsum(pr$sdev^2 / sum(pr$sdev^2))
#   ) |>
#     ggplot(aes(x = 1:length(explVar))) +
#     geom_col(aes(y = explVar)) +
#     geom_point(aes(y = cumexplVar)) +
#     geom_line(aes(y = cumexplVar)) +
#     geom_hline(yintercept = 1 / length(pr$sdev), color = "red") +
#     scale_y_continuous(labels = percent_format(accuracy = 1)) +
#     labs(
#       x = "Principal Component",
#       y = "Explained Variance",
#       #title = glue("{rel_pcs} PCs explain more variance than average")
#     ) +
#     theme_minimal() +
#     theme(plot.title.position = "plot")
#   g2 <- pr$rotation |>
#     as_tibble() |>
#     select(PC1, PC2) |>
#     mutate(Topic = row.names(pr$rotation)) |>
#     # mutate(Topic = fct_inorder(pr$rotation |> row.names()) |> fct_rev()) |>
#     # pivot_longer(starts_with("PC"), names_to = "PC", values_to = "Loading") |>
#     #mutate(PC = fct_inorder(PC)) |>
#     arrange(PC1) |>
#     mutate(Topic = fct_inorder(Topic)) |>
#     pivot_longer(starts_with("PC"), names_to = "PC", values_to = "Loading") |>
#     ggplot(aes(x = Loading, y = Topic)) +
#     geom_col() +
#     facet_wrap(~PC) +
#     labs(
#       x = "Loading",
#       y = ""
#     ) +
#     theme_minimal()
#   # g3 <- pr$x |>
#   #   as_tibble() |>
#   #   ggplot(aes(x = PC1, y = PC2, color = {{ color_by }})) +
#   #   geom_point(alpha = 1) +
#   #   labs(
#   #     x = "PC1",
#   #     y = "PC2",
#   #     color = color_by_label
#   #   ) +
#   #   theme_minimal() +
#   #   theme(legend.position = "left")
#   # (g1 + g2 + plot_layout(widths = c(1, 5))) /
#   #   wrap_elements(g3 |> ggMarginal(type = "histogram", groupFill = TRUE)) +
#   #   plot_layout(heights = plot_heights)
#   g1 + g2
# }
# PCAplot(PCAwvs)
