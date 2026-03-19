library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

summarise_pct <- function(.data, col, pattern, category, distinct_values = FALSE) {
  apply_distinct <- list(dplyr::distinct, identity)[[as.integer(!distinct_values) + 1]]
  
  .data |>
    select({{ col }}) |>
    apply_distinct() |>
    summarise(
      n_total = n(),
      n_has = sum(str_detect({{ col }}, pattern), na.rm = TRUE),
      n_no  = n_total - n_has,
      pct_has = n_has / n_total,
      pct_no  = n_no  / n_total
    ) |>
    transmute(
      Category = category,
      `TRUE`  = pct_has,
      `FALSE` = pct_no
    ) |>
    pivot_longer(
      cols = c(`TRUE`, `FALSE`),
      names_to = "is_doi",
      values_to = "value"
    ) |>
    mutate(
      is_doi = is_doi == "TRUE",
      value_pct = percent(value, accuracy = 0.1)
    )
}

doi_pattern        <- fixed("doi")
starts_10_pattern  <- regex("^10\\.")

pie_tbl <-
  bind_rows(
    summarise_pct(
      DCC_corpus_orig,
      dataset,
      doi_pattern,
      category = "DCC Raw by cases",
      distinct_values = FALSE
    ),
    summarise_pct(
      DCC_corpus_orig,
      dataset,
      doi_pattern,
      category = "DCC Raw by datasets",
      distinct_values = TRUE
    ),
    summarise_pct(
      DCC_corpus_11_std_lbl,
      dataset_for_matching,
      starts_10_pattern,
      category = "DCC Clean by cases",
      distinct_values = FALSE
    ),
    summarise_pct(
      DCC_corpus_11_std_lbl,
      dataset_for_matching,
      starts_10_pattern,
      category = "DCC Clean by datasets",
      distinct_values = TRUE
    )
  ) |>
  mutate(
    Category = factor(
      Category,
      levels = c(
        "DCC Raw by cases",
        "DCC Raw by datasets",
        "DCC Clean by cases",
        "DCC Clean by datasets"
      )
    )
  )

# Pie charts (2x2 facet)
pie_tbl |>
  ggplot(aes(x = "", y = value, fill = is_doi)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Category, ncol = 2) +
  geom_text(
    aes(label = value_pct),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE,
    size = 5
  ) +
  theme_void() +
  labs(fill = "is_doi") +
  scale_fill_manual(
  values = c("TRUE" = "#56B4E9", "FALSE" = "#FCA5A5"),
  labels = c("TRUE" = "Yes", "FALSE" = "No")
  ) +
theme(
  strip.text = element_text(size = 14),
  strip.text.x = element_text(size = 14),
  strip.text.y = element_text(size = 14),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14)
)
  