# This script creates variables that could then be referenced in index.qmd


# 1. Setup ----------------------------------------------------------------

Sys.setenv(LANG = "EN")  # Set environment language to English

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, readxl, lubridate, stringi, fs)

save_cr <- function(..., file) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  save(..., file = file)
} # wrapper for save() with automatic directory creation

write_csv_cr <- function(x, file, ...) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  write.csv(x, file = file, ...)
} # wrapper for write.csv() with automatic directory creation

# fucntion to load latest metadata version

load_latest_metadata_update <- function() {
  # Define target directory using here()
  rda_dir <- here("data", "verification", "metadata all", "datasets_metadata_master_updated", "rda")
  
  # List all .RData files
  files <- list.files(rda_dir, pattern = "\\.RData$", full.names = TRUE)
  
  # Extract numeric suffixes from filenames
  suffixes <- sub(".*_([0-9]+)\\.RData$", "\\1", files)
  suffixes_num <- as.integer(suffixes)
  
  # Find file with highest suffix
  max_index <- which.max(suffixes_num)
  latest_file <- files[max_index]
  
  # Load the file
  loaded_vars <- load(latest_file, envir = .GlobalEnv)
  
  # Extract and print the base file name without extension
  file_base <- tools::file_path_sans_ext(basename(latest_file))
  current_suffix <- sub(".*_(\\d+)$", "\\1", file_base)
  next_suffix <- sprintf("%03d", as.integer(current_suffix) + 1)
  next_file_base <- sub("_(\\d+)$", paste0("_", next_suffix), file_base)
  message("✅ Loaded: ", file_base, "\nNext file version: ", next_file_base)
  
  # Return the object invisibly
  invisible(get(loaded_vars[1], envir = .GlobalEnv))
}

# 2. Get results files ----------------------------------------------------

# Load relevant tables to extract variables from

# 1. Metadata

metadata <- load_latest_metadata_update()

metadata_no_da <- metadata |>
  dplyr::filter(source_charite != "data_articles") |>
  dplyr::filter(dataset_for_matching != "10.18112/openneuro.ds001226") # remove this overlapping case from Numbat cases, as it belongs to Data Articles

# 2. Matched

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_7_w_metadata.RData"))

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_8_dedup.RData"))

detected_dedup <- dcc_detected_ids_all_sources_8_dedup
detected_dup <- dcc_detected_ids_all_sources_7_w_metadata

detected_dedup_no_da <- dcc_detected_ids_all_sources_8_dedup |> dplyr::filter(source_charite != "data_articles")

# 3. For matching

## 3.1 Numbat

load(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_9_clean_pairs.RData"))

num_for_match <- numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(source == "numbat") |> 
  dplyr::filter(dataset_for_matching != "10.18112/openneuro.ds001226") |> # remove this overlapping case from Numbat cases, as it belongs to Data Articles
  dplyr::filter(!stringr::str_detect(data_identifier, "1\\.0\\.1")) # 7 identifiers of the same dataset
  

## 3.2 Additional IDs

load(here("data", "wrangling_steps", "datastet", "added_and_ds_for_matching_4_rm_exist.RData"))

ad_for_match <- added_and_ds_for_matching_4_rm_exist |> 
  dplyr::filter(source == "additional_ids") |> 
  distinct()

ad_non_matched <- ad_for_match |> 
  dplyr::filter(!dataset_for_matching %in% detected_dedup$detected_id) |> 
  select(dataset_for_matching) |> 
  distinct()

# 4. Analyses

# 4.1 Chi square matched: non-matched analysis

load(here("data", "tables_for_plots", "summary_model_chi.RData"))
load(here("data", "tables_for_plots", "anova_result.RData"))
load(here("data", "tables_for_plots", "odds_ratios.RData"))

# 4.2 GLM: age-citation relationship

load(here("data", "tables_for_plots", "summary_model_glm.RData"))

# 5. Data Articles

load(here("data", "tables_for_plots", "counts_ids_and_dois_da.RData")) # counts of flowchart

load(here("data", "wrangling_steps", "data_articles", "data_articles_dois_v10.RData")) # for number of reusing papers

# 6. DCC
load(here("data", "wrangling_steps", "dcc", "DCC_corpus_11_std_lbl.RData")) # DCC wrangled
load(here("data", "inputs_for_quick_render", "doi_is_id_count.RData")) # DCC wrangled

# clean up
rm(dcc_detected_ids_all_sources_7_w_metadata,
   dcc_detected_ids_all_sources_8_dedup,
   numbat_da_dois_and_ids_9_clean_pairs,
   added_and_ds_for_matching_4_rm_exist,
   datasets_metadata_master_updated_021)

# 3. Build results tables --------------------------------------------------

# 1. Counts

res <- tibble(
  value = c(
    "placeholder",
    "total_published_charite_dois",            # All published Charite papers 2020-2023
    "ds_matched_ids_count_dist",               # DS   ids dist detected
    "n_matched_ids_count_dist",                # N+AD ids dist detected      
    "all_matched_ids_count_dist",              # All  ids dist detected
    
    "ds_non_matched_ids_count_dist",           # DS   ids dist not
    "n_non_matched_ids_count_dist",            # N+AD ids dist not

    "max_mentions_of_single_id",               # All  max-ref of 1 id
    "all_ids_with_1_mention_count_dist",       # All  ids with 1 mention
    "n_ids_with_1_mention_count_dist",         # N    ids with 1 mention
    
    "n_ids_for_match_count",                   # N+AD ids for matching
    "n_2nd_for_match_count",                   # N    2nd ids for matching

    "n_dois_for_match",                        # N    dois for matching
    "ad_dois_for_match",                       # AD   dois for matching
    
    "n_mentions",                              # N    refs dist detected
    "ds_mentions",                             # DS   refs dist detected
    "all_mentions",                            # All  refs dist detected
     
    "all_mentions_geo",                        # All  refs dist detected in GEO
    "all_mentions_mendeley",                   # All  refs dist detected in mendeley
    "all_mentions_emdb",                       # All  refs dist detected in general purpose / disciplinary
    
    "all_mentions_gen_and_discp",               # All  refs dist detected in emdb

    "human_data",                              # All  ids dist detected human-data
    
    "all_matched_charite_dois",                # All  Charite's dois dist detected
    
    "all_matched_dcc_dois",                    # All  Mentioning dois dist detected
    
    "da_dois",                                 # DA charite's data articles
    "da_ids",                                  # DA charite's datasets
    "da_mentions",                             # DA confirmed citations
    "da_citing_papers",                        # DA number of reusing papers     
    
    "dcc_datasets",                            # Number of dcc_datasets (in the corpus)
    "dcc_mentions",                            # Number of dcc_mentions (in the corpus)
    "dcc_rep",                                 # Number of dcc_repositories (in the corpus)
    "dcc_journals",                            # Number of dcc_journals (in the corpus)
    "dcc_papers",                              # Number of dcc_papers (in the corpus)
    "dcc_doi_is_dataset",                      # Number of dcc_doi = dcc_dataset (in the corpus)
    "human_odd_r",                             # human_odd_r (odds_ratio)
    "covid_odd_r",                             # covid_odd_r (odds_ratio)
    "datasets_20_23"                           # Number of datasets published between 2020-2023
    ),
  count = c(
    
    # placeholder
    999,
    
    # total_published_charite_dois (2020-2023)
    4457+4924+2171+4196,
    
    # ds_matched_ids_count_dist
    detected_dup |> dplyr::filter(source_charite == "datastet") |> select(detected_id) |> distinct() |> nrow(),

    # n_matched_ids_count_dist
    detected_dedup |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> select(detected_id) |> distinct() |> nrow(),
    
    # all_matched_ids_count_dist
    detected_dedup_no_da |> select(detected_id) |> distinct() |> nrow(),
    
    # ds_non_matched_ids_count_dist (sanity check: returns 0)
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite == "datastet") |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # n_non_matched_ids_count_dist (including additional ids that weren't detected)
    (num_for_match |> 
      dplyr::filter(!dataset_for_matching %in% detected_dedup$detected_id) |>
      select(dataset_for_matching) |> 
      dplyr::filter(!is.na(dataset_for_matching)) |> 
      distinct() |> 
      nrow()) + (ad_non_matched |> nrow()) - 2, # minus 2 because 2 of the matched are secondary ids, so they need to be removed from the non-matched couunt here

    # max_mentions_of_single_id
    detected_dedup_no_da |>
      distinct(detected_id, doi_dcc) |>
      group_by(detected_id) |> 
      summarise(n=n()) |> 
      arrange(desc(n)) |> 
      slice(1) |>
      pull(n),
    
    # all_ids_with_1_mention_count_dist
    detected_dedup_no_da |>
      distinct(detected_id, doi_dcc) |>
      group_by(detected_id) |> 
      summarise(n=n()) |>
      dplyr::filter(n == "1") |>
      nrow(),
    
    # n_ids_with_1_mention_count_dist
    detected_dedup_no_da |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> 
      distinct(detected_id, doi_dcc) |>
      group_by(detected_id) |> 
      summarise(n=n()) |>
      dplyr::filter(n == "1") |>
      nrow(),
    
    # n_ids_for_match_count (including additional)
    (num_for_match |>
      select(dataset_for_matching) |>
      dplyr::filter(!is.na(dataset_for_matching)) |>
      distinct() |> 
      nrow()) + (ad_for_match |> 
                   select(dataset_for_matching) |> 
                   dplyr::filter(!is.na(dataset_for_matching)) |>
                   distinct() |> 
                   nrow()),
    
    # n_2nd_for_match_count
    num_for_match |> select(data_id_secondary) |> dplyr::filter(!is.na(data_id_secondary)) |> distinct() |> nrow(),
    
    # n_dois_for_match
    num_for_match |> select(doi_no_ver_info) |> distinct() |> nrow(),
    
    # ad_dois_for_match (They are included in n_dois_for_match)
    ad_for_match |> dplyr::filter(source == "additional_ids") |> select(doi) |> distinct() |> nrow(),
    
    # n_mentions
    detected_dedup |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # ds_mentions (no overlap)   
    detected_dedup |> dplyr::filter(source_charite == "datastet") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions (no overlap)  
    detected_dedup_no_da |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_geo
    detected_dedup_no_da |> dplyr::filter(repository == "gene expression omnibus (geo)") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_mendeley
    detected_dedup_no_da |> dplyr::filter(repository == "mendeley") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_emdb
    detected_dedup_no_da |> dplyr::filter(repository == "the electron microscopy data bank (emdb)") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_gen_and_discp
    detected_dedup_no_da |> dplyr::filter(repository %in% c(
      "the electron microscopy data bank (emdb)",
      "openneuro",
      "zenodo",
      "figshare",
      "mendeley",
      "harvard dataverse",
      "apollo - university of cambridge repository")) |>
      distinct(detected_id, doi_dcc) |> nrow(),
    
    # human_data ids
    detected_dedup_no_da |> dplyr::filter(human_data == "TRUE") |> select(detected_id) |> distinct() |> nrow(),
    
    # all_matched_charite_dois 
    detected_dedup_no_da |> select(doi_no_ver_info) |> distinct() |> nrow(),
    
    # all_matched_dcc_dois
    detected_dedup_no_da |> select(doi_dcc) |> distinct() |> nrow(),
    
    # da_dois" (DA charite's data articles)
    counts_ids_and_dois_da |> dplyr::filter(category == "da_doi") |> dplyr::pull(count) |> unlist() |> as.integer(),
    
    # da_ids" (DA charite's datasets)
    counts_ids_and_dois_da |> dplyr::filter(category == "da_datasets") |> dplyr::pull(count) |> unlist() |> as.integer(),
    
    # da_mentions" (DA confirmed citations)
    counts_ids_and_dois_da |> dplyr::filter(category == "n_citations") |> dplyr::pull(count) |> unlist() |> as.integer(),
    
    # da_citing_papers" (DA number of reusing papers)
    counts_ids_and_dois_da |> dplyr::filter(category == "reusing_papaers") |> dplyr::pull(count) |> unlist() |> as.integer(),
    
    # dcc_datasets Number of dcc_datasets (in the corpus)
    DCC_corpus_11_std_lbl |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # dcc_mentions (Number of dcc_mentions (in the corpus))
    DCC_corpus_11_std_lbl |> select(dataset_for_matching, doi) |> distinct() |> nrow(),
    
    # dcc_rep (Number of dcc_repositories (in the corpus))
    DCC_corpus_11_std_lbl |> select(repository) |> distinct() |> nrow(),
    
    # dcc_journals (Number of dcc_journals (in the corpus))
    DCC_corpus_11_std_lbl |> select(journal) |> distinct() |> nrow(),
    
    # dcc_papers (Number of dcc_papers (in the corpus))
    DCC_corpus_11_std_lbl |> select(doi) |> distinct() |> nrow(),
    
    # dcc_doi_is_dataset (Number of dcc_doi = dcc_dataset (in the corpus))
    doi_is_id_count,
    
    # human_odd_r (human_odd_r (odds_ratio))
    odds_ratios[["human_dataTRUE"]],
    
    # covid_odd_r (covid_odd_r (odds_ratio))
    odds_ratios[["covid_relatedTRUE"]],
    
    # datasets_20_23 Number of datasets published between 2020-2023)
    999
  )
)

res <- res |> pivot_wider(names_from = value, values_from = count) |> mutate(placeholder = "__")

# save as RData
save_cr(res, file = file.path(here("data",
                                   "inputs_for_quick_render",
                                   "res.RData")))


# 4. Build Analyses results tables ----------------------------------------

# Chi square results:

# 1. Extract raw p-values (excluding intercept)
coeffs <- summary_model_chi$coefficients
raw_p <- coeffs[, "Pr(>|z|)"]
raw_p <- raw_p[!str_detect(names(raw_p), "Intercept")]

# 2. Rename raw p-values
raw_p_named <- raw_p |>
  setNames(names(raw_p) |> str_remove("TRUE") |> (\(x) paste0("p_", x))())

# 3. Adjust p-values (FDR by default)
adj_p <- p.adjust(raw_p, method = "fdr")

# 4. Rename adjusted p-values
adj_p_named <- adj_p |>
  setNames(names(adj_p) |> str_remove("TRUE") |> (\(x) paste0("p_adj_", x))())

# 5. Extract overall model p-value and format
p_model <- anova_result$`Pr(>Chi)`[2] |> as.numeric()
p_model <- ifelse(p_model < 1e-6, "<0.000001", formatC(p_model, format = "f", digits = 6))
names(p_model) <- "p_model"

# 6. Get odds ratios (excluding intercept)
or_vals <- odds_ratios[!str_detect(names(odds_ratios), "Intercept")]
or_vals_named <- or_vals |>
  setNames(names(or_vals) |> str_remove("TRUE") |> (\(x) paste0("odds_ratios_", x))())

# 7. Combine into final 1-row tibble
stat_chi_metadata_res <- tibble(
  !!!as.list(raw_p_named),
  !!!as.list(adj_p_named),
  p_model = p_model,
  !!!as.list(or_vals_named)
)

# save as RData
save_cr(stat_chi_metadata_res, file = file.path(here("data",
                                   "inputs_for_quick_render",
                                   "stat_chi_metadata_res.RData")))

# GLM: age-citation relationship model results

load(here("data", "tables_for_plots", "summary_model_glm.RData"))

p_age <- as.data.frame(summary_model_glm$coefficients$cond["age", "Pr(>|z|)"]) |> 
  rename(p_value = `summary_model_glm$coefficients$cond[\"age\", \"Pr(>|z|)\"]`)



# save as RData
save_cr(p_age, file = file.path(here("data",
                                     "inputs_for_quick_render",
                                     "p_age.RData")))

