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

metadata_no_da <- metadata |> dplyr::filter(source_charite != "data_articles")

# 2. Matched

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_7_w_metadata.RData"))

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_8_dedup.RData"))

detected_dedup <- dcc_detected_ids_all_sources_8_dedup

detected_dedup_no_da <- dcc_detected_ids_all_sources_8_dedup |> dplyr::filter(source_charite != "data_articles")

# 3. For matching

## 3.1 Numbat
load(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_9_clean_pairs.RData"))

num_for_match <- numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(source == "numbat")

## 3.2 Data Articles

da_for_match <- numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(source == "data_articles")

# Count how many cases with "1.0.1" should be subtracted later (NOT relevant for matched, since only 1 version of it was matched)
n_collapse <- da_for_match |> 
  dplyr::filter(stringr::str_detect(numbat_id_of_da_id, "1\\.0\\.1")) |> 
  nrow() - 1 # 7 cases are supposed to be treated as 6 cases

## 3.3 Additional IDs
load(here("data", "wrangling_steps", "datastet", "added_and_ds_for_matching_4_rm_exist.RData"))

ad_for_match <- added_and_ds_for_matching_4_rm_exist |> 
  dplyr::filter(source == "additional_ids")

# clean up
rm(numbat_da_dois_and_ids_9_clean_pairs,
   added_and_ds_for_matching_4_rm_exist,
   datasets_metadata_master_updated_018)

# 3. Build results tables --------------------------------------------------

# 1. Counts

res <- tibble(
  value = c(
    "placeholder",
    "ds_matched_ids_count_dist",               # DS   ids dist detected
    "n_matched_ids_count_dist",                # N+AD ids dist detected      
    "da_matched_ids_count_dist",               # DA   ids dist detected
    "all_matched_ids_count_dist",              # All  ids dist detected
    
    "ds_non_matched_ids_count_dist",           # DS   ids dist not
    "n_non_matched_ids_count_dist",            # N+AD ids dist not
    "da_non_matched_ids_count_dist",           # DA   ids dist not
    "all_non_matched_ids_count_dist",          # All  ids dist not
    
    "all_matched_and_non_matched_ids_count_dist", # All ids (matched+non)
    
    "max_mentions_of_single_id",               # All  max-ref of 1 id
    "all_ids_with_1_mention_count_dist",       # All  ids with 1 mention
    "n_ids_with_1_mention_count_dist",         # N    ids with 1 mention
    
    "n_ids_for_match_count",                   # N    ids for matching
    "n_2nd_for_match_count",                   # N    2nd ids for matching
    "da_for_match_count",                      # DA   ids for matching
    "ad_for_match_count",                      # AD   ids for matching
    
    "n_dois_for_match",                        # N    dois for matching
    "da_dois_for_match",                       # DA   dois for matching
    "ad_dois_for_match",                       # AD   dois for matching
    
    "n_mentions",                              # N    refs dist detected
    "ds_mentions",                             # DS   refs dist detected
    "da_mentions",                             # DA   refs dist detected
    "all_mentions",                            # All  refs dist detected
     
    "all_mentions_geo",                        # All  refs dist detected in GEO
    "all_mentions_mendeley",                   # All  refs dist detected in mendeley
    "all_mentions_emdb",                       # All  refs dist detected in general purpose / disciplinary
    
    "all_mentions_gen_and_discp",               # All  refs dist detected in emdb

    "human_data",                              # All  ids dist detected human-data
    
    "all_matched_charite_dois",                # All  Charite's dois dist detected
    
    "all_matched_dcc_dois"),                   # All  Mentioning dois dist detected
    
  count = c(
    
    # placeholder
    999,
    
    # ds_matched_ids_count_dist
    detected_dedup |> dplyr::filter(source_charite == "datastet") |> select(detected_id) |> distinct() |> nrow(),
    
    # n_matched_ids_count_dist
    detected_dedup |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> select(detected_id) |> distinct() |> nrow(),
    
    # da_matched_ids_count_dist
    detected_dedup |> dplyr::filter(source_charite == "data_articles") |> select(detected_id) |> distinct() |> nrow(),
    
    # all_matched_ids_count_dist
    detected_dedup_no_da |> select(detected_id) |> distinct() |> nrow(),
    
    # ds_non_matched_ids_count_dist
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite == "datastet") |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # n_non_matched_ids_count_dist
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite %in% c("numbat", "additional_ids")) |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # da_non_matched_ids_count_dist
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite == "data_articles") |> select(dataset_for_matching) |> distinct() |> nrow() - n_collapse + 1,
    # 1 case is in dcc, which means it's not in this count, which means that nrow is 36 and should be substracted 5 from it (n_collapse + 1)
    
    # all_non_matched_ids_count_dist
    metadata_no_da |> dplyr::filter(in_dcc == "FALSE") |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # all_matched_and_non_matched_ids_count_dist
    metadata_no_da |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # max_mentions_of_single_id
    detected_dedup_no_da |>
      distinct(detected_id, doi_dcc) |>
      count(detected_id, name = "n_citations") |>
      arrange(desc(n_citations)) |>
      slice(1) |>
      pull(n_citations),
    
    # all_ids_with_1_mention_count_dist
    detected_dedup_no_da |>
      distinct(detected_id, doi_dcc) |>
      count(detected_id, name = "n_citations") |>
      dplyr::filter(n_citations == 1) |>
      nrow(),
    
    # n_ids_with_1_mention_count_dist
    detected_dedup_no_da |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> 
      distinct(detected_id, doi_dcc) |>
      count(detected_id, name = "n_citations") |>
      dplyr::filter(n_citations == 1) |>
      nrow(),
    
    # n_ids_for_match_count 
    num_for_match |> select(dataset_for_matching) |> dplyr::filter(!is.na(dataset_for_matching)) |> distinct() |> nrow(),
    
    # n_2nd_for_match_count
    num_for_match |> select(data_id_secondary) |> dplyr::filter(!is.na(data_id_secondary)) |> distinct() |> nrow(),
    
    # da_for_match_count
    da_for_match |> select(dataset_for_matching) |> dplyr::filter(!is.na(dataset_for_matching)) |> distinct() |> nrow() - n_collapse,
    # 7 "1.0.1" cases were collapsed into 1 case to count
    
    # ad_for_match_count
    ad_for_match |> select(dataset_for_matching) |> dplyr::filter(!is.na(dataset_for_matching)) |> distinct() |> nrow(),
    
    # n_dois_for_match
    num_for_match |> dplyr::filter(source == "numbat") |> select(doi) |> distinct() |> nrow(),
    
    # da_dois_for_match
    da_for_match |> dplyr::filter(source == "data_articles") |> select(doi) |> distinct() |> nrow(),
    # No need to collapse, since it's the same DOI for each "1.0.1", so distinct on "doi" already takes care of it
    
    # ad_dois_for_match
    ad_for_match |> dplyr::filter(source == "additional_ids") |> select(doi) |> distinct() |> nrow(),
    
    # n_mentions
    detected_dedup |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # ds_mentions    
    detected_dedup |> dplyr::filter(source_charite == "datastet") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # da_mentions    
    detected_dedup |> dplyr::filter(source_charite == "data_articles") |> distinct(detected_id, doi_dcc) |> nrow(),
    # no need to collapse since there are no variations of "1.0.1" in this count
    
    # all_mentions   
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
      "harvard dataverse")) |>
      distinct(detected_id, doi_dcc) |> nrow(),
    
    # human_data
    detected_dedup_no_da |> dplyr::filter(human_data == "TRUE") |> select(detected_id) |> distinct() |> nrow(),
    
    # all_matched_charite_dois 
    detected_dedup_no_da |> select(doi_no_ver_info) |> distinct() |> nrow(),
    
    # all_matched_dcc_dois
    detected_dedup_no_da |> select(doi_dcc) |> distinct() |> nrow()
  )
)

res <- res |> pivot_wider(names_from = value, values_from = count) |> mutate(placeholder = "__")

# save as RData
save_cr(res, file = file.path(here("data",
                                   "inputs_for_quick_render",
                                   "res.RData")))

# 2. Chi square results:

# 1. Extract raw p-values (excluding intercept)
coeffs <- summary_model$coefficients
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

   

# 3. age-citation relationship model

p_age <- as.data.frame(summary(model_nb)$coefficients$cond["age", "Pr(>|z|)"]) |> 
  rename(p_value = `summary(model_nb)$coefficients$cond[\"age\", \"Pr(>|z|)\"]`)



# save as RData
save_cr(p_age, file = file.path(here("data",
                                     "inputs_for_quick_render",
                                     "p_age.RData")))

