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

# 2. Matched:

# get file names
files <- dir_ls(
  here("data", "wrangling_steps", "all_sources_binded"),
  regexp = "dcc_detected_ids_all_sources_\\d+.*\\.RData$"
)

# get numbers index
nums <- str_extract(basename(files), "(?<=dcc_detected_ids_all_sources_)\\d+") |> as.integer()

# get latest file
latest_file <- files[which.max(nums)]

# load into environment and pull the object into detected
env <- new.env()
obj_names <- load(latest_file, envir = env)
detected <- env[[obj_names[1]]]

# 3. For matching

## 3.1 Numbat + DA
load(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_9_clean_pairs.RData"))

num_for_match <- numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(source == "numbat")

da_for_match <- numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(source == "data_articles")

## 3.2 Additional IDs
load(here("data", "wrangling_steps", "datastet", "added_and_ds_for_matching_4_rm_exist.RData"))

ad_for_match <- added_and_ds_for_matching_4_rm_exist |> 
  dplyr::filter(source == "additional_ids")

# 3. Build results table --------------------------------------------------

res <- tibble(
  value = c(
    "placeholder",
    "ds_matched_ids_count_dist_no_overlap",
    "n_matched_ids_count_dist_no_overlap",
    "da_matched_ids_count_dist_no_overlap",
    "all_matched_ids_count_dist_no_overlap",
    
    "ds_non_matched_ids_count_dist_no_overlap",
    "n_non_matched_ids_count_dist_no_overlap",
    "da_non_matched_ids_count_dist_no_overlap",
    "all_non_matched_ids_count_dist_no_overlap",
    
    "n_max_mentions_of_single_id",
    "all_ids_with_1_mention_count_dist",
    "n_ids_with_1_mention_count_dist",
    
    "n_ids_for_match_count",
    "n_2nd_for_match_count",
    "da_for_match_count",
    "ad_for_match_count",
    
    "n_dois_for_match",
    "da_dois_for_match",
    "ad_dois_for_match",
    "n_mentions"),
    # "all_matched_mentions_count_dist_no_overlap_geo",
    # "all_matched_mentions_dist_no_overlap_gen_rep",
    # "all_matched_mentions_dist_no_overlap_disp_rep",
    # "all_matched_mentions_dist_no_overlap_mendeley",
    # "all_matched_mentions_dist_no_overlap_emd",
    # "all_matched_ids_dist_no_overlap_gen_rep",
    # "all_matched_ids_dist_no_overlap_disp_rep"),
  count = c(
    999,
    detected |> dplyr::filter(source_charite == "datastet") |> select(detected_id) |> distinct() |> nrow(),
    detected |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> select(detected_id) |> distinct() |> nrow(),
    detected |> dplyr::filter(source_charite == "data_articles") |> select(detected_id) |> distinct() |> nrow(),
    detected |> select(detected_id) |> distinct() |> nrow(),
    
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite == "datastet") |> select(dataset_for_matching) |> distinct() |> nrow(),
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite %in% c("numbat", "additional_ids")) |> select(dataset_for_matching) |> distinct() |> nrow(),
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite == "data_articles") |> select(dataset_for_matching) |> distinct() |> nrow(),
    metadata |> dplyr::filter(in_dcc == "FALSE") |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    detected |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> 
      distinct(detected_id, doi_dcc) |>
      count(detected_id, name = "n_citations") |>
      arrange(desc(n_citations)) |>
      slice(1) |>
      pull(n_citations),
    
    detected |>
      distinct(detected_id, doi_dcc) |>
      count(detected_id, name = "n_citations") |>
      dplyr::filter(n_citations == 1) |>
      nrow(),
    
    detected |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> 
      distinct(detected_id, doi_dcc) |>
      count(detected_id, name = "n_citations") |>
      dplyr::filter(n_citations == 1) |>
      nrow(),
      
    num_for_match |> select(dataset_for_matching) |> dplyr::filter(!is.na(dataset_for_matching)) |> distinct() |> nrow(),
    num_for_match |> select(data_id_secondary) |> dplyr::filter(!is.na(data_id_secondary)) |> distinct() |> nrow(),
    
    da_for_match |> select(dataset_for_matching) |> dplyr::filter(!is.na(dataset_for_matching)) |> distinct() |> nrow(),
    ad_for_match |> select(dataset_for_matching) |> dplyr::filter(!is.na(dataset_for_matching)) |> distinct() |> nrow(),
    
    num_for_match |> dplyr::filter(source == "numbat") |> select(doi) |> distinct() |> nrow(),
    da_for_match |> dplyr::filter(source == "data_articles") |> select(doi) |> distinct() |> nrow(),
    ad_for_match |> dplyr::filter(source == "additional_ids") |> select(doi) |> distinct() |> nrow(),
    detected |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> distinct(detected_id, doi_dcc) |> nrow()
  )
)

res <- res |> pivot_wider(names_from = value, values_from = count) |> mutate(placeholder = "__")

# save as RData
save_cr(res, file = file.path(here("data",
                                   "inputs_for_quick_render",
                                   "res.RData")))
