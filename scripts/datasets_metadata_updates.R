# This script is meant to add data to datasets_metadata_master_updated_xxx and save it as a new version each time.
# For each update, please follow the template under:
# "Updating datasets_metadata_master_updated"

# 1. Setup ----------------------------------------------------------------

Sys.setenv(LANG = "EN")  # Set environment language to English

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, readxl, lubridate, stringi, writexl)

# wrappers for save. write.csv() and write_xlsx with automatic directory creation

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
} 

write_xlsx_cr <- function(x, file, ...) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  writexl::write_xlsx(x, path = file, ...)
}

# Function to write updated file to next version

metadata_update <- function(obj) {
  # Get object name as a string
  obj_name <- deparse(substitute(obj))
  
  # Extract suffix (e.g., "032" from datasets_metadata_master_updated_032)
  suffix <- sub(".*_(\\d+)$", "\\1", obj_name)
  
  # Define base directory
  base_dir <- here("data", "verification", "metadata all", "datasets_metadata_master_updated")
  
  # Paths
  rda_path <- file.path(base_dir, "rda", paste0("datasets_metadata_master_updated_", suffix, ".RData"))
  csv_path <- file.path(base_dir, "csv", paste0("datasets_metadata_master_updated_", suffix, ".csv"))
  xlsx_path <- file.path(base_dir, "xlsx", paste0("datasets_metadata_master_updated_", suffix, ".xlsx"))
  
  # Save in all formats
  save_cr(list = obj_name, file = rda_path)
  write_csv_cr(obj, file = csv_path, row.names = FALSE)
  write_xlsx_cr(obj, file = xlsx_path)
}

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

# Updating datasets_metadata_master_updated -------------------------------

# 002: get more years

# load
load_latest_metadata_update() # call function to load latest version

# get years

for_update_002 <- read.csv(
  file.path(here("data",
                 "verification",
                 "datasets years",
                 "datasets_years_filled_AC_v5.csv")),
  header = TRUE) |> 
  distinct()

# Add (get lastest version number from global environment or from console)

datasets_metadata_master_updated_002 <- datasets_metadata_master_updated_001 |> 
  left_join(for_update_002,
            by = "data_id_merged") |> 
  mutate(charite_id_year = coalesce(charite_id_year.x, charite_id_year.y)) |>
  select(-c(charite_id_year.x, charite_id_year.y))

# save
metadata_update(datasets_metadata_master_updated_002) # call function to save as csv, xlsx, rda
  

# 003: add missing license metadata for data articles -----------------------

# load
load_latest_metadata_update() # call function to load latest version

# Prepare a table of missing metadata

in_dcc_to_fill_ac <- datasets_metadata_master_updated_002 |>
  dplyr::filter(in_dcc == "TRUE") |>
  select(
    doi,
    data_id_merged,
    covid_related,
    human_data,
    data_availability_statement,
    license,
    source) |>
  dplyr::filter(
    is.na(covid_related)
    | is.na(human_data)
    | is.na(data_availability_statement)
    | is.na(license))

# repository and year had no NAs.

# save
write_csv_cr(in_dcc_to_fill_ac,
          file = here("data",
                      "verification",
                      "metadata all",
                      "datasets_metadata_master_updated",
                      "tables to fill",
                      "in_dcc_to_fill_ac.csv"),
          row.names = FALSE)
    

# While entering the metadata I checked and "10.18112/openneuro.ds001226" appears twice
# (once as a numbat source and once as data_articles source)
# which is fine by itself, but the metadata was not coherent for each case, so I fixed that as well.
# other datasets that appear in numbat and data_articles didn't have that problem

# load back the filled file

in_dcc_to_fill_ac_filled <- read.csv(
  file.path(here("data",
                 "verification",
                 "metadata all",
                 "datasets_metadata_master_updated",
                 "tables to fill",
                 "in_dcc_to_fill_ac_filled.csv")),
  header = TRUE)

# prepare for joining

for_update_003 <- in_dcc_to_fill_ac_filled |> 
  dplyr::filter(source == "data_articles") |> 
  select(data_id_merged, license) |> 
  distinct()

# add to master (get lastest version number from global environment or from console)

datasets_metadata_master_updated_003 <- datasets_metadata_master_updated_002 |> 
  left_join(for_update_003 |> select(data_id_merged, license),
            by = "data_id_merged") |> 
  mutate(license = coalesce(license.y, license.x)) |> 
  select(-license.x, -license.y)

# save
metadata_update(datasets_metadata_master_updated_003) # call function to save as csv, xlsx, rda


# 004: fix zenodo cases where data_id_merged is shifted -----------------------------------------------

# load
load_latest_metadata_update() # call function to load latest version

datasets_metadata_master_updated_004 <- datasets_metadata_master_updated_003 |>
  left_join(charite_dois_and_ids_8_for_matching |>
              select(unique_id, data_id_m_val, data_id_merged),
            by = "unique_id") |> 
  select(-c(data_id_m_val.x, data_id_merged.x)) |> 
  rename(data_id_m_val = data_id_m_val.y,
         data_id_merged = data_id_merged.y) |> 
  relocate(data_id_m_val, .after = data_id_no_ex_chr) |>
  relocate(data_id_merged, .after = data_id_m_val)

# save
metadata_update(datasets_metadata_master_updated_004) # call function to save as csv, xlsx, rda

# 005: add missing metadata of matched from Evgeny -----------------------------------------------

load_latest_metadata_update() # call function to load latest version

# prepare a missing values table 

in_dcc_to_fill_eb <- datasets_metadata_master_updated_004 |>
  dplyr::filter(in_dcc == "TRUE") |>
  select(
    doi,
    data_id_merged,
    covid_related,
    human_data,
    data_availability_statement,
    license,
    source) |>
  dplyr::filter(
    is.na(covid_related)
    | is.na(human_data)
    | is.na(data_availability_statement)
    | is.na(license))


# save
write_csv_cr(in_dcc_to_fill_eb,
             file = here("data",
                         "verification",
                         "metadata all",
                         "datasets_metadata_master_updated",
                         "tables to fill",
                         "in_dcc_to_fill_eb.csv"),
             row.names = FALSE)


# load filled file
in_dcc_filled <- read_excel(here("data",
                                 "verification",
                                 "metadata all",
                                 "datasets_metadata_master_updated",
                                 "filled tables",
                                 "in_dcc_to_fill_bi_v3.xlsx"))

# join to master metadata
datasets_metadata_master_updated_005 <- datasets_metadata_master_updated_004 |>
  mutate(license = case_when(license == "NULL" ~ NA_character_, .default = license)) |> 
  left_join(in_dcc_filled |>
              select(-c(`...9`, `Notes`, `Evgeny comment`)),
            by = c("doi", "data_id_merged", "source"), suffix = c("", "_new")) |> 
  mutate(
    covid_related = coalesce(covid_related_new),
    human_data = coalesce(human_data, human_data_new),
    data_availability_statement = coalesce(data_availability_statement, data_availability_statement_new),
    license = coalesce(license, license_new) # merge updated values into columns
  ) |> 
  select(-ends_with("_new")) |> # remove temporary new columns
  select(-unique_id) # remove unique_id column

# manually correct one value, since in "in_dcc_filled" there were 2 contradicting values for the same doi+data_id

datasets_metadata_master_updated_005 <- datasets_metadata_master_updated_005 |>
  mutate(
    data_availability_statement = case_when(
      doi == "10.1038/s41597-022-01806-4" &
        data_id_merged == "10.18112/openneuro.ds001226" ~ "yes",
      .default = data_availability_statement
    )
  )

# save
metadata_update(datasets_metadata_master_updated_005) # call function to save as csv, xlsx, rda

# 006: add non-matched metadata ----------------------------

load_latest_metadata_update() # call function to load latest version


# load filled file
not_in_dcc_filled <- read_excel(here("data",
                                 "verification",
                                 "metadata all",
                                 "datasets_metadata_master_updated",
                                 "filled tables",
                                 "sample_200_ids_no_citation_v14.xlsx"))

# prepare for joining

not_in_dcc_filled_for_joining <- not_in_dcc_filled |> 
  select(doi, data_id_merged, ...) |> 
  mutate(data_availability_statement = case_when(
    `dataset mentioned in DAS` == "1" ~ "yes"
  ))



# 007: rename, remove and create columns ----------------------------

# rename / remove / create according to the instructions below:
  # unique_id - remove
  # doi -> doi_charite
  # doi_no_ver_info - keep
  # data_id -> data_identifier_orig_1st_entry
  # data_id_auto_cleaned - keep
  # data_id_no_ex_chr - remove
  # data_id_m_val - keep
  # data_id_merged -> dataset_for_matching
  # data_access
  # in_dashboard
  # source -> data_id_source
  # listed_in_numbat_output - remove
  # validated - remove
  # Category -> category
  # covid_related
  # human_data
  # dataset_is_doi - if same as category then remove
  # repository
  # in_dcc
  # data_availability_statement
  # charite_id_year
  # license
  # create (left_join) "is_gen_rep"


# 00? add data articles metadata? ------------------------------------------



# add only if Blanka didn't already added metadata for data articles in matched / non-matched:

# data_articles_ids <- read_excel(file.path(here("data",
#                                                "raw",
#                                                "data_articles",
#                                                "v10"),
#                                           "datajournal_articles - analysis of citations v10.xlsx"),
#                                 sheet = "datasets_repos") |> # load relevant sheet from xlsx
#   rename(doi = `Charité article DOI`,
#          data_identifier = `dataset DOI, accession code, or link`) |>  # rename columns to meach numbat list later
#   mutate(across(everything(), tolower)) |> # tolower
#   select(doi, data_identifier, license) |> # get only relevant columns: charite data article and dataset id
#   dplyr::filter(!data_identifier == "n/a") # remove NAs
