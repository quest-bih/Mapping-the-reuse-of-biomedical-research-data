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


# 004: add missing metadata from Evgeny -----------------------------------------------

# load
load_latest_metadata_update() # call function to load latest version

# prepare a missing values table 

in_dcc_to_fill_eb <- datasets_metadata_master_updated_003 |>
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

##### CONTINUE FROM HERE





# 00?: add a secondary identifier for EBI repo ----------------------------

# load
load_latest_metadata_update() # call function to load latest version

# get EBI (ena) prefixes
prefixes <- c("prj", "erp", "samea", "ers", "err", "erx", "erz", "cab", "gca", "cm", "erc", "taxon")

# get ids with thie prefixes to understand how many are there

no_secondary_id <- datasets_metadata_master_updated_003 |>
  dplyr::filter(str_starts(data_id_merged, str_c("^(", str_c(prefixes, collapse = "|"), ")"))) |> 
  select(data_id_merged) |> 
  distinct()

# save
write_csv_cr(no_secondary_id,
             file = here("data",
                         "verification",
                         "metadata all",
                         "datasets_metadata_master_updated",
                         "tables to fill",
                         "no_secondary_id.csv"),
             row.names = FALSE)










# 00? add data articles metadata ------------------------------------------


data_articles_ids <- read_excel(file.path(here("data",
                                               "raw",
                                               "data_articles",
                                               "v10"),
                                          "datajournal_articles - analysis of citations v10.xlsx"),
                                sheet = "datasets_repos") |> # load relevant sheet from xlsx
  rename(doi = `Charité article DOI`,
         data_identifier = `dataset DOI, accession code, or link`) |>  # rename columns to meach numbat list later
  mutate(across(everything(), tolower)) |> # tolower
  select(doi, data_identifier, license) |> # get only relevant columns: charite data article and dataset id
  dplyr::filter(!data_identifier == "n/a") # remove NAs






# 00? add more metadata of non-matched ids --------------------------------


# sample 170 non-matched to send to blanks so that she would add metadata to them

# get the first 30 that I already sampled

first_30 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_30_ids_no_citation.csv")),
  header = TRUE)

# Verify that 
# all(first_30$charite_data_id_or_acc_nr_merged %in% datasets_master_metadata$data_id_merged)

# sample 170 

sample_170_ids_no_citation <- datasets_master_metadata |> 
  dplyr::filter(in_dcc == "FALSE") |> 
  distinct(data_id_merged, .keep_all = TRUE) |>
  dplyr::filter(!data_id_merged %in% first_30$charite_data_id_or_acc_nr_merged) |> 
  slice_sample(n = 170)

# save
save_cr(sample_170_ids_no_citation,
        file = file.path(here("data", "verification","verification of sample", "sample_170_ids_no_citation.RData")))

# write as csv
write_csv_cr(
  sample_170_ids_no_citation,
  file = here("data",
              "verification",
              "verification of sample",
              "sample_170_ids_no_citation.csv"),
  row.names = FALSE
)

# add the metadata of the 170 back to datasets_metadata_master_updated