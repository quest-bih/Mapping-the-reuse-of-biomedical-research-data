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
  select(doi, data_id_merged, `covid`, `human data`, `dataset mentioned in DAS`, license) |> 
  mutate(
    data_availability_statement = case_when(
      `dataset mentioned in DAS` == "1" ~ "TRUE",
      `dataset mentioned in DAS` == "0" ~ "FALSE",
      .default = as.character(`dataset mentioned in DAS`)),
    human_data = case_when(
      `human data` == "1" ~ "TRUE",
      `human data` == "0" ~ "FALSE",
      .default = as.character(`human data`)),
    covid_related = case_when(
      `covid` == "1" ~ "TRUE",
      `covid` == "0" ~ "FALSE",
      .default = as.character(`covid`)),
    license = case_when(
      license == "0" ~ "no license",
      .default = license)) |> 
  select(-c(`dataset mentioned in DAS`, `human data`, `covid`))

# join

datasets_metadata_master_updated_006 <- datasets_metadata_master_updated_005 |> 
  left_join(not_in_dcc_filled_for_joining,
            by = c("doi", "data_id_merged")) |> 
  mutate(
    license = coalesce(license.y, license.x),
    data_availability_statement = coalesce(data_availability_statement.y, data_availability_statement.x),
    human_data = coalesce(human_data.y, human_data.x),
    covid_related = coalesce(covid_related.y, covid_related.x) # get new value unless the new value is NA, then fall back to old value
  ) |>
  select(-license.x, -license.y, 
         -data_availability_statement.x, -data_availability_statement.y,
         -human_data.x, -human_data.y,
         -covid_related.x, -covid_related.y)

# there were 4 cases where some metadata was not there to begin with:
left_to_fill <- not_in_dcc_filled |>
  dplyr::filter(is.na(`human data`)
                | is.na(`dataset mentioned in DAS`)
                | is.na(`covid`)
                | is.na(`license`)) |>
  select(doi, data_id_merged, `human data`, `dataset mentioned in DAS`, `covid`, `license`)

# I'll add it in a separate step, once I get it.

# save
metadata_update(datasets_metadata_master_updated_006) # call function to save as csv, xlsx, rda

# 007: restructure table ----------------------------

load_latest_metadata_update() # call function to load latest version

datasets_metadata_master_updated_007 <- datasets_metadata_master_updated_006 |>
  # remove redundant columns
  select(-c(data_id_no_ex_chr, validated, dataset_is_doi, Category)) |> 
  # rename columns to fit dcc-charite joined tables
  rename(
    doi_charite = doi,
    data_identifier_orig_1st_entry = data_id,
    dataset_for_matching = data_id_merged,
    data_id_source = source
  ) |> 
  # create "orig_id_is_doi" and "is_gen_rep" by "data_identifier_orig_1st_entry" value
  mutate(
    orig_id_is_doi = case_when(
      str_detect(data_identifier_orig_1st_entry, fixed("10.")) ~ "TRUE",
      .default = "FALSE"),
    is_gen_rep = case_when(
      str_detect(data_identifier_orig_1st_entry, regex("zenodo|figshare|dryad|mendeley|harvard|osf")) ~ "TRUE",
      .default = "FALSE"),
    # if dataset_for_matching starts with "10.17605/osf.io/", convert the value to "osf_*slug*"
    dataset_for_matching = case_when(
      str_starts(dataset_for_matching, "10.17605/osf.io/")
      ~ str_c("osf_", str_remove(dataset_for_matching, "10.17605/osf.io/")),
      .default = dataset_for_matching
    ))

# save
metadata_update(datasets_metadata_master_updated_007) # call function to save as csv, xlsx, rda
  
# 008 add data articles metadata, where missing ------------------------------------------

load_latest_metadata_update() # call function to load latest version

# load Blanka'ss data articles file
data_articles_ids <- read_excel(file.path(here("data",
                                               "raw",
                                               "data_articles",
                                               "v10"),
                                          "datajournal_articles - analysis of citations v10.xlsx"),
                                sheet = "datasets_repos") |> # load relevant sheet from xlsx
  rename(doi_charite = `Charité article DOI`,
         data_identifier_orig_1st_entry = `dataset DOI, accession code, or link`) |>  # rename columns to meach numbat list later
  mutate(across(everything(), tolower)) |> # tolower
  select(doi_charite, data_identifier_orig_1st_entry, license) |> # get only relevant columns: charite data article and dataset id
  dplyr::filter(!data_identifier_orig_1st_entry == "n/a")

# check what's missing
data_articles_ids |>
  inner_join(datasets_metadata_master_updated_008, by = c("doi_charite", "data_identifier_orig_1st_entry")) |>
  select(doi_charite, dataset_for_matching, license.x, license.y) |> View()

# add manually

datasets_metadata_master_updated_008 <- datasets_metadata_master_updated_007 |> 
  mutate(license = case_when(
    dataset_for_matching %in% c("srp136594", "ccrp136594", "ghnv00000000", "gse173610") ~ "no license",
    dataset_for_matching %in% c("10.6084/m9.figshare.c.4869732", "pxd016782") ~ "cc0",
    dataset_for_matching %in% c("10.6084/m9.figshare.c.5222051", "10.6084/m9.figshare.19316219", "10.6084/m9.figshare.19255067") ~ "cc-by",
    .default = license))

# save
metadata_update(datasets_metadata_master_updated_008) # call function to save as csv, xlsx, rda

# 009: complete metadata of 200 non-matched cases -------------------

load_latest_metadata_update() # call function to load latest version

# change entries manually
datasets_metadata_master_updated_009 <- datasets_metadata_master_updated_008 |> 
  mutate(
    data_availability_statement = case_when(
      dataset_for_matching == "pxd036786" ~ "TRUE",
      dataset_for_matching == "prjna413158" ~ "TRUE",
      dataset_for_matching == "10.5281/zenodo.7889352" ~ "TRUE",
      dataset_for_matching == "e-mtab-8521" ~ "TRUE",
      dataset_for_matching == "gse148720" ~ "FALSE",
      .default = data_availability_statement
    ),
    human_data = case_when(
      dataset_for_matching == "pxd036786" ~ "TRUE",
      dataset_for_matching == "prjna413158" ~ "TRUE",
      dataset_for_matching == "10.5281/zenodo.7889352" ~ "TRUE",
      dataset_for_matching == "e-mtab-8521" ~ "FALSE",
      dataset_for_matching == "gse148720" ~ "FALSE",
      .default = human_data
      ),
    covid_related = case_when(
      dataset_for_matching == "pxd036786" ~ "FALSE",
      dataset_for_matching == "prjna413158" ~ "FALSE",
      dataset_for_matching == "10.5281/zenodo.7889352" ~ "FALSE",
      dataset_for_matching == "e-mtab-8521" ~ "FALSE",
      dataset_for_matching == "gse148720" ~ "FALSE",
      .default = covid_related
    ),
    license = case_when(
      dataset_for_matching == "pxd036786" ~ "cc0",
      dataset_for_matching == "prjna413158" ~ "no license",
      dataset_for_matching == "10.5281/zenodo.7889352" ~ "cc-by",
      dataset_for_matching == "e-mtab-8521" ~ "no license",
      dataset_for_matching == "gse148720" ~ "no license",
      .default = license
    )
  )


# save
metadata_update(datasets_metadata_master_updated_009) # call function to save as csv, xlsx, rda


# 010: standardize metadata values ----------------------------------------

load_latest_metadata_update() # call function to load latest version

# check col types
str(datasets_metadata_master_updated_009)

# convert logi cols to to chr
datasets_metadata_master_updated_in_process <- datasets_metadata_master_updated_009 |> 
  mutate(in_dashboard = as.character(in_dashboard),
         listed_in_numbat_output = as.character(listed_in_numbat_output))

# check unique values of relevant columns: overview
datasets_metadata_master_updated_in_process |>
  select(7:last_col()) |>
  select(-charite_id_year) |> 
  map(~ as.character(unique(.x))) |>   # Coerce to character
  enframe(name = "column", value = "unique_values") |>
  View()

# check all unique values of license, data_availability_statement and repository
datasets_metadata_master_updated_in_process |> select(license) |> unique()
datasets_metadata_master_updated_in_process |> select(data_availability_statement) |> unique()
datasets_metadata_master_updated_in_process |> select(repository) |> unique()

datasets_metadata_master_updated_010 <- datasets_metadata_master_updated_in_process |>
  mutate(
    das_for_analysis = case_when(
      is.na(data_availability_statement) ~ NA_character_,
      data_availability_statement %in% c("TRUE", "yes") ~ "TRUE",
      .default = "FALSE"
    ),
    license_for_analysis = case_when(
      is.na(license) ~ NA_character_,
      license %in% c("CC0", "cc0", "CC BY", "cc-by", "CC BY-NC", "CC BY;CC0", "CC BY-NC-SA", "CC BY-NC-ND", "GNU GPLv3", "MIT", "GNU") ~ "TRUE",
      .default = "FALSE"
    )
  )

# save
metadata_update(datasets_metadata_master_updated_010) # call function to save as csv, xlsx, rda


# 011: add last matched metadata ------------------------------------------

load_latest_metadata_update() # call function to load latest version

# I created this table to fill NAs in:
datasets_metadata_master_updated_010 |>
  dplyr::filter(in_dcc == "TRUE") |>
  select(doi_charite, data_identifier_orig_1st_entry, dataset_for_matching, license_for_analysis, data_access, covid_related) |>
  distinct() |> 
  dplyr::filter(
    is.na(license_for_analysis)
    | is.na(data_access)
    | is.na(covid_related)) |> 
  View()

# and sent it here:
# https://teams.microsoft.com/l/message/19:cba60d9f-88ed-4fe4-ac8c-0c0e5477dfee_efba9537-d132-44d8-bc1e-c7742bb99d78@unq.gbl.spaces/1753449258487?context=%7B%22contextType%22%3A%22chat%22%7D


# load filled table


# prepare for joining


# join


# save
metadata_update(datasets_metadata_master_updated_011) # call function to save as csv, xlsx, rda

