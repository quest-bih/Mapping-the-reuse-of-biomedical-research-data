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

# Create a table to fill NAs in:
last_matched_metadata_to_fill <- datasets_metadata_master_updated_010 |>
  dplyr::filter(in_dcc == "TRUE") |>
  select(doi_charite, data_identifier_orig_1st_entry, dataset_for_matching, license_for_analysis, data_access, covid_related) |>
  distinct() |> 
  dplyr::filter(
    is.na(license_for_analysis)
    | is.na(data_access)
    | is.na(covid_related))

# Save as xslx (was done retroactively)

write_xlsx_cr(last_matched_metadata_to_fill, file = here(
  "data", "verification", "metadata all",
  "datasets_metadata_master_updated", "tables to fill",
  "last_metadata_of_matched_to_fill.xlsx"))

# Send the file here:
# https://teams.microsoft.com/l/message/19:cba60d9f-88ed-4fe4-ac8c-0c0e5477dfee_efba9537-d132-44d8-bc1e-c7742bb99d78@unq.gbl.spaces/1753449258487?context=%7B%22contextType%22%3A%22chat%22%7D


# load filled table
last_matched_metadata_to_fill_filled <- read_excel(here(
  "data",
  "verification",
  "metadata all",
  "datasets_metadata_master_updated",
  "filled tables",
  "last_metadata_of_matched_to_fill_filled_BI.xlsx"))

# prepare for joining

  # check type
  str(last_matched_metadata_to_fill_filled_for_joining)
  str(datasets_metadata_master_updated_010)

last_matched_metadata_to_fill_filled_for_joining <- last_matched_metadata_to_fill_filled |> 
  select(-7, -dataset_for_matching) |> 
  mutate(license_for_analysis = as.character(license_for_analysis),
         covid_related = as.character(covid_related)) |> 
  rename(license = license_for_analysis) 
  
# join

datasets_metadata_master_updated_011 <- datasets_metadata_master_updated_010 |> 
  left_join(last_matched_metadata_to_fill_filled_for_joining,
            by = c("doi_charite", "data_identifier_orig_1st_entry")) |> 
  mutate(
    license = coalesce(license.y, license.x),
    data_access = coalesce(data_access.y, data_access.x),
    covid_related = coalesce(covid_related.y, covid_related.x) # get new value unless the new value is NA, then fall back to old value
  ) |>
  select(-license.x, -license.y, 
         -data_access.x, -data_access.y,
         -covid_related.x, -covid_related.y) |> 
  select(-c(license_for_analysis, das_for_analysis))

# NOTICE: I removed "license_for_analysis" and "das_for_analysis" columns
# and moved the mutation code to "joined_bind_add_metadata_verify.qmd"
# because it makes more sense for this operation to be excecuted there.

# save
metadata_update(datasets_metadata_master_updated_011) # call function to save as csv, xlsx, rda


# 012: add metadata for 2 missed secondary ids ----------------------------------------

# these detected ids weren't in the last step because they are secondary ids, and I accidentally omitted them:
# erp123138 (secondary to prjeb39602)
# srp229815 (secondary to prjna589622)

load_latest_metadata_update() # call function to load latest version

# create a table to fill
secondaries_to_fill <- datasets_metadata_master_updated_011 |> 
  select(
    doi_charite,
    dataset_for_matching,
    data_access,
    license,
    human_data,
    covid_related,
    orig_id_is_doi,
    repository
  ) |> 
  dplyr::filter(dataset_for_matching %in% c("prjeb39602", "prjna589622")) |> 
  mutate(
    data_id_secondary = case_when(dataset_for_matching == "prjeb39602" ~ "erp123138", .default = "srp229815"),
    detected_in_dcc = case_when(dataset_for_matching == "prjeb39602" ~ "erp123138", .default = "srp229815")) |> 
  relocate(data_id_secondary, .after = dataset_for_matching) |> 
  relocate(detected_in_dcc, .after = data_id_secondary)

# Save as xslx (was done retroactively)

write_xlsx_cr(secondaries_to_fill, file = here(
  "data", "verification", "metadata all",
  "datasets_metadata_master_updated", "tables to fill",
  "2_secondary_ids_to_fill.xlsx"))

# file was sent here:
# https://teams.microsoft.com/l/message/19:cba60d9f-88ed-4fe4-ac8c-0c0e5477dfee_efba9537-d132-44d8-bc1e-c7742bb99d78@unq.gbl.spaces/1755591105733?context=%7B%22contextType%22%3A%22chat%22%7D

# load filled table
secondaries_to_fill_filled <- read_excel(here(
  "data",
  "verification",
  "metadata all",
  "datasets_metadata_master_updated",
  "filled tables",
  "2_secondary_ids_to_fill_filled_BI.xlsx"))

# fill out manually by values in xlsx
# (It's just a bit more readable than standardizing before joining and then joining)

datasets_metadata_master_updated_012 <- datasets_metadata_master_updated_012 |> 
  mutate(data_access =
           case_when(dataset_for_matching %in% c("prjeb39602", "prjna589622")
                     ~ "yes",
                     .default = data_access),
         data_availability_statement =
           case_when(dataset_for_matching %in% c("prjeb39602", "prjna589622")
                     ~ "yes",
                     .default = data_availability_statement),
         license =
           case_when(dataset_for_matching %in% c("prjeb39602", "prjna589622")
                     ~ "TRUE",
                     .default = license),
         human_data =
           case_when(dataset_for_matching %in% c("prjeb39602", "prjna589622")
                     ~ "TRUE",
                     .default = human_data),
         covid_related =
           case_when(dataset_for_matching %in% c("prjeb39602", "prjna589622")
                     ~ "FALSE",
                     .default = covid_related),
         charite_id_year =
           case_when(dataset_for_matching == "prjeb39602"
                     ~ 2020,
                     .default = charite_id_year),
         charite_id_year =
           case_when(dataset_for_matching == "prjna589622"
                     ~ 2019,
                     .default = charite_id_year)
         )

# save
metadata_update(datasets_metadata_master_updated_012) # call function to save as csv, xlsx, rda


# 013: Restructure and rejoin all metadata again -----------------------------------------

# from sources: numbat+da matched, numbat+da non-matched, 012, dataset+added matched

# how it all started:
# https://docs.google.com/document/d/1y-WVv9rrwy8d1KOzKG1u0WTiAtZoCfu3qTcl2y_sjGw/edit?tab=t.0#bookmark=id.40nwpc5w2tl

# So I'm actually going to change the structure of the file so that I'll have everything documented in every column

# 1. Load final detected ids list from all sources (these are only the matched ones)

# 1.1 load and inspect reference tables

# load final matched list (datastet+added are already all matched)
load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_3_rm_dcc_is_ch.RData"))

# make sure that it also has the same structure as "numbat for matching":

load(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_9_clean_pairs.RData")) # load
setdiff(names(numbat_da_dois_and_ids_9_clean_pairs), names(dcc_detected_ids_all_sources_3_rm_dcc_is_ch)) # check

# all_sources has all of numbat. just have to add "_charite" suffix to "doi"    "slug"   "source" cols (later)

# 1.2 create the metadata 013 table structure from all_sources

# it basically means removing all dcc cols and labeling everything as "in_dcc" = "TRUE":

colnames(dcc_detected_ids_all_sources_3_rm_dcc_is_ch) # check cols to know which to remove

# create new metadata table structure
datasets_metadata_master_new_structure_matched_only <- dcc_detected_ids_all_sources_3_rm_dcc_is_ch |> 
  select(-c(id:primary), -c(authors_dcc, publication_year_dcc)) |> 
  mutate(in_dcc = "TRUE") |> # label that these are cases that were matched (detected) in DCC
  distinct()

# 2. Bind the non-matched numbat+da entries as well

# 2.1 prepare "numbat for matching" for binding
numbat_da_non_matched_for_binding <- numbat_da_dois_and_ids_9_clean_pairs  |> 
  rename(
    doi_charite = doi,
    slug_charite = slug,
    source_charite = source
  )  |> # rename cols
  dplyr::filter(
    !(dataset_for_matching %in% datasets_metadata_master_new_structure_matched_only$detected_id
      | data_id_secondary %in% datasets_metadata_master_new_structure_matched_only$detected_id)
  ) |> # get non-matched
  # (by the way, filtering by orig_pairs actually results in 3 extra ids that ARE a match, just with other dois)
  mutate(in_dcc = "FALSE") |> # label as non-matched
  mutate(validated = as.character(validated)) # convert col type

# check distinct
numbat_da_non_matched_for_binding |> distinct() |> nrow() # good

# 2.2 bind matched and non-matched

datasets_metadata_master_new_structure_all <- datasets_metadata_master_new_structure_matched_only |> 
  bind_rows(numbat_da_non_matched_for_binding)

# check nrow matched + non-matched
nrow(datasets_metadata_master_new_structure_matched_only) + nrow(numbat_da_non_matched_for_binding) # good
# check distinct
datasets_metadata_master_new_structure_all |> distinct() |> nrow() # good

# verify that orig and clean pairs are identical to the existing doi+id cols (orig and clean)

# orig
datasets_metadata_master_new_structure_all |> 
  select(doi_charite, data_identifier, doi_id_orig_pair) |> # select doi, id, pair
  mutate(check = paste(doi_charite, data_identifier, sep = ";")) |> # make a new pair
  mutate(is_equal = doi_id_orig_pair == check) |> # compare pairs
  dplyr::filter(is_equal = FALSE) # remove non-matched pairs

# clean
datasets_metadata_master_new_structure_all |> 
  select(doi_no_ver_info, detected_id, doi_id_clean_pair) |>
  mutate(check = paste(doi_no_ver_info, doi_no_ver_info, sep = ";")) |> 
  mutate(is_equal = doi_id_clean_pair == check) |>
  dplyr::filter(is_equal = FALSE)

# both types of pair identical.

save_cr(datasets_metadata_master_new_structure_all,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_structure_all.RData")))

# clean up
rm(
  datasets_metadata_master_new_structure_matched_only,
  numbat_da_dois_and_ids_9_clean_pairs,
  numbat_da_non_matched_for_binding,
  dcc_detected_ids_all_sources_3_rm_dcc_is_ch)

# 2. add metadata

# 2.1 write down which to add first (by date updated and reliability):

# numbat+da matched, numbat+da non-matched sample, 012, dataset+added matched

# Best order to add by:
# dataset+added matched (verified lately)
# numbat+da matched (verified as well)
# numbat+da non-matched sample (200) (verified lately)
# previous metadata = 012: only for cases that are still missing for some reason

# 2.3 Join dataset+added matched

# load metadata filled file (code copied from ds_added qmd)
datastet_and_added_matched_metatdata <- read_excel(here("data",
                                                    "verification",
                                                    "datastet_and_added_summarised",
                                                    "datastet_and_added_filled_raw",
                                                    "charite_dois_ids_distinct_v9.xlsx"))

# get only relevant cases
datastet_and_added_matched_metatdata_1_od_ids <- datastet_and_added_matched_metatdata |>
  dplyr::filter(is_dataset == "y") |> # get only verified datasets
  dplyr::filter(`data authorship` %in% c("own data", "data authorship")) # get only OD datasets

rm(datastet_and_added_matched_metatdata)

# prepare for joining

colnames(datastet_and_added_matched_metatdata_1_od_ids) # check colnames
any(grepl("[A-Z]", datastet_and_added_matched_metatdata_1_od_ids$doi_datastet_and_added)) # doi is already lowercase 
datastet_and_added_matched_metatdata_1_od_ids |> select(doi_datastet_and_added) |> distinct() |> View() # doi is already no_ver_info


ds_add_match_metadata_for_joining <- datastet_and_added_matched_metatdata_1_od_ids |> 
  rename(
    doi_no_ver_info = doi_datastet_and_added,
    data_availability_statement = DAS,
    human_data = human,
    covid_related = covid,
    charite_id_year = year,
    data_access_temp_check = `data access`
  ) |> 
  mutate(
    doi_id_clean_pair = paste(doi_no_ver_info, dataset_for_matching, sep = ";")) |> 
  select(
    doi_id_clean_pair,
    data_availability_statement,
    human_data,
    covid_related,
    charite_id_year,
    license,
    data_access_temp_check
  )
  
# join by clean_pair

datasets_metadata_master_new_st_all_1_ds_ad <- datasets_metadata_master_new_structure_all |> 
  left_join(ds_add_match_metadata_for_joining, by = "doi_id_clean_pair")

# verify that data_access from the 2 sources is the same
datasets_metadata_master_new_st_all_1_ds_ad |> dplyr::filter(data_access != data_access_temp_check) # yes

datasets_metadata_master_new_st_all_1_ds_ad <- datasets_metadata_master_new_st_all_1_ds_ad |> 
  select(-data_access_temp_check)

# save
save_cr(datasets_metadata_master_new_st_all_1_ds_ad,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_1_ds_ad.RData")))

# clean up
rm(datastet_and_added_matched_metatdata_1_od_ids, ds_add_match_metadata_for_joining)

# 2.4 Join numbat+da matched

# load
load_latest_metadata_update() # call function to load latest version

# prepare  for joining
colnames(datasets_metadata_master_updated_012) # check colnames
any(grepl("[A-Z]", datasets_metadata_master_updated_012$dataset_for_matching)) # verifying dataset_for_matching
datasets_metadata_master_updated_012 |> select(doi_no_ver_info) |> distinct() |> View() # verifying doi_no_ver_info

datasets_metadata_master_updated_012_for_joining <- datasets_metadata_master_updated_012 |> 
  rename(data_access_temp_check = data_access) |> 
  mutate(
    doi_id_clean_pair = paste(doi_no_ver_info, dataset_for_matching, sep = ";")) |> 
  select(
    doi_id_clean_pair,
    data_availability_statement,
    human_data,
    covid_related,
    charite_id_year,
    license,
    data_access_temp_check
  ) |>
  dplyr::filter(
    !is.na(data_availability_statement),
    !is.na(human_data),
    !is.na(covid_related),
    !is.na(charite_id_year),
    !is.na(license)
  ) |> 
  distinct()
  
# join by clean_pair

datasets_metadata_master_new_st_all_2_num_da <- datasets_metadata_master_new_st_all_1_ds_ad |> 
  left_join(datasets_metadata_master_updated_012_for_joining,
            by = "doi_id_clean_pair",
            suffix = c("", ".new")) |> 
  mutate(
    data_availability_statement = coalesce(data_availability_statement, data_availability_statement.new),
    human_data = coalesce(human_data, human_data.new),
    covid_related = coalesce(covid_related, covid_related.new),
    charite_id_year = coalesce(charite_id_year, charite_id_year.new),
    license = coalesce(license, license.new)
  ) |> 
  select(-ends_with(".new"))

# "10.18112/openneuro.ds001226" appears twice in 012 with 2 license values (pretty much the same, but not identical).
# I will resolve it later in this code.

# verify that data_access from the 2 sources is the same
datasets_metadata_master_new_st_all_2_num_da |> dplyr::filter(data_access != data_access_temp_check) # yes

# save
save_cr(datasets_metadata_master_new_st_all_2_num_da,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_2_num_da.RData")))

# Some metadata wasn't joined because some ids have different dois for same ids in different sources:
datasets_metadata_master_new_st_all_2_num_da |>
  dplyr::filter(in_dcc == "TRUE") |> 
  dplyr::filter(
    is.na(data_availability_statement)
    | is.na(human_data)
    | is.na(covid_related)
    | is.na(charite_id_year)
    | is.na(license)
  ) |> 
  select(detected_id, source_charite) |> 
  distinct() |> 
  View()

# add last matched that weren't joined because of different source for the same id

# create table to match
remaining_matched_ids_for_joining <- datasets_metadata_master_new_st_all_2_num_da |>
  dplyr::filter(in_dcc == "TRUE") |> 
  dplyr::filter(
    is.na(human_data)
    | is.na(covid_related)
    | is.na(charite_id_year)
    | is.na(license)
  ) |> 
  select(detected_id) |> 
  distinct() |>
  left_join(datasets_metadata_master_new_st_all_2_num_da |> 
              select(detected_id,
                     human_data,
                     covid_related,
                     charite_id_year,
                     license) |> 
              distinct(), by = "detected_id") |> 
  # remove NA duplicates
  dplyr::group_by(detected_id) |>
  dplyr::slice_max(rowSums(!is.na(across(everything()))), n = 1) |>
  dplyr::ungroup() |> 
  distinct(detected_id, .keep_all = TRUE) # remove duplicate ids with same metadata that is just written differently

# join
datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources <- datasets_metadata_master_new_st_all_2_num_da |> 
  left_join(remaining_matched_ids_for_joining, by = "detected_id",
            suffix = c("", ".new")) |> 
  mutate(
    human_data = coalesce(human_data, human_data.new),
    covid_related = coalesce(covid_related, covid_related.new),
    charite_id_year = coalesce(charite_id_year, charite_id_year.new),
    license = coalesce(license, license.new)
  ) |> 
  select(-ends_with(".new"))

# check DAS, that wasn't joined initially, because it is related to the doi

check <- datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources |>
  dplyr::filter(in_dcc == "TRUE") |> 
  dplyr::filter(
    is.na(data_availability_statement)
    | is.na(human_data)
    | is.na(covid_related)
    | is.na(charite_id_year)
    | is.na(license)
  ) |> 
  select(detected_id) |> 
  distinct()

datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources |> 
  dplyr::filter(detected_id %in% check$detected_id) |> 
  select(doi_charite, detected_id, data_availability_statement, source_charite) |> 
  distinct() |> 
  View()

# prepare a table to complete DAS with, since different DOIS for same id exist, and for them DAS wasn't extracted

# save
save_cr(datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources.RData")))

# clean up
rm(datasets_metadata_master_updated_012,
   datasets_metadata_master_updated_012_for_joining,
   check,
   remaining_matched_ids_for_joining)

# 2.5 Join numbat+da non-matched sample (200)

  
  ##### 
  # datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources$doi_id_lc_pair_for_joining
  # is NOT REALLY TOLOWERED! and it's supposed to be doi_lc+data_id_lc.
  # But in the join below I'll join by doi_id_lc_for_matching: doi_lc+dataset_for_matching,
  # since these are the common columns between sample-200 and datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources.
  # so first lower it and name it "doi_id_lc_pair" (I've checked backwards thoroughly and it doesn't change anything in the results until now).
  #####

# load 200 non matched filled

sample_200_non_matched_ids <- read_excel(here("data",
                                              "verification",
                                              "metadata all",
                                              "datasets_metadata_master_updated",
                                              "filled tables",
                                              "sample_200_ids_no_citation_v14.xlsx"))


# check that all of the 200-sample is in _3_
sample_200_non_matched_ids |> select(data_id_merged) |>
  rename(dataset_for_matching = data_id_merged) |> 
  mutate(dataset_for_matching = tolower(dataset_for_matching)) |> 
  dplyr::filter(!dataset_for_matching %in% datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources$dataset_for_matching) |> 
  View() # there are osf and figshare cases with different format. I'll handle it below while preparing the df for joining

# prepare  for joining

colnames(sample_200_non_matched_ids)
any(grepl("[A-Z]", sample_200_non_matched_ids$doi)) # doi is already lowercase
any(grepl("[A-Z]", sample_200_non_matched_ids$data_id_merged)) # verifying dataset_for_matching: one is not lowered!

# I will use "dataset_for_matching", which will be a fit version of "data_id_merged". of course there is no "detected_id" in non-matched.
sample_200_non_matched_ids_for_joining <- sample_200_non_matched_ids |> 
  rename(doi_lc = doi, # already lowercase
         data_availability_statement = `dataset mentioned in DAS`,
         covid_related = covid,
         human_data = `human data`,
         data_access_temp_check = data_access) |> 
  mutate(dataset_for_matching = tolower(data_id_merged), # tolower in order to join succesfully
         dataset_for_matching = case_when(
           # match osf cases to the "dataset_for_matching" format:
           str_detect(data_id_merged, "osf.io/") ~ paste0(
             "osf_", str_extract(data_id_merged, "(?<=osf\\.io/)[a-z0-9]+")
           ),
           
           # in the sample-200 files there were some wrongly formatted figshare cases. return them to the right format:
           data_id_merged == "10.6084/m9.figshare.12054234" ~ "//figshare.com/articles/dataset/association_of_suicidal_behavior_with_exposure_to_suicide_and_suicide_attempt_a_systematic_review_and_multilevel_meta-analysis/12054234?file=22151703",
           data_id_merged == "10.6084/m9.figshare.14383401" ~ "//figshare.com/articles/dataset/raw_data_of_all_mrsa_outbreak_reports_included_/14383401",
           data_id_merged == "10.6084/m9.figshare.14831532" ~ "//figshare.com/articles/dataset/raw_data_of_the_lipidomics_experiments_including_sample_identifiers_/14831532",
           data_id_merged == "10.6084/m9.figshare.16806162" ~ "//figshare.com/articles/dataset/dataset_1_/16806162",
           
           # Default fallback — leave as is
           .default = dataset_for_matching)) |> 
  mutate(doi_id_lc_for_matching = paste(doi_lc, dataset_for_matching, sep = ";")) |> # doi lc ; dataset_for_matching (since there's no detected here)
  select(doi_id_lc_for_matching,
         data_availability_statement,
         human_data,
         covid_related,
         charite_id_year,
         license,
         data_access_temp_check) |> # values are already distcint
  mutate(across(everything(), as.character)) # original values are sometimes 0 / 1)

# prepare datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources for matching

any(grepl("[A-Z]", datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources$doi_lc)) # lc
any(grepl("[A-Z]", datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources$dataset_for_matching)) # lc

datasets_metadata_3_for_matching <- datasets_metadata_master_new_st_all_3_self_join_for_same_ids_dif_sources |> 
  mutate(doi_id_lc_for_matching = paste(doi_lc, dataset_for_matching, sep = ";")) # doi lc ; dataset_for_matching (since there's no detected here)
  
# join by doi_id_lc_for_matching

datasets_metadata_master_new_st_all_4_non_matched <- datasets_metadata_3_for_matching |>
  mutate(charite_id_year = as.character(charite_id_year)) |> # convert to chr
  left_join(sample_200_non_matched_ids_for_joining,
            by = "doi_id_lc_for_matching",
            suffix = c("", ".new")) |> 
  mutate(
    data_availability_statement = coalesce(data_availability_statement, data_availability_statement.new),
    human_data = coalesce(human_data, human_data.new),
    covid_related = coalesce(covid_related, covid_related.new),
    charite_id_year = coalesce(charite_id_year, charite_id_year.new),
    license = coalesce(license, license.new)
  ) |> 
  select(-ends_with(".new"))

# check that data access values match
datasets_metadata_master_new_st_all_4_non_matched |> dplyr::filter(data_access != data_access_temp_check) # yes

datasets_metadata_master_new_st_all_4_non_matched <- datasets_metadata_master_new_st_all_4_non_matched |> 
  select(-data_access_temp_check)

# save
save_cr(datasets_metadata_master_new_st_all_4_non_matched,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_4_non_matched.RData")))

# 3. Resolve case: # datasets_metadata_master_new_st_all_4_non_matched$detected_id == "10.18112/openneuro.ds001226"

# make sure it is an exact duplication besides the "licesne" values which actually mean the same:
datasets_metadata_master_new_st_all_4_non_matched |> 
  dplyr::filter(detected_id == "10.18112/openneuro.ds001226") |> 
  select(license) |> 
  distinct() |> 
  nrow() # 2 with license col

datasets_metadata_master_new_st_all_4_non_matched |> 
  dplyr::filter(detected_id == "10.18112/openneuro.ds001226") |> 
  select(-license) |> 
  distinct() |> 
  nrow() # 1 w/o license col

# remove the "TRUE" one, since the other one is "CCO" - more information about the license type

# get row
row_to_remove <- datasets_metadata_master_new_st_all_4_non_matched |> 
  dplyr::filter(detected_id == "10.18112/openneuro.ds001226" & license == "TRUE")
  
# remove
datasets_metadata_master_new_st_all_5_dedup <- datasets_metadata_master_new_st_all_4_non_matched |> 
  anti_join(row_to_remove, by = c("detected_id", "license"))
  
# save
save_cr(datasets_metadata_master_new_st_all_5_dedup,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_5_dedup.RData")))

# 4. manually add metadata to detected_id = 10.13026/x4td-x982:

# metadata was taken from: here("data/raw/charite/data_articles_AI_2025_05_14.csv")

datasets_metadata_master_new_st_all_6_man <- datasets_metadata_master_new_st_all_5_dedup |> 
  mutate(
    charite_id_year = case_when(
      detected_id == "10.13026/x4td-x982" ~ "2020", .default = charite_id_year),
    license = case_when(
      detected_id == "10.13026/x4td-x982" ~ "CC-BY", .default = license),
    human_data = case_when(
     detected_id == "10.13026/x4td-x982" ~ "TRUE", .default = human_data),
    covid_related = case_when(
      detected_id == "10.13026/x4td-x982" ~ "FALSE", .default = covid_related))

# save
save_cr(datasets_metadata_master_new_st_all_6_man,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_6_man.RData")))

# 5. categorization of ids:

  # Plan:
  # 1. add "is_detected_id_doi" to detected only
  # 2. verify is_gen_rep (change values if necessary)
  # 3. verify Category for original ids (so matched + non matched) (change values if necessary)

# 1. add "is_detected_id_doi" to detected only

# overview
datasets_metadata_master_new_st_all_6_man |> 
  select(detected_id) |> 
  distinct() |> 
  View()

datasets_6_in_progress_is_doi <- datasets_metadata_master_new_st_all_6_man |> 
  mutate(
    is_detected_id_doi = case_when(
      is.na(detected_id) ~ NA_character_,
      grepl("^10\\.", detected_id) ~ "TRUE",
      .default = "FALSE"
    )
  )


# 2. verify is_gen_rep (change values if necessary)

# overview
datasets_6_in_progress_is_doi |> 
  select(dataset_for_matching, is_gen_rep) |> 
  distinct() |>
  arrange(is_gen_rep, dataset_for_matching) |> 
  View()


# st001673 should be "FALSE", but other than that everything looks fine.

# change "st001673"
datasets_6_in_progress_is_gen_rep <- datasets_6_in_progress_is_doi |> 
  mutate(is_gen_rep = as.character(is_gen_rep)) |> # I'll also convert it to chr while I'm at it
  dplyr::mutate(
    is_gen_rep = case_when(
      dataset_for_matching == "st001673" ~ "FALSE",
      .default = is_gen_rep))

# 3. verify Category for dataset_for_matching ids (so matched + non matched) (change Category values if necessary)

# overview

# matched
datasets_6_in_progress_is_gen_rep |> 
  dplyr::filter(in_dcc == "TRUE") |> 
  select(data_id_lc, data_id_secondary, dataset_for_matching, detected_id, Category) |> 
  distinct() |> 
  View() # looks fine

# non-matched (acc_nr)
datasets_6_in_progress_is_gen_rep |> 
  dplyr::filter(in_dcc == "FALSE") |> 
  select(data_id_lc, data_id_secondary, dataset_for_matching, Category) |> 
  distinct() |> 
  dplyr::filter(Category == "acc_nr") |> 
  View() # looks fine

  # non-matched (rest of values)
datasets_6_in_progress_is_gen_rep |> 
  dplyr::filter(in_dcc == "FALSE") |> 
  select(data_id_lc, data_id_secondary, dataset_for_matching, Category) |> 
  distinct() |>
  dplyr::filter(Category != "acc_nr") |> 
  View() # looks fine. "url" osfs were also in the numbat list to match with DCC.
  

datasets_metadata_master_new_st_all_7_is_doi <- datasets_6_in_progress_is_gen_rep

# save
save_cr(datasets_metadata_master_new_st_all_7_is_doi,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_7_is_doi.RData")))

# 6. quality assurance

# 6.1. check inconsistencies in metadata

# without DAS
datasets_metadata_master_new_st_all_7_is_doi |>
  dplyr::group_by(detected_id) |>
  dplyr::summarise(
    # for each column, check if there is more than 1 unique value
    across(
      c(human_data, covid_related, charite_id_year, license),
      ~ n_distinct(.) > 1,     # TRUE = inconsistent within group
      .names = "check_{.col}"  # Name output columns like: check_human_data, etc.
    )
  ) |>
  dplyr::filter(if_any(starts_with("check_"), ~ .)) |> 
  View() # no issues

# only DAS
datasets_metadata_master_new_st_all_7_is_doi |>
  dplyr::group_by(detected_id) |>
  dplyr::summarise(
    across(data_availability_statement,
      ~ n_distinct(.) > 1,
      .names = "check_{.col}"
    )
  ) |>
  dplyr::filter(if_any(starts_with("check_"), ~ .)) |> 
  View()

# 6.2. check that doi id orig and clean pairs are the real doi and id cols

datasets_metadata_master_new_st_all_7_is_doi |>
  mutate(
    # Combine doi_charite and data_identifier with a semicolon
    doi_id_orig_check = paste(doi_charite, data_identifier, sep = ";"),
    
    # Combine doi_no_ver_info and dataset_for_matching
    doi_id_clean_check = paste(doi_no_ver_info, dataset_for_matching, sep = ";"),
    
    # Compare pasted versions to reference columns
    doi_id_orig_match = doi_id_orig_check == doi_id_orig_pair,
    doi_id_clean_match = doi_id_clean_check == doi_id_clean_pair
  ) |> 
  dplyr::filter(
    doi_id_orig_match == FALSE |
      doi_id_clean_match == FALSE
  ) |> 
  View()


# 6.3. check that all non-matched 200 are here with metadata

datasets_metadata_master_new_st_all_7_is_doi |> 
  dplyr::filter(in_dcc == "FALSE") |> 
  dplyr::filter(!is.na(covid_related)) |> 
  View() # yes



# 7. standardize metadata values (e.g. "0"/"1" should be "FALSE"/"TRUE")

# overview
str(datasets_metadata_master_new_st_all_7_is_doi$charite_id_year)
datasets_metadata_master_new_st_all_7_is_doi |> select(charite_id_year) |> unique()
datasets_metadata_master_new_st_all_7_is_doi |> select(data_availability_statement) |> unique()
datasets_metadata_master_new_st_all_7_is_doi |> select(license) |> unique()
datasets_metadata_master_new_st_all_7_is_doi |> select(covid_related) |> unique()
datasets_metadata_master_new_st_all_7_is_doi |> select(human_data) |> unique()
datasets_metadata_master_new_st_all_7_is_doi |> select(data_access) |> unique()
datasets_metadata_master_new_st_all_7_is_doi |> select(is_detected_id_doi) |> unique()

# recode values

datasets_metadata_master_new_st_all_8_std <- datasets_metadata_master_new_st_all_7_is_doi |> 
  mutate(
    # DAS
    data_availability_statement = 
      case_when(
        data_availability_statement %in% c("yes", "y", "1") ~ "TRUE",
        data_availability_statement %in% c("no", "data_ref_not_in_das", "data not in DAS", "0") ~ "FALSE",
        .default = data_availability_statement),
    # license
    license = 
      case_when(
        license %in% c("no license", "no", "0") ~ "FALSE",
        .default = license),
    # covid_related
    covid_related = 
      case_when(
        covid_related %in% c("yes", "1") ~ "TRUE",
        covid_related %in% c("no", "n", "0") ~ "FALSE",
        .default = covid_related),
    # human_data
    human_data = 
      case_when(
        human_data %in% c("yes", "y", "1") ~ "TRUE",
        human_data %in% c("no", "0") ~ "FALSE",
        .default = human_data))
             
    

# check again:
datasets_metadata_master_new_st_all_8_std |> select(charite_id_year) |> unique()
datasets_metadata_master_new_st_all_8_std |> select(data_availability_statement) |> unique()
datasets_metadata_master_new_st_all_8_std |> select(license) |> unique()
datasets_metadata_master_new_st_all_8_std |> select(covid_related) |> unique()
datasets_metadata_master_new_st_all_8_std |> select(human_data) |> unique()
datasets_metadata_master_new_st_all_8_std |> select(data_access) |> unique()
datasets_metadata_master_new_st_all_8_std |> select(is_detected_id_doi) |> unique()

# save
save_cr(datasets_metadata_master_new_st_all_8_std,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_8_std.RData")))


# remove authors and publication year of doi_charite
# I already have them in "all_sources" (the df which the metadata will be joined to)
# and "authors" had a value that was too long to save as xlsx

datasets_metadata_master_new_st_all_9_done <- datasets_metadata_master_new_st_all_8_std |> 
  select(-c(authors_charite, publication_year_charite))
  
# save: this will become 013 below
save_cr(datasets_metadata_master_new_st_all_9_done,
        file = file.path(here("data", "verification", "metadata_new_structure",
                              "datasets_metadata_master_new_st_all_9_done.RData")))

datasets_metadata_master_updated_013 <- datasets_metadata_master_new_st_all_9_done

# save
metadata_update(datasets_metadata_master_updated_013) # call function to save as csv, xlsx, rda


# 014: correct mt108784 to covid_related = "TRUE" ------------------------

# load
load_latest_metadata_update() # call function to load latest version


# check where there's more than 1 metadata value for the same dataset:
datasets_metadata_master_updated_013 |>
  select(detected_id,
         license,
         human_data,
         covid_related,
         is_detected_id_doi,
         data_availability_statement # will be taken care of separately, if it will be a problem
         # charite_id_year, # was checked in joined_bind qmd and was ok
         #repository # will be checked and corrected in joined_bind qmd
  ) |> 
  group_by(detected_id) |>
  summarise(across(everything(), n_distinct), .groups = "drop") |>
  dplyr::filter(if_any(-detected_id, ~ . > 1)) |> 
  View()

datasets_metadata_master_updated_014 <- datasets_metadata_master_updated_013 |> 
  mutate(covid_related = case_when(detected_id == "mt108784" ~ "TRUE", .default = covid_related))

# save
metadata_update(datasets_metadata_master_updated_014) # call function to save as csv, xlsx, rda

# 015: fix DAS issue ------------------------------------------------------

# Tasks:
#   1. Mutate these values: "No_statement_no_data, yes_statement_no_data" to FASLE (so that there's only TRUE/FALSE values)
#      (This has been decided after consulting with Evgeny)
#   2. Resolve contradicting values of DAS for the same identifier
#     a. find cases with contradictory DAS values for the same identifier
#     b. set 1 value for each of these cases based on the earliest charite id year of them
#     c. if something is still missing, send to Blanka

# The new values will be under "das_for_analysis".

# load
load_latest_metadata_update() # call function to load latest version

# 1. Mutate these values: "no_statement_no_data, yes_statement_no_data" to FASLE (so that there's only TRUE/FALSE values)

# check:
datasets_metadata_master_updated_014 |> 
  select(data_availability_statement) |> 
  unique()

# change:
datasets_metadata_master_updated_015 <- datasets_metadata_master_updated_014 |> 
  mutate(
    das_for_analysis = 
      case_when(data_availability_statement %in% c("no_statement_no_data", "yes_statement_no_data")
                ~ "FALSE",
                .default = data_availability_statement))

# verify:
datasets_metadata_master_updated_015 |> 
  select(das_for_analysis) |> 
  unique() # good

# 2. The second issue is that for the same id there are multiple DAS values, because it appears in multiple Charité papers:

datasets_metadata_master_updated_015 |>
  dplyr::select(
    doi_no_ver_info, # was verified against doi_charite that it gives the same results
    dataset_for_matching,
    data_availability_statement,
    in_dcc
  ) |>
  dplyr::group_by(dataset_for_matching) |>
  dplyr::filter(
    dplyr::n_distinct(doi_no_ver_info, na.rm = TRUE) > 1 &
      dplyr::n_distinct(data_availability_statement, na.rm = TRUE) > 1
  ) |>
  dplyr::ungroup() |> 
  distinct() |> 
  View()

# So for each of the ids I checked all of their Charité papers and determined which was the earliest paper publication date.
# This way, I could set the valuae of the id by the value that the id have under the eraliest doi publication.
# This was done manually in excel, and resulted the following:
#
# 10.1002/eji.202048797	is the earliest Charité paper with gse160097
# 10.1038/s41596-019-0251-6	is the earliest Charité paper with gse90496
# 10.1038/s41586-020-2294-9	is the earliest Charité paper with mt108784
# 10.1111/jgh.15071	is the earliest Charité paper with prjna540738
# 10.1016/j.cell.2020.04.018 is the earliest Charité paper with gse84795

# Under all earliest publications, DAS == "TRUE", with the exception of "gse84795" (where it's "FALSE").

# So below I'll set these values for every occurence of
# gse160097, gse90496, mt108784, prjna540738 ad gse84795 under dataset_for_matching:

datasets_metadata_master_updated_015 <- datasets_metadata_master_updated_015 |> 
  mutate(
    das_for_analysis = # mutate a column for analysis
      case_when(
        dataset_for_matching %in% c("gse160097", "gse90496", "mt108784", "prjna540738")
        ~ "TRUE",
        dataset_for_matching == "gse84795"
        ~ "FALSE",
        .default = data_availability_statement))

# verify:
datasets_metadata_master_updated_015 |>
  dplyr::select(
    doi_no_ver_info, 
    dataset_for_matching,
    das_for_analysis, # this time I'm checking das_for_analysis
    in_dcc
  ) |>
  dplyr::group_by(dataset_for_matching) |>
  dplyr::filter(
    dplyr::n_distinct(doi_no_ver_info, na.rm = TRUE) > 1 &
      dplyr::n_distinct(das_for_analysis, na.rm = TRUE) > 1
  ) |>
  dplyr::ungroup() |> 
  distinct() |> 
  View()

# save
metadata_update(datasets_metadata_master_updated_015) # call function to save as csv, xlsx, rda


# 016: add license_for_analysis again --------------------------------------

# licesne_for_analysis should be added here

# load
load_latest_metadata_update() # call function to load latest version

datasets_metadata_master_updated_016 <- datasets_metadata_master_updated_015 |> 
  mutate(
    license_for_analysis = case_when(
      license %in% c("CC0", "CC BY", "cc-by", " CC BY-NC-SA", "cc0", "CC-BY", "CC BY-NC-SA", "CC BY-NC-ND") ~ "TRUE",
      license %in% c("FALSE", "data use agreement", "bespoke use terms", "EMBL-EBI", "DUC", "DUL, DUC", "DTA", "other (open)") ~ "FALSE",
      .default = license
    ))

datasets_metadata_master_updated_016 |> select(license_for_analysis) |> unique() # check

# save
metadata_update(datasets_metadata_master_updated_016) # call function to save as csv, xlsx, rda

# 017: remove trailing ws -------------------------------------------------

# load
load_latest_metadata_update() # call function to load latest version

# check where
cols_with_trailing_ws <- datasets_metadata_master_updated_016 |>
  dplyr::select(where(is.character)) |>
  dplyr::summarise(
    dplyr::across(everything(), ~ any(stringr::str_detect(., "\\s+$")))
  ) |>
  tidyr::pivot_longer(everything(), names_to = "column", values_to = "has_trailing_ws") |>
  dplyr::filter(has_trailing_ws) |>
  dplyr::pull(column)

# remove
datasets_metadata_master_updated_017 <- datasets_metadata_master_updated_016 |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(cols_with_trailing_ws),
      ~ stringr::str_replace(., "\\s+$", "")
    )
  )

# save
metadata_update(datasets_metadata_master_updated_017) # call function to save as csv, xlsx, rda


# 018: Rejoining metadata after src-ds-ws-fix run -------------------------

# numbat+da, ds+added and joined qmd files were executed again
# in order to fix source and whitespaces issues and in order to add 2 more datastet matches.
# The structure didn't change much except clean doi;id;source col that was added.
# So here I'll re-add the metadata

load_latest_metadata_update() # call function to load latest version

# check for ws in latest metadata:
datasets_metadata_master_updated_017 |> 
  select(data_id_secondary) |> 
  dplyr::filter(str_detect(data_id_secondary, "\\s")) |> 
  distinct()

datasets_metadata_master_updated_017 |> 
  select(dataset_for_matching) |> 
  dplyr::filter(str_detect(dataset_for_matching, "\\s")) |> 
  distinct() # one case of non-matched, which is ok, because it wasn't supposed to match anyway.

datasets_metadata_master_updated_017 |> 
  select(detected_id) |> 
  dplyr::filter(str_detect(detected_id, "\\s")) |> 
  distinct()

# None.

# check for difference in cases between 017 and all_sources_6

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_6_pair_fix.RData"))

# create a version of 017 without additional_ids_in_numbat and with datastet_in_numbat renamed to datastet and only in dcc
t_metadata <- datasets_metadata_master_updated_017 |> 
  dplyr::filter(!is.na(detected_id)) |> 
  select(doi_no_ver_info, detected_id, source_charite) |> 
  mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet", .default = source_charite)) |> 
  dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
  distinct()

# verify that I can trust in_dcc in the same way I trust !is.na(detected_id)
# 
# t_metadata_2 <- datasets_metadata_master_updated_017 |> 
#   dplyr::filter(in_dcc == "TRUE") |> 
#   select(doi_no_ver_info, detected_id, source_charite) |> 
#   mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet", .default = source_charite)) |> 
#   dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
#   distinct()
# 
# waldo::compare(t_metadata, t_metadata_2)
# waldo::compare(t_metadata_2, t_metadata)
#
# yes.

t_detected <- dcc_detected_ids_all_sources_6_pair_fix |> 
  select(doi_no_ver_info, detected_id, source_charite) |> 
  distinct()


anti_join(t_metadata, t_detected) |> distinct() |> View()
anti_join(t_detected, t_metadata) |> distinct() |> View() # only the 3 new datastet cases (1 of them is already in numbat)

waldo::compare(t_metadata, t_detected)
waldo::compare(t_detected, t_metadata)


# Plan:

# So the current 017 includes, according to the inspection above:
# * detected datastet and additional ids
# * detected numbat
# * non-detected numbat (including 200 with metadata)
#
# What is supposed to be different from 017 and the newest dcc_detected_ids_all_sources_6_pair_fix cases?
#
# * no additional-ids in numbat
# * datastet in numbat should be datastet
# +2 datastet cases
# +1 datastet case that is already in numbat
#
# So that means that I have to:
# 
# 1. add the 3 datastet cases
# 2. rename datastet_in_numbat to datastet
# 3. remove additional_ids_in_numbat cases
#

# so let's try to make it simple:
# 1. use all_sources_6's charite cols to join 017
# 2. manually correct and add according to the 1. 2. 3. just above
# 3. check differences between 017, 018 and _6
# 4. bind non-matched from 017

dcc_detected_ids_all_sources_6_for_joining <- dcc_detected_ids_all_sources_6_pair_fix |> 
  # select only charite columns
  select(-c(26:49)) |> 
  distinct()

metadata_017_for_joining <- datasets_metadata_master_updated_017 |>
  # rename "datastet_in_numbat" source
  mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet", .default = source_charite)) |> 
  # exclude "additional_ids_in_numbat" source
  dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
  # create clean doi+id+source col for joining
  mutate(doi_id_clean_pair_with_source =paste(doi_no_ver_info, detected_id, source_charite, sep = ";")) |> 
  # select only key and metadata columns
  select(doi_id_clean_pair_with_source, c(26:35)) |> 
  distinct()

# join
test_018_only_matched <- dcc_detected_ids_all_sources_6_for_joining |>
  left_join(metadata_017_for_joining, by = "doi_id_clean_pair_with_source")

# check 018 against _6:

t_md <- test_018_only_matched |> 
  select(doi_id_clean_pair_with_source) |> 
  distinct()
  
t_as <- dcc_detected_ids_all_sources_6_pair_fix |> 
  select(doi_id_clean_pair_with_source) |> 
  distinct()

anti_join(t_md, t_as) |> distinct()
anti_join(t_as, t_md) |> distinct()
waldo::compare(t_md, t_as)
waldo::compare(t_as, t_md)

# matched are correct! good.

# check 017  against _6 (doi_id_clean_pair_with_source)
t_17 <- datasets_metadata_master_updated_017 |>
  dplyr::filter(in_dcc == "TRUE") |>
  mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet", .default = source_charite)) |> 
  dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
  mutate(doi_id_clean_pair_with_source =paste(doi_no_ver_info, detected_id, source_charite, sep = ";")) |> 
  select(doi_id_clean_pair_with_source) |>
  distinct()

anti_join(t_17, t_as) |> distinct()
anti_join(t_as, t_17) |> distinct()

waldo::compare(t_17, t_as)
waldo::compare(t_as, t_17)

setdiff(t_md, t_as)
setdiff(t_as, t_md)
setdiff(t_md, t_as)
setdiff(t_as, t_md)

# matched only also have the same columns! (018 matched, _6, 017 matched w/ renamed sources and doi+id+source)

# So only the 3 added cases are the difference! good.

# verify that you don't have the unwanted source_charite values in in_dcc == "FALSE"
datasets_metadata_master_updated_017 |>
  dplyr::filter(in_dcc == "FALSE") |>
  dplyr::filter(source_charite %in% c("datastet_in_numbat", "datastet_in_numbat"))

# Bind non-matched cases to 018
datasets_metadata_master_updated_018 <- test_018_only_matched |> 
  mutate(is_gen_rep = as.character(is_gen_rep)) |> 
  bind_rows(
    datasets_metadata_master_updated_017 |>
      mutate(doi_id_clean_pair_with_source =paste(doi_no_ver_info, detected_id, source_charite, sep = ";")) |> 
      dplyr::filter(in_dcc == "FALSE"))

# verify that numebr of rows make sense:
datasets_metadata_master_updated_018 |> nrow() # 2193

datasets_metadata_master_updated_017 |>
  mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet", .default = source_charite)) |> 
  #dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
  mutate(doi_id_clean_pair_with_source =paste(doi_no_ver_info, detected_id, source_charite, sep = ";")) |> 
  nrow() # 2194

# It makes sense, since I deleted 4 "additional_ids_in_numbat" cases and added 3 "datastet" cases. 

# complete info of 3 added datastet cases:

  # 10.17863/cam.23511 (datastet)
  # 10.17863/cam.87955 (datastet)
  # 10.6084/m9.figshare.12436517 (datastet - already in numbat)

# 10.6084/m9.figshare.12436517: take info from Numbat entry of the same ID and DOI:

datasets_metadata_master_updated_018 <- datasets_metadata_master_updated_018 |> 
  mutate(
    in_dcc = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = in_dcc),
    data_availability_statement = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = data_availability_statement),
    human_data = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = human_data),
    covid_related = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = covid_related),
    charite_id_year = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "2020", .default = charite_id_year),
    license = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "CC BY", .default = license),                                
    is_detected_id_doi = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = is_detected_id_doi),
    das_for_analysis = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = das_for_analysis),
    license_for_analysis = case_when(
      detected_id == "10.6084/m9.figshare.12436517" & source_charite == "datastet" ~ "TRUE", .default = license_for_analysis))

# and for the other 2 I'll complete the info manually (from _v9)

datasets_metadata_master_updated_018 <- datasets_metadata_master_updated_018 |> 
  mutate(
    in_dcc = case_when(detected_id %in% c("10.17863/cam.23511", "10.17863/cam.87955") ~ "TRUE", .default = in_dcc),
    covid_related = case_when(detected_id %in% c("10.17863/cam.23511", "10.17863/cam.87955") ~ "no", .default = covid_related),
    charite_id_year = case_when(detected_id %in% c("10.17863/cam.23511", "10.17863/cam.87955") ~ "2023", .default = charite_id_year),
    license = case_when(detected_id %in% c("10.17863/cam.23511", "10.17863/cam.87955") ~ "CC BY", .default = license),                                
    is_detected_id_doi = case_when(detected_id %in% c("10.17863/cam.23511", "10.17863/cam.87955") ~ "TRUE", .default = is_detected_id_doi),
    license_for_analysis = case_when(detected_id %in% c("10.17863/cam.23511", "10.17863/cam.87955") ~ "TRUE", .default = license_for_analysis),
    
    human_data = case_when(
      detected_id == "10.17863/cam.23511" ~ "yes",
      detected_id == "10.17863/cam.87955" ~ "no", .default = human_data),
    data_availability_statement = case_when(
      detected_id == "10.17863/cam.23511" ~ "yes",
      detected_id == "10.17863/cam.87955" ~ "not in DAS", .default = data_availability_statement),
    das_for_analysis = case_when(
      detected_id == "10.17863/cam.23511" ~ "TRUE",
      detected_id == "10.17863/cam.87955" ~ "FALSE", .default = das_for_analysis))

# Verify NA / "" in matched metadata:

# check Blanks
datasets_metadata_master_updated_018 |> 
  dplyr::filter(in_dcc == "TRUE") |> 
  dplyr::filter(
    data_access == ""
    | license == ""
    | human_data == ""
    | covid_related == ""
    | is_detected_id_doi == ""
    | data_availability_statement == ""
    | charite_id_year == ""
    | das_for_analysis == ""
    | license_for_analysis == ""
    ) |>  View() # no blanks

# check NAs
datasets_metadata_master_updated_018 |> 
  dplyr::filter(in_dcc == "TRUE") |> 
  dplyr::filter(
    is.na(data_access)
    | is.na(license)
    | is.na(human_data)
    | is.na(covid_related)
    | is.na(is_detected_id_doi)
    #| is.na(data_availability_statement)
    | is.na(charite_id_year)
    #| is.na(das_for_analysis)
    | is.na(license_for_analysis)
  ) |> View() # there are some, but only under "data_availability_statement" and "das_for_analysis" (commented out for verification)

# which?
which_ids <- datasets_metadata_master_updated_018 |> 
  dplyr::filter(in_dcc == "TRUE") |> 
  dplyr::filter(
    is.na(data_availability_statement)
    | is.na(das_for_analysis)
  ) |>
  select(doi_no_ver_info, detected_id, source_charite) |> 
  distinct()

# verify that they actually do have metadata under their numbat entry
datasets_metadata_master_updated_017 |> 
  dplyr::filter(detected_id %in% which_ids$detected_id) |> 
  select(doi_no_ver_info,
         detected_id,
         source_charite,
         data_availability_statement,
         das_for_analysis) |> 
  distinct() |> 
  View()

# verify that they actually do have metadata under their numbat entry
datasets_metadata_master_updated_018 |> 
  dplyr::filter(detected_id %in% which_ids$detected_id) |> 
  select(doi_no_ver_info,
         detected_id,
         source_charite,
         data_availability_statement,
         das_for_analysis) |> 
  distinct() |> 
  # dplyr::filter(is.na(data_availability_statement) & is.na(das_for_analysis)) |> 
  # select(detected_id) |> 
  # distinct() |> 
  arrange(detected_id, doi_no_ver_info) |> 
  View() # they do

# verify that they are all "datastet_in_numbat or additional_ids_in_datasete"
datasets_metadata_master_updated_017 |> 
  dplyr::filter(detected_id %in% which_ids$detected_id) |> 
  select(doi_no_ver_info,
         detected_id,
         source_charite,
         data_availability_statement,
         das_for_analysis) |> 
  distinct() |> 
  # dplyr::filter(is.na(data_availability_statement) & is.na(das_for_analysis)) |> 
  # select(detected_id) |> 
  # distinct() |> 
  arrange(detected_id, doi_no_ver_info) |> 
  View() # they are

# So until now almost everything is completed.
# I still need to complete DAS and DAS for analysis values, if necessary.

# correct last das_for_analysis and the last 2 "missed" datastet cases that were added recently:

datasets_metadata_master_updated_018 <- datasets_metadata_master_updated_018 |> 
  mutate(
    das_for_analysis = 
      case_when(das_for_analysis == "no_statement_no_data"
              ~ "FALSE",
              .default = das_for_analysis),
    human_data =
      case_when(
        human_data == "yes" ~ "TRUE",
        human_data == "no" ~ "FALSE",
        .default = human_data),
    covid_related =
      case_when(
        covid_related == "yes" ~ "TRUE",
        covid_related == "no" ~ "FALSE",
        .default = covid_related))

# save
metadata_update(datasets_metadata_master_updated_018) # call function to save as csv, xlsx, rda

# 19. Finish completing DAS and das_for_analysis values -------------------

# Here I'll complete DAS and DAS for analysis values, if necessary, and then choose the earlier DOI to set the DAS value.
# I'll start with an inspection of the values (take from chunk above).





