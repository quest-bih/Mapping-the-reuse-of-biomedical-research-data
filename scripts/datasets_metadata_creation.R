# This script adds metadata of Charité datasets listed in "charite_dois_and_ids_8_for_matching.RData" to create a metadata file.
# the output is the same table but with the following added columns:
# license, human/not, covid related (yes/no), data access (yes/restricted), doi/acc_n, repository, is in DAS, year ,matched/not with DCC

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

# Load Charité datasets for matching with the DCC

load(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_8_for_matching.RData"))

# 2. Add Metadata ---------------------------------------------------------

# Note: data_access column already exists and filled for Numabt source datasets! So no need to create it here.

### Load human/not & covid relatet (yes/no): from distinct_id_list - covid or not.xlsx

covid_and_human_metadata <- read.csv(
  file.path(here("data",
                 "verification",
                 "covid",
                 "distinct_id_list - covid or not.csv")),
  header = TRUE) |> 
  as_tibble() |> 
  rename(covid_related = 2,
         human_data = 3) |> 
  select(1:3) |> 
  mutate(
    covid_related = case_when(
      covid_related == "1" ~ "TRUE",
      covid_related == "0" ~ "FALSE",
      is.na(covid_related) ~ ""),
    human_data = case_when(
      human_data == "1" ~ "TRUE",
      human_data == "0" ~ "FALSE",
      is.na(human_data) ~ ""),
    charite_data_id_or_acc_nr = tolower(charite_data_id_or_acc_nr))

find_duplicates(covid_and_human_metadata, charite_data_id_or_acc_nr, covid_related)

# Add

datasets_metadata_1_covid_and_human <- charite_dois_and_ids_8_for_matching |>
  
  # 1st join: match by data_id
  left_join(
    covid_and_human_metadata,
    by = c("data_id" = "charite_data_id_or_acc_nr")
  ) |>
  
  # Rename the joined columns to avoid collision
  rename(covid_related_1 = covid_related,
         human_data_1 = human_data) |> 
  
  # 2nd join: match by data_id_merged
  left_join(
    covid_and_human_metadata,
    by = c("data_id_merged" = "charite_data_id_or_acc_nr")
  ) |> 
  
  # Merge metadata columns
  mutate(
    covid_related = coalesce(covid_related_1, covid_related),
    human_data = coalesce(human_data_1, human_data)
  ) |> 
  select(-covid_related_1, -human_data_1) # remove excess columns

### doi/acc_n

datasets_metadata_2_doi_or_acc_nr <- datasets_metadata_1_covid_and_human |> 
  mutate(dataset_is_doi = case_when(
    str_starts(data_id_merged, "10.")
    ~"TRUE",
    .default = "FALSE"
  ))

### repository: from dcc_charite_joined_5_rm_au_ov

load(here("data", "wrangling_steps", "dcc_charite", "dcc_charite_joined_5_rm_au_ov.RData"))

distinct_ids_and_repos <- dcc_charite_joined_5_rm_au_ov |>
  dplyr::filter(
    !(data_id_merged == "10.17632/btchxktzyw" & 
        repository %in% c("elsevier bv", "mendeley"))) |> # 2 extra rows due to duplication in DCC metadata
  select(data_id_merged, repository) |>
  distinct()

datasets_metadata_3_repo <- datasets_metadata_2_doi_or_acc_nr |> 
  left_join(distinct_ids_and_repos, by = "data_id_merged") |> 
  mutate(repository = case_when(
    str_detect(data_id_merged, "osf")
    ~ "osf",
    .default = repository))


### publication year of dataset

# Get publication year metadata

datasets_publication_years <- read.csv(
  file.path(here("data",
                 "verification",
                 "datasets years",
                 "datasets_years_filled_AC_v4.csv")),
  header = TRUE) |> 
  select(charite_data_id_or_acc_nr, charite_id_year) |>
  distinct()

# Add

datasets_metadata_4_year <- datasets_metadata_3_repo |> 
  left_join(datasets_publication_years,
            by = c("data_id_merged" = "charite_data_id_or_acc_nr"))

### "found_in_dcc" (T/F): This is in order to sample 170 non-matched values for Blanka


datasets_metadata_5_dcc_match <- datasets_metadata_4_year |> 
  mutate(in_dcc = case_when(
    data_id_merged %in% dcc_charite_joined_5_rm_au_ov$data_id_merged
    ~ "TRUE",
    .default = "FALSE"))


### is in DAS: from numbat master 2020-2023

load(here("data", "raw", "charite", "master_od_screening_manual_check_2020_2023_v2.rda"))

# check for encoding issues

master_2020_2023_1_unique <- master_2020_2023 |> 
  mutate(unique_id = row_number()) |> 
  relocate(unique_id, .before = 1) # add unique id

View(master_2020_2023_1_unique[!stringi::stri_enc_isutf8(master_2020_2023_1_unique$data_identifier), ]) # view issues

# Both cases where there's a doi and id values are not relevant, and will be removed below

charite_2020_2023 <- master_2020_2023_1_unique |>
  dplyr::filter(!unique_id %in% c(2870, 3184)) |> # remove 2 irrelevant cases with encoding issues
  select(-unique_id) |>  # remove temporary unique_id column
  mutate(doi = tolower(doi),
         data_identifier = tolower(data_identifier)) |> # tolower
  mutate(doi = na_if(doi, "na")) |> # replace "na" with true NA
  dplyr::filter(!(is.na(doi))) |> # filter out cases where doi is NA
  mutate(doi_no_ver_info = str_remove(doi, "\\.[0-9]$")) |> # remove version information from DOIs
  relocate(doi_no_ver_info, .after = 1) # relocate new column

bad_vals <- c("NA", "NULL", "", "null", "na") # define values to filter out

# get metadata

datasets_das <- charite_2020_2023 |> 
  select(doi, data_identifier, data_availability_statement) |> 
  dplyr::filter(
    !(
      is.na(data_identifier)
      | is.na(data_availability_statement)
      | data_identifier %in% bad_vals
      | data_availability_statement %in% bad_vals
    )
  ) |> 
  distinct()

# add metadata

datasets_metadata_6_das <- datasets_metadata_5_dcc_match |> 
  left_join(datasets_das,
            by = c("doi" = "doi", "data_id" = "data_identifier"))

# Find cases with different DAS values for the same DOI+ID combination:

datasets_metadata_6_das |>
  group_by(unique_id) |>
  summarise(n = n()) |>
  dplyr::filter(n > 1)

# filter them out:

datasets_metadata_6_das <- datasets_metadata_6_das |>
  distinct(unique_id, .keep_all = TRUE)


### license (yes/no): from numbat master 2020-2023 and blanka's data articles file

# get metadata

# Notice:
# NULL in license name is either no license OR no answer.
  # (i) "dataset_license" answer "no"? "NULL" in license_name means no license.
  # (ii) "dataset_license" is anything else? "NULL" in license_name means no answer.


datasets_lic <- charite_2020_2023 |> 
  # select only relevant columns
  select(doi, data_identifier, dataset_license, license_name) |> 
  # filter out irrelevant cases (no id or licesne = NA / blank)
  dplyr::filter(
    !(
      is.na(data_identifier)
      | is.na(license_name)
      | data_identifier %in% bad_vals
      | license_name %in% c("NA", "", " ", "na")
    )
  ) |>
  distinct() |> 
  # create a new "license" column based on the conditions above this chunk
  mutate(license = case_when(
    dataset_license == "no"
    & license_name %in% c("null", "NULL")
    ~ "no license",
    .default = license_name)) |> 
  select(-c(dataset_license, license_name)) |> 
  distinct() |> 
  group_by(doi, data_identifier) |>
  dplyr::filter(!(license == "NULL" & any(license != "NULL"))) |>
  ungroup()

# add metadata

datasets_metadata_7_lic <- datasets_metadata_6_das |> 
  left_join(datasets_lic,
            by = c("doi" = "doi", "data_id" = "data_identifier"))


# save
  save_cr(datasets_metadata_7_lic, file = file.path(here(
    "data",
    "verification",
    "metadata all",
    "datasets_metadata_7_lic.RData")))

# Save "datasets_metadata_7_lic" as the final template to update:
  # Notice: the updates are taking place in datasets_metadata_updates.R!

# assign
datasets_metadata_master_updated_001 <- datasets_metadata_7_lic

# save
save_cr(datasets_metadata_master_updated_001, file = file.path(here(
  "data",
  "verification",
  "metadata all",
  "datasets_metadata_master_updated",
  "rda",
  "datasets_metadata_master_updated_001.RData"))) #rdata


write_csv_cr(datasets_metadata_master_updated_001, file = file.path(here(
  "data",
  "verification",
  "metadata all",
  "datasets_metadata_master_updated",
  "csv",
  "datasets_metadata_master_updated_001.csv")),
  row.names = FALSE) # csv


write_xlsx_cr(
  datasets_metadata_master_updated_001,
  file = file.path(here(
    "data",
    "verification",
    "metadata all",
    "datasets_metadata_master_updated",
    "xlsx",
    "datasets_metadata_master_updated_001.xlsx"
  ))) # xlsx