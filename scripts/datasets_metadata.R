# This script adds metadata of Charité datasets listed in "charite_dois_and_ids_8_for_matching.RData".
# the output is the same table but with the following added columns:
# license, human/not, covid related (yes/no), data access (yes/restricted), doi/acc_n, repository, is in DAS, year ,matched/not with DCC

# 1. Setup ----------------------------------------------------------------

Sys.setenv(LANG = "EN")  # Set environment language to English

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, readxl, lubridate, stringi)

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

# Load Charité datasets

load(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_8_for_matching.RData"))

# 2. Add Metadata ---------------------------------------------------------

# Note: data_access column already exists and filled for Numabt source datasets! So no need to create it here.

# Deal with duplicates that have only one valid value in another column:
bad_vals <- c("NA", "NULL", "")

# find these values
cases <- master_for_prep |>
  group_by(data_identifier) |>
  dplyr::filter(
    n_distinct(license_name, na.rm = FALSE) > 1,  # more than one unique value
    any(is.na(license_name) | license_name %in% bad_vals | is.null(license_name)),  # at least one bad
    any(!(is.na(license_name) | license_name %in% bad_vals | is.null(license_name)))  # at least one good
  ) |>
  ungroup() |> 
  dplyr::filter(!(data_identifier %in% bad_vals | is.na(data_identifier)))

# filter them
get_unique_with_no_bad_vals <- df |> 
  dplyr::filter(!(is.na(col2) | is.null(col2) | col2 %in% bad_vals)) |> 
  distinct(col1, .keep_all = TRUE)


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


##### Break to print a preview for the team:


datasets_master_metadata <- datasets_metadata_5_dcc_match |> 
  select(-c(data_id_auto_cleaned, data_id_no_ex_chr, data_id_m_val))

# save
save_cr(datasets_master_metadata,
        file = file.path(here("data", "verification","metadata all", "datasets_master_metadata.RData")))

# write as csv
write_csv_cr(
  datasets_master_metadata,
  file = here("data",
              "verification",
              "metadata all",
              "datasets_master_metadata.csv"),
  row.names = FALSE
)

#####

### is in DAS: from numbat master 2020-2023

load(here("data", "raw", "charite", "master_od_screening_manual_check_2020_2023_v2.rda"))

# get metadata

master_for_prep <- master_2020_2023 |>
  select(data_identifier, license_name, data_availability_statement)

master_2020_2023 |>
  #dplyr::filter(!(is.na(data_identifier) | data_identifier == "NULL")) |> 
  select(doi, data_identifier, license_name, data_availability_statement) |>
  distinct() |>
  group_by(data_identifier) |>
  dplyr::filter(
    (
      sum(is.na(license_name) | license_name == "NULL") > 1 &
        sum(!(is.na(license_name) | license_name == "NULL")) > 0
    ) |
      (
        sum(is.na(data_availability_statement) | data_availability_statement == "NULL") > 1 &
          sum(!(is.na(data_availability_statement) | data_availability_statement == "NULL")) > 0
      )
  ) |>
  ungroup() |> 
  View()

### license (yes/no): from numbat master 2020-2023 and blanka's data articles file

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





# 3. sample 170 non matched -----------------------------------------------

first_30 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_30_ids_no_citation.csv")),
  header = TRUE)

# Verify that 
# all(first_30$charite_data_id_or_acc_nr_merged %in% datasets_master_metadata$data_id_merged)

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

