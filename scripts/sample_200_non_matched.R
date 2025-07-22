# this script samples 200 datasets out of the datasets that weren't detected in DCC
# Metadata of these 200 datasets  will be extracted to compare with the metadata of the detected ones.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, htmlwidgets, readxl)

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


# 1. Sample 30 cases to check feasibility ---------------------------------

# The first sample is in order to check that it's reasonable to add metadata

# load "dcc_charite_joined_7_ds_years_info"
load(here("data", "wrangling_steps", "dcc_charite", "dcc_charite_joined_7_ds_years_info.RData"))

# load "charite_dois_and_ids_clean_manually_validated"
load(here("data", "processed", "charite", "charite_dois_and_ids_clean_manually_validated.RData"))

sample_30_ids_no_citation <- charite_dois_and_ids_clean_manually_validated  |>
  select(doi_auto_current, charite_data_id_or_acc_nr_merged) |> 
  dplyr::filter(!charite_data_id_or_acc_nr_merged %in% dcc_charite_joined_7_ds_years_info$charite_data_id_or_acc_nr) |> 
  sample_n(30)

# replace one non-valid dataset manually (since I sampled from everything, not just from the valid ones):
sample_30_ids_no_citation[
  sample_30_ids_no_citation$doi_auto_current == "10.1016/j.immuni.2021.09.002" &
    sample_30_ids_no_citation$charite_data_id_or_acc_nr_merged == "datasets",
  c("doi_auto_current", "charite_data_id_or_acc_nr_merged")
] <- list("10.1093/gigascience/giad024", "10.5281/zenodo.1197578")

# save
write_csv_cr(sample_30_ids_no_citation,
             file = file.path(here(
               "data",
               "verification",
               "verification of sample",
               "sample_30_ids_no_citation.csv")),
             row.names = F)

# 2. Sample 170 more cases to reach 200 cases -----------------------------

# Since it took a short amount of time to add metadata to the first 30 cases
# we'll add 170 more cases

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

datasets_master_metadata_latest <- load_latest_metadata_update() # call function to load latest version


sample_170_ids_no_citation <- datasets_master_metadata_latest |> 
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


# 3. Sample 3 more cases --------------------------------------------------

# During the metadata extraction, it was discovered that 3 datasets weren't available anymore.
# That's why we'll sample 3 more cases to replace them.

load(here("data", "verification", "metadata all", "datasets_metadata_master_updated", "rda", "datasets_metadata_master_updated_003.RData"))

load(here("data", "verification", "metadata all", "sample_170_ids_no_citation.RData"))


sample_3_ids_no_citation <- datasets_metadata_master_updated_003 |> 
  dplyr::filter(in_dcc == "FALSE") |> 
  dplyr::filter(!data_id_merged %in% sample_170_ids_no_citation$data_id_merged) |> 
  sample_n(3)

# save
write_csv_cr(sample_3_ids_no_citation,
             file = file.path(here(
               "data",
               "verification",
               "verification of sample",
               "sample_3_ids_no_citation.csv")),
             row.names = F)


# 4. Sample 12 more cases -------------------------------------------------

# We only now realized that 12 cases were actually detected in DCC
# but with author overlap. So we'll sample 12 cases (not detected at all) to replace them.

# load 170

load(here("data",
          "verification",
          "verification of sample",
          "sample_170_ids_no_citation.RData"))

# load 30
sample_30 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_30_ids_no_citation.csv")),
  header = TRUE)

# load 3
sample_3 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_3_ids_no_citation.csv")),
  header = TRUE)

# get only author-ovelapped from _joined (to know what to filter by later)

only_au_ov <- dcc_charite_joined_4_au_info |>
  dplyr::filter(
    !doi_charite %in% dcc_charite_joined_5_rm_au_ov$doi_charite)

# Here's where I check if there are cases in one of the samples, and there are:

au_ov_in_sample <- only_au_ov |> 
  dplyr::filter(
    doi_charite %in% sample_170_ids_no_citation$doi
    | doi_charite %in% sample_30$doi_auto_current
    | doi_charite %in% sample_3$doi
  ) |>
  select(doi_charite) |> 
  distinct()

# save for Blanka the DOI list to replace
write_csv_cr(au_ov_in_sample,
             file = file.path(here(
               "data",
               "verification",
               "verification of sample",
               "au_ov_in_sample.csv")),
             row.names = F)

# get 12 additional cases to replace the DOIs above

sample_12_replacment_cases <- datasets_metadata_master_updated_003 |> 
  dplyr::filter(in_dcc == "FALSE") |> # not in DCC (but still includes author overlap!)
  dplyr::filter(!data_id_merged %in% sample_30$charite_data_id_or_acc_nr_merged) |> # not in the 30 sample
  dplyr::filter(!data_id_merged %in% sample_170_ids_no_citation$data_id_merged) |> # not in the 170 sample
  dplyr::filter(!data_id_merged %in% sample_3$data_id_merged) |> # not in the 3 sample
  dplyr::filter(!doi %in% only_au_ov$doi_charite) |> # not in author overlap
  sample_n(12) |> # sample 12
  select(
    unique_id,
    doi,
    doi_no_ver_info,
    data_id,
    data_id_merged,
    data_access,
    in_dashboard,
    source,
    listed_in_numbat_output,
    validated,
    Category,
    dataset_is_doi,
    repository,
    charite_id_year,
    in_dcc,
    human_data,
    covid_related,
    license
  ) # reorder columns to match more for Blanka's columns order

# # sanity check:
# 
# sample_12_replacment_cases |> 
#  dplyr::filter(
#     doi %in% only_au_ov$doi_charite)


# manually fix a zenodo entry (it was later fixed in the whole master metadata)

sample_12_replacment_cases <- sample_12_replacment_cases |> 
  mutate(data_id_merged = str_replace(
    data_id_merged,
    fixed("10.5281/zenodo.7895994"),
    "10.5281/zenodo.7889352"
  ))

# save
write_csv_cr(sample_12_replacment_cases,
             file = file.path(here(
               "data",
               "verification",
               "verification of sample",
               "sample_12_replacment_cases.csv")),
             row.names = F)


# 5. Sample 1 more case ---------------------------------------------------

# 1 more case was discovered as invalid (already in the numbat extraction step)
# so we'll sample 1 case more.

# load 170

load(here("data",
          "verification",
          "verification of sample",
          "sample_170_ids_no_citation.RData"))

# load 30
sample_30 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_30_ids_no_citation.csv")),
  header = TRUE)

# load 3
sample_3 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_3_ids_no_citation.csv")),
  header = TRUE)

# load 12
sample_12 <- read.csv(
  file.path(here("data",
                 "verification",
                 "verification of sample",
                 "sample_12_replacment_cases.csv")),
  header = TRUE)

# get 12 additional cases to replace the DOIs above

sample_1_replacement_case <- datasets_metadata_master_updated_005 |> 
  dplyr::filter(in_dcc == "FALSE") |> # not in DCC (but still includes author overlap!)
  dplyr::filter(!data_id_merged %in% sample_30$charite_data_id_or_acc_nr_merged) |> # not in the 30 sample
  dplyr::filter(!data_id_merged %in% sample_170_ids_no_citation$data_id_merged) |> # not in the 170 sample
  dplyr::filter(!data_id_merged %in% sample_3$data_id_merged) |> # not in the 3 sample
  dplyr::filter(!data_id_merged %in% sample_12$data_id_merged) |> # not in the 12 sample
  dplyr::filter(!doi %in% only_au_ov$doi_charite) |> # not in author overlap
  sample_n(1) |> # sample 1
  select(
    unique_id,
    doi,
    doi_no_ver_info,
    data_id,
    data_id_merged,
    data_access,
    in_dashboard,
    source,
    listed_in_numbat_output,
    validated,
    Category,
    dataset_is_doi,
    repository,
    charite_id_year,
    in_dcc,
    human_data,
    covid_related,
    license
  ) # reorder columns to match more for Blanka's columns order

# # sanity check:

sample_1_replacement_case |>
  dplyr::filter(
    doi %in% only_au_ov$doi_charite) |> View()

# save
write_csv_cr(sample_1_replacement_case,
             file = file.path(here(
               "data",
               "verification",
               "verification of sample",
               "sample_1_replacement_case.csv")),
             row.names = F)


# sample 1 more dataset ---------------------------------------------------

# 10.5281/zenodo.1197578 was accidentally twice in the list!

load_latest_metadata_update() # call function to load latest version

sample_1_more_case <- datasets_metadata_master_updated_008 |> 
  dplyr::filter(is.na(human_data)
                & is.na(covid_related)
                & is.na(data_availability_statement)
                & is.na(license)) |>
  sample_n(1) |> # sample 1
  select(doi_charite, dataset_for_matching, data_availability_statement, human_data, covid_related, license)
  
# save
write_csv_cr(sample_1_more_case,
             file = file.path(here(
               "data",
               "verification",
               "verification of sample",
               "sample_1_more_case.csv")),
             row.names = F)


# TBD: replace "non-matched" if needed ------------------------------------

# Check if after matching Added and DataStet, some "non-matched" so far
# where actually detected and are now needed to be replaced.



