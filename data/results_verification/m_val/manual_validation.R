# This script takes the RData object "2_charite_dois_and_ids_clean.RData" (loaded as "charite_dois_and_ids_clean")
# with automatically extracted accession numbers, and:
# 1. Validates extraction of accession numbers with common prefixes
# 2. Validates and Standardizes identifiers of general repositories (zenodo, osf, figshare, dryad)
# 3. Creates a csv for manual validation of the rest of the accession numbers


# 1. Load latest manual validation ----------------------------------------

# Since there are always cases added, we will first get only the cases that were not manually validated until now.

# Locate manual validation folder
files <- list.files(here("data",
                         "results_verification",
                         "m_val"),
  pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)


# Identify the latest manual validation file based on modification time
latest_file_name <- files[which.max(file.info(files)$mtime)]

# Load the latest manual validation file into the global environment
latest_validation <- read_csv(latest_file_name) # this is the latest "manual_validation_done"

# Verify that identifiers in this file and in charite_dois_and_ids_clean are lower cased
any(str_detect(charite_dois_and_ids_clean$data_identifier, "[A-Z]"))
any(str_detect(latest_validation$identifier, "[A-Z]"))

# "tolower" data_identifier
charite_dois_and_ids_clean <- charite_dois_and_ids_clean |>
  mutate(data_identifier = tolower(data_identifier))

# Add auto_cleaned cases to previously manually validated list
manual_validation_22042025 <- charite_dois_and_ids_clean |> 
  left_join(
    latest_validation |> 
      distinct(identifier, .keep_all = TRUE),
    by = c("data_identifier" = "identifier"),
    suffix = c("_auto_current", "_auto_previous")
    )

# 2. Common accession numbers prefixed ---------------------------------------

# In this step, I manually created a list of common accession numbers prefixes
# in order to label accession numbers in the Charite list that the were
# verified as correctly extracted automatically (TRUE) and those which weren't yet verified as such (FALSE): ###

# list of prefixes (created manually)
prefixes <- c("//github.com",
              "//osf.io",
              "gse",
              "gsm",
              "e-mtab-",
              "egas",
              "egad",
              "e-geod",
              "mk",
              "mh",
              "phs",
              "mn",
              "mw",
              "pxd",
              "srr",
              "prjeb",
              "emd-",
              "gcst",
              "pdb_",
              "nm_",
              "nct",
              "err",
              "gds",
              "msv",
              "mz",
              "nc_",
              "np_",
              "prjna",
              "prjca",
              "srp",
              "phs",
              "pgs",
              "s-bsst",
              "mt",
              "kt",
              "st",
              "ol",
              "op",
              "or",
              "oq",
              "scp",
              "s-biad",
              "e-tabm",
              "srx",
              "empiar",
              "fr-fcm-z") 

# Mark cases with the prefixes above as validated (validated = TRUE)

manual_validation_22042025_in_progress <- manual_validation_22042025 |> 
  mutate(validated = case_when(
    is.na(validated)
    & map_lgl(charite_data_id_or_acc_nr_auto_current, ~ any(str_starts(.x, prefixes))) # these prefixes existence mean that the function handled these cases well
    ~ TRUE,
    .default = validated))

# 3. Final clean up before manual validation ------------------------------

# 1. Remove version information from id (".v...")

manual_validation_22042025_in_progress <- manual_validation_22042025_in_progress |>
  mutate(charite_data_id_or_acc_nr_auto_current = case_when(
    is.na(validated) ~ sub("\\.v[0-9]+.*$", "", charite_data_id_or_acc_nr_auto_current),
    .default = charite_data_id_or_acc_nr_auto_current)) # remove version information

# 2. Replace "," with ".", and remove:

  # "." at the beginning / end
  # " " anywhere
  # "," at the beginning / end

# View
manual_validation_22042025_in_progress |>
  dplyr::filter(
    is.na(validated) &
    (str_detect(charite_data_id_or_acc_nr_auto_current, "^,|,$") | # ","
      str_detect(charite_data_id_or_acc_nr_auto_current, "^\\.|\\.$") | # "."
      str_detect(charite_data_id_or_acc_nr_auto_current, "\\s") | # " " anywhere
      str_detect(charite_data_id_or_acc_nr_auto_current, "^\\s|\\s$")) # " " at the beginning / end
  ) |> View()

# Remove
manual_validation_22042025_in_progress <- manual_validation_22042025_in_progress |> 
  mutate(charite_data_id_or_acc_nr_auto_current = case_when(
    is.na(validated)
    & str_detect(charite_data_id_or_acc_nr_auto_current, "^,|,$")
    ~ str_remove_all(charite_data_id_or_acc_nr_auto_current, "^,|,$"), # ","
    
    is.na(validated)
    & str_detect(charite_data_id_or_acc_nr_auto_current, "^\\.|\\.$")
    ~ str_remove_all(charite_data_id_or_acc_nr_auto_current, "^\\.|\\.$"), # "."
    
    is.na(validated)
    & str_detect(charite_data_id_or_acc_nr_auto_current, "^\\s|\\s$")
    ~ str_trim(charite_data_id_or_acc_nr_auto_current), # " " anywhere
    
    is.na(validated)
    & str_detect(charite_data_id_or_acc_nr_auto_current, "\\s")
    ~ str_replace_all(charite_data_id_or_acc_nr_auto_current, "\\s", ""), # " " at the beginning / end
    
    # Keep all other cases unchanged
    .default = charite_data_id_or_acc_nr_auto_current
  ))

# View cases left unhandled

manual_validation_22042025_in_progress |> dplyr::filter(is.na(validated)) |> View()

# Write a copy of it with the suffix _in_progress - this is the file you should work on!

write_csv_cr(
  manual_validation_22042025_in_progress,
  file = here("data",
              "results_verification",
              "m_val",
              "manual_validation_22042025_in_progress_for_work.csv"),
  row.names = FALSE
)

# 4. Finalizing the validation --------------------------------------------

# After finishing working on the _in_progress.csv, here we'll make sure that the best identifier
# (out of the columns: "data_identifier"
#                      "charite_data_id_or_acc_nr_auto_current",
#                      "charite_data_id_or_acc_nr_v"
#                      "charite_data_id_or_acc_nr_merged")
# is listed under "_merged"!

# Load _in_progress that you've just finished working on:

new_files <- list.files(here("data",
                         "results_verification",
                         "m_val"),
                    pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE) # get updated files list

new_latest_file_name <- new_files[which.max(file.info(new_files)$mtime)] # get updated last modified file

manual_validation_in_progress_all_validated <- read_csv(new_latest_file_name) # load _in_progress

manual_validation_22042025_done <- manual_validation_in_progress_all_validated |> 
  mutate(charite_data_id_or_acc_nr_merged =
           if_else(
             # if "merged" is NA
             is.na(charite_data_id_or_acc_nr_merged),
             # then get _v
             coalesce(charite_data_id_or_acc_nr_v,
                      # if _v is also NA, get the automatically extracted ("charite_data_id_or_acc_nr_auto_current")
                      charite_data_id_or_acc_nr_auto_current),
             # and put either of them in _merged
             charite_data_id_or_acc_nr_merged))

# Write a csv for the finished manual validation: This file will be loaded in charite_loading_and_preprocessing.qmd!

write_csv_cr(
  manual_validation_22042025_done,
  file = here("data",
              "results_verification",
              "m_val",
              "manual_validation_22042025_done.csv"),
  row.names = FALSE
)
