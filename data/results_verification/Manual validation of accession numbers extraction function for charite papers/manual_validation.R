# This script takes the RData object "charite_dois_and_ids_clean" with automatically extracted accession numbers, and:
# 1. Validates extraction of accession numbers with common prefixes
# 2. Validates and Standardizes identifiers of general repositories (zenodo, osf, figshare, dryad)
# 3. Creates a csv for manual validation of the rest of the accession numbers

# 1. Set up ---------------------------------------------------------------

rm(list=ls())

Sys.setenv(LANG = "EN") # make sure system environment is in English

if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               DT,
               patchwork,
               RColorBrewer,
               rstudioapi,
               here,
               tcltk) # load libraries

# Wrapper for save() with automatic directory creation
save_cr <- function(..., file) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  save(..., file = file)
}

# Wrapper for write.csv() with automatic directory creation
write_csv_cr <- function(x, file, ...) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  write.csv(x, file = file, ...)
}


# 2. Load file ------------------------------------------------------------

# Load Charite's doi and ids list (after automatic accession number extraction)

root_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # get current directory
relative_path <- normalizePath(file.path(root_dir, "../..", "Data Wrangling", "Charite wrangling steps", "2_charite_dois_and_ids_clean.RData"), 
                               winslash = "/", mustWork = FALSE) # get location of file to load
load(relative_path) # load file

# Get only cases that were not verified until now
# (there was a previous manual validation that was being done on a subset of the data)

# Locate dir
files <- list.files(root_dir,
                    pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

# Identify the latest file based on modification time
latest_file_name <- files[which.max(file.info(files)$mtime)]

# Load the file into the global environment
latest_file_to_load <- read_csv(latest_file_name) # this is the latest "manual_validation_done"

charite_dois_and_ids_clean$identifier <- tolower(charite_dois_and_ids_clean$identifier)

manual_validation_20022025 <- charite_dois_and_ids_clean |> 
  filter(!identifier %in% tolower(latest_file_to_load$identifier)) # get only added cases (after automatic cleaning)

# bind them to existing manual validation latest file
manual_validation_20022025 <- latest_file_to_load |> 
  bind_rows(manual_validation_20022025) # now cases where "validated = NA" are the added ones

# get how many cases were added (validated = NA)
manual_validation_20022025 |> group_by(validated) |> summarise(n = n())

# 3. Common accession numbers prefixed ---------------------------------------

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

manual_validation_20022025 <- manual_validation_20022025 |> 
  mutate(validated = case_when(
    map_lgl(charite_data_id_or_acc_nr, ~ any(str_starts(.x, prefixes))) # these prefixes existence mean that the function handled these cases well
    ~ TRUE,
    .default = validated))

# get how many casesare left after labeling cases with prefixes (validated = NA)
manual_validation_20022025 |> group_by(validated) |> summarise(n = n())

# # 4. Standardizing general repositories identifiers -----------------------
# 
# # zenodo (std to: 10.5281/zenodo.xxxxx)
# 
# # manual_validation_03122024 |> filter(str_detect(charite_data_id_or_acc_nr, "zenodo")) |> View()
# 
# # Detect "zenodo" that isn't standardized and append ""10.5281/" to it (as a prefix)
# manual_validation_03122024 <- manual_validation_03122024 |> 
#   mutate(charite_data_id_or_acc_nr = case_when(
#     str_detect(charite_data_id_or_acc_nr, "zenodo") & 
#       !str_detect(charite_data_id_or_acc_nr, "^10\\.5281/zenodo\\.\\d+$") ~ 
#       paste0("10.5281/", charite_data_id_or_acc_nr), 
#     .default = charite_data_id_or_acc_nr
#   ))
# 
# # osf (std to: //osf.io/xxxxx)
# 
# # manual_validation_03122024 |> filter(str_detect(charite_data_id_or_acc_nr, "osf")) |> View()
# 
# # Only one case to standardize (remove "/files" trail)
# manual_validation_03122024 <- manual_validation_03122024 |> 
#   mutate(charite_data_id_or_acc_nr = str_remove(charite_data_id_or_acc_nr, "/files$"))
# 
# # figshare (std to: 10.6084/m9.figshare.xxxxx)
# 
# # manual_validation_03122024 |> filter(str_detect(charite_data_id_or_acc_nr, "figshare")) |> View()
# 
# # Only one case to standardize (add "10.6084/" before value)
# manual_validation_03122024 <- manual_validation_03122024 |> 
#   mutate(charite_data_id_or_acc_nr = case_when(
#     str_detect(charite_data_id_or_acc_nr, "figshare") & 
#       !str_starts(charite_data_id_or_acc_nr, "10.6084") ~ 
#       paste0("10.6084/", charite_data_id_or_acc_nr),
#     .default = charite_data_id_or_acc_nr
#   ))
# 
# # dryad
# 
# # manual_validation_03122024 |> filter(str_detect(charite_data_id_or_acc_nr, "dryad")) |> View() # already std'd
# 
# # Mark all general repositories as validated:
# 
# manual_validation_03122024 <- manual_validation_03122024 |> 
#   mutate(validated = case_when(
#     str_detect(charite_data_id_or_acc_nr, "zenodo")
#     | str_detect(charite_data_id_or_acc_nr, "osf")
#     | str_detect(charite_data_id_or_acc_nr, "figshare")
#     | str_detect(charite_data_id_or_acc_nr, "dryad")
#     ~ T,
#     .default = validated
#   ))

# 5. Final clean up before manual validation ------------------------------

# 1. Remove version information from id (".v...")
manual_validation_20022025$charite_data_id_or_acc_nr <- 
  sub("\\.v[0-9]+.*$", "", manual_validation_20022025$charite_data_id_or_acc_nr)

# 2. Replace "," with ".", and remove:

  # "." at the beginning / end
  # " " anywhere
  # "," at the beginning / end

# View
manual_validation_20022025 |>
  filter(
    str_detect(charite_data_id_or_acc_nr, "^,|,$") | # ","
      str_detect(charite_data_id_or_acc_nr, "^\\.|\\.$") | # "."
      str_detect(charite_data_id_or_acc_nr, "\\s") | # " " anywhere
      str_detect(charite_data_id_or_acc_nr, "^\\s|\\s$") # " " at the beginning / end
  ) |> View()

# Remove
manual_validation_20022025 <- manual_validation_20022025 |> 
  mutate(charite_data_id_or_acc_nr = case_when(
    str_detect(charite_data_id_or_acc_nr, "^,|,$") ~ 
      str_remove_all(charite_data_id_or_acc_nr, "^,|,$"), # ","
    
    str_detect(charite_data_id_or_acc_nr, "^\\.|\\.$") ~ 
      str_remove_all(charite_data_id_or_acc_nr, "^\\.|\\.$"), # "."
    
    str_detect(charite_data_id_or_acc_nr, "^\\s|\\s$") ~ 
      str_trim(charite_data_id_or_acc_nr), # " " anywhere
    
    str_detect(charite_data_id_or_acc_nr, "\\s") ~ 
      str_replace_all(charite_data_id_or_acc_nr, "\\s", ""), # " " at the beginning / end
    
    # Keep all other cases unchanged
    .default = charite_data_id_or_acc_nr
  ))

# # 3. Mark cases starting with "10." as validated
# 
# # I Manually looked over the remaining cases that start with "10.".
# # They looked valid, so I will change them to "validated = T"
# 
# manual_validation_03122024 <- manual_validation_03122024 |> 
#   mutate(validated = case_when(
#     str_detect(charite_data_id_or_acc_nr, "^10\\.") ~ TRUE,
#     .default = validated
#   ))

# Write a csv for manual validation of the rest of the identifiers

write_csv_cr(manual_validation_20022025,
             file = file.path(root_dir,
                              "manual_validation_20022025.csv"),
             row.names = FALSE)
          
# Next, save "manual_validation_20022025.csv" as "manual_validation_20022025_done.csv" ana manually validate it!

##### ARCHIVE:

# Notes for manual validation:
# https://www.ncbi.nlm.nih.gov/nuccore/2085340607 leads to acc_nr MW718881.1
# https://www.deciphergenomics.org/patient/427511/overview/general no acc_nr
# http://csg.sph.umich.edu/willer/public/glgc-lipids2021/ no acc_nr

# Not accessible (error):
# https://www.ncbi.nlm.nih.gov/traces/wgs/jaiezw01?display=contigs
# https://www.ebi.ac.uk/empiar/empiar-10822/
# https://massive.ucsd.edu/proteosafe/dataset.jsp?task=2ae5c0457a3f4e9196365f9448657b59
# https://massive.ucsd.edu/proteosafe/dataset.jsp?task=1a42c18056484609afafe07519049ad9
# https://ddbj.nig.ac.jp/public/ddbj_database/dra/fastq/dra010/dra010491/
# https://data.4dnucleome.org/files-processed/4dnfijrx16ek/#file-overview
# https://data.4dnucleome.org/files-fastq/4dnfi1f6zlao/
# https://flowrepository.org/id/fr-fcm-z3g7 

###
