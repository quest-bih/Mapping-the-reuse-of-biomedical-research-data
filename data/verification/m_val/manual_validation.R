# This script takes the RData object "charite_dois_and_ids_3_auto_cleaned.RData".
# This file has automatically extracted accession numbers. So this script:
# 1. Validates extraction of accession numbers with common prefixes
# 2. Validates and Standardizes identifiers of general repositories (zenodo, osf, figshare, dryad)
# 3. Creates a csv for manual validation of the rest of the accession numbers

# 1. Load charite_dois_and_ids_3_auto_cleaned.RData -----------------------

load(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_3_auto_cleaned.RData"))

# 2. Remove excess characters ---------------------------------------------

# # Verify that DOIs and IDs columns are all in lower case
# 
# charite_dois_and_ids_3_auto_cleaned |>
#   dplyr::filter(
#     dplyr::if_any(c(doi, doi_no_ver_info, data_id, data_id_auto_cleaned), ~ stringr::str_detect(.x, "[A-Z]"))
#   ) |> View()

# Replace "," with ".", and remove:

# "." at the beginning / end
# " " anywhere
# "," at the beginning / end

# View
charite_dois_and_ids_3_auto_cleaned |>
  dplyr::filter(
    str_detect(data_id_auto_cleaned, "^,|,$") | # ","
      str_detect(data_id_auto_cleaned, "^\\.|\\.$") | # "."
      str_detect(data_id_auto_cleaned, "\\s") | # " " anywhere
      str_detect(data_id_auto_cleaned, "^\\s|\\s$") # " " at the beginning / end
  ) |> View()

# Remove (mutate "data_id_auto_cleaned" into "data_id_no_ex_chr")
charite_dois_and_ids_4_m_val_in_prog_ex <- charite_dois_and_ids_3_auto_cleaned |> 
  mutate(data_id_no_ex_chr = case_when(
    str_detect(data_id_auto_cleaned, "^,|,$")
    ~ str_remove_all(data_id_auto_cleaned, "^,|,$"), # ","
    
    str_detect(data_id_auto_cleaned, "^\\.|\\.$")
    ~ str_remove_all(data_id_auto_cleaned, "^\\.|\\.$"), # "."
    
    str_detect(data_id_auto_cleaned, "^\\s|\\s$")
    ~ str_trim(data_id_auto_cleaned), # " " anywhere
    
    str_detect(data_id_auto_cleaned, "\\s")
    ~ str_replace_all(data_id_auto_cleaned, "\\s", ""), # " " at the beginning / end
    
    # Keep all other cases unchanged
    .default = data_id_auto_cleaned
  )) |> 
  relocate(data_id_no_ex_chr, .after = data_id_auto_cleaned)

# save
save_cr(charite_dois_and_ids_4_m_val_in_prog_ex,
        file = file.path(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_4_m_val_in_prog_ex.RData")))

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

charite_dois_and_ids_5_m_val_in_prog_pref_lbl <- charite_dois_and_ids_4_m_val_in_prog_ex |> 
  mutate(validated = FALSE) |> # create a column to label the validation
  mutate(validated = case_when(
    map_lgl(data_id_no_ex_chr, ~ any(str_starts(.x, prefixes))) # cases with these prefixes will be considered as validated
    ~ TRUE,
    .default = validated))

# Note: there will be another manual overview on these "validated" cases!

# save
save_cr(charite_dois_and_ids_5_m_val_in_prog_pref_lbl,
        file = file.path(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_5_m_val_in_prog_pref_lbl.RData")))

# Write a csv for documentation

write_csv_cr(
  charite_dois_and_ids_5_m_val_in_prog_pref_lbl,
  file = here("data",
              "verification",
              "m_val",
              "charite_dois_and_ids_5_m_val_in_prog_pref_lbl.csv"),
  row.names = FALSE
)

# Write another copy of it - this is the file you should work on!

write_csv_cr(
  charite_dois_and_ids_5_m_val_in_prog_pref_lbl,
  file = here("data",
              "verification",
              "m_val",
              "charite_dois_and_ids_6_m_val_done.csv"),
  row.names = FALSE
)

# And that's it.
# From here, return to "charite_loading_and_preprocessing.qmd" to get the relevant cases from
# "charite_dois_and_ids_6_m_val_done"