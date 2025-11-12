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


# 4. Restrcutre _6_m_val_done ---------------------------------------------

# 22.08.2025:
#
# I ran ds_primary qmd again in order to:
#
# 1. leave multiple charite dois in the table for later joins
# 2. pair dois and ids into one column for later joins
# 3. consider https://physionet.org/content/ptb-xl/1.0.1/ as multiple ids for the same dataset
# 4. rename variables so that they will reflect their content more accurately (e.g. numbat_da instead of charite) 
#
# That's why here I'll restructure m_val according to the newest auto_cleaned output:
#
# numbat_da_dois_and_ids_3_auto_cleaned.RData
#
# Notice: the former m_val starts with charite. the new structure starts with numbat_da.
# So you can archive all "charite_..." files.

# load new structure auto cleaned
load(here("data","wrangling_steps","charite","numbat_da_dois_and_ids_3_auto_cleaned.RData"))

# load latest manual validation in the old structure
load(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_4_m_val_done.RData"))

# colnames
colnames(numbat_da_dois_and_ids_3_auto_cleaned)
colnames(charite_dois_and_ids_4_m_val_done)

# check for upper cases
# doi
any(grepl("[A-Z]", numbat_da_dois_and_ids_3_auto_cleaned$doi)) # new has, as expected
any(grepl("[A-Z]", charite_dois_and_ids_4_m_val_done$doi)) # original doesn't have
# id (lc)
any(grepl("[A-Z]", numbat_da_dois_and_ids_3_auto_cleaned$data_id_lc)) # new doesn't have, as expected
any(grepl("[A-Z]", charite_dois_and_ids_4_m_val_done$data_id)) # original also doesn't, as expected

# So I'll pair (and then join) by doi_lc and 

# add paired auto-cleaned for latest manual validation

charite_dois_and_ids_4_m_val_done_for_joining_1 <- charite_dois_and_ids_4_m_val_done|>
  mutate(doi_id_lc_pair = paste(doi, data_id, sep = ";")) |> # pair lower case doi-id
  distinct()

numbat_da_dois_and_ids_3_auto_cleaned_for_joining <- numbat_da_dois_and_ids_3_auto_cleaned|>
  mutate(doi_id_lc_pair = paste(tolower(doi), data_id_lc, sep = ";")) |> # pair lower case doi-id
  distinct()


# prepare latest manual validation for joining

colnames(charite_dois_and_ids_4_m_val_done_for_joining_1) # check which columns to keep
colnames(numbat_da_dois_and_ids_3_auto_cleaned_for_joining) # check which columns to keep


any(grepl("[A-Z]", charite_dois_and_ids_4_m_val_done_for_joining_1$doi_id_lc_pair))
any(grepl("[A-Z]", numbat_da_dois_and_ids_3_auto_cleaned_for_joining$doi_id_lc_pair))


charite_dois_and_ids_4_m_val_done_for_joining_2 <- charite_dois_and_ids_4_m_val_done_for_joining_1 |>
  select(
    doi_id_lc_pair,
    data_id_m_val,
    validated,
    Category,
    is_gen_rep)
  
# join to new auto-cleaned

numbat_da_dois_and_ids_4_m_val_joined <- numbat_da_dois_and_ids_3_auto_cleaned_for_joining |>
  left_join(charite_dois_and_ids_4_m_val_done_for_joining_2,
            by = "doi_id_lc_pair") |>
  relocate(data_id_m_val, .after = slug) |>
  relocate(doi_id_orig_pair, .after = is_gen_rep) |>
  relocate(doi_id_lc_pair, .after = doi_id_orig_pair)
  
# check if there's anything else to add
numbat_da_dois_and_ids_4_m_val_joined |> dplyr::filter(
  is.na(validated) | is.na(Category) | is.na(is_gen_rep)) |> View()

# Yes there is.

# So I'll save numbat_da_dois_and_ids_4_m_val_joined as "in progress"
# and then a csv for documentation and one to actually fill.

# save
save_cr(numbat_da_dois_and_ids_4_m_val_joined,
        file = file.path(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_4_m_val_joined_in_progress.RData")))



# Write a csv for documentation

write_csv_cr(
  numbat_da_dois_and_ids_4_m_val_joined,
  file = here("data",
              "verification",
              "m_val",
              "numbat_da_dois_and_ids_4_m_val_joined_in_progress.csv"),
  row.names = FALSE
)

# Write another copy of it - this is the file you should work on!

write_csv_cr(
  numbat_da_dois_and_ids_4_m_val_joined,
  file = here("data",
              "verification",
              "m_val",
              "numbat_da_dois_and_ids_5_m_val_joined_done.csv"),
  row.names = FALSE
)

# and that's it.
# From here, return to "charite_loading_and_preprocessing.qmd" to get the relevant cases from
# "numbat_da_dois_and_ids_5_m_val_joined_done.csv"
# it's saved there as an RData object as well ("numbat_da_dois_and_ids_5_m_val_joined_done.RData")

# I changed the column name manutlly from doi_id_lc_pair_for_joining to doi_id_lc_pair
# and then here I'll just tolower the doi_id_lc_pair values, which were not lowered in
# the original script above. I fixed it in the script and made sure that the cases
# before and after the fix are identical. All that changed is the column name and the
# values' tolower.

temp_4 <- read.csv(
  file.path(here("data", "verification", "m_val", "numbat_da_dois_and_ids_4_m_val_joined_in_progress.csv")),
  header = TRUE) |> 
  mutate(doi_id_lc_pair = tolower(doi_id_lc_pair))

temp_5 <- read.csv(
  file.path(here("data", "verification", "m_val", "numbat_da_dois_and_ids_5_m_val_joined_done.csv")),
  header = TRUE) |> 
  mutate(doi_id_lc_pair = tolower(doi_id_lc_pair))

# check that aside from this column temp_5 is identical to _5_m_val_done:

check <- numbat_da_dois_and_ids_4_m_val_joined |> select(doi_id_orig_pair)
check_4 <- temp_4 |> select(doi_id_orig_pair)
check_5 <- temp_5 |> select(doi_id_orig_pair)

identical(check, numbat_da_dois_and_ids_4_m_val_joined |> select(doi_id_orig_pair))
identical(check_4, temp_4 |> select(doi_id_orig_pair))
identical(check_5, temp_5 |> select(doi_id_orig_pair))

check_44 <- check_4 |> select(doi_id_orig_pair) |> arrange(doi_id_orig_pair)
check_55 <- check_5 |> select(doi_id_orig_pair) |> arrange(doi_id_orig_pair)

t_44 <- numbat_da_dois_and_ids_4_m_val_joined |> select(doi_id_orig_pair) |> arrange(doi_id_orig_pair)

identical(check_44$doi_id_orig_pair, t_44$doi_id_orig_pair)
identical(check_55$doi_id_orig_pair, t_44$doi_id_orig_pair)

anti_join(check_44, t_44 |> select(doi_id_orig_pair)) |> View()
anti_join(check_55, t_44 |> select(doi_id_orig_pair)) |> View()

# reweite the csvs with the correct col names and lowercase values:

# Write a csv for documentation

numbat_da_dois_and_ids_4_m_val_joined_in_progress <- temp_4

write_csv_cr(
  numbat_da_dois_and_ids_4_m_val_joined_in_progress,
  file = here("data",
              "verification",
              "m_val",
              "numbat_da_dois_and_ids_4_m_val_joined_in_progress.csv"),
  row.names = FALSE
)

# Write another copy of it - this is the file you should work on!

numbat_da_dois_and_ids_5_m_val_joined_done <- temp_5

write_csv_cr(
  temp_5,
  file = here("data",
              "verification",
              "m_val",
              "numbat_da_dois_and_ids_5_m_val_joined_done.csv"),
  row.names = FALSE
)

# Last verification:

load(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_5_m_val_joined_done.RData"))

new_5_done <- read.csv(
  file.path(here("data", "verification", "m_val", "numbat_da_dois_and_ids_5_m_val_joined_done.csv")),
  header = TRUE,
  sep = ",")

any(grepl("[A-Z]", new_5_done$doi_id_lc_pair)) # original also doesn't, as expected
any(grepl("[A-Z]", old_5_done$doi_id_lc_pair_for_joining)) # original also doesn't, as expected


old_5_done <- numbat_da_dois_and_ids_5_m_val_joined_done |> 
  rename(doi_id_lc_pair = doi_id_lc_pair_for_joining) |> 
  mutate(doi_id_lc_pair = tolower(doi_id_lc_pair))

identical(new_5_done, old_5_done)

# Identical. No problems.

# 5. Archive --------------------------------------------------------------

# ARCHIVE of updating manual validation:
#
# # Check which and if there are any identifiers to manual validate
# 
# numbat_da_dois_and_ids_6_m_val_done <- read.csv(
#   file.path(here("data", "verification", "m_val", "charite_dois_and_ids_6_m_val_done.csv")),
#   header = TRUE,
#   sep = ",") # load previous m_val
# 
# with_new_ids_to_val_v1_to_edit <- numbat_da_dois_and_ids_3_auto_cleaned |> 
#   left_join(numbat_da_dois_and_ids_6_m_val_done |> select(doi, data_id, data_id_m_val, validated, Category),
#   by = c("doi", "data_id")
# ) |> # join existing prior validation
#   relocate(data_id_m_val, .after = slug)
# 
# with_new_ids_to_val_v1_to_edit |> dplyr::filter(is.na(validated) | is.na(Category)) |> View()
# 
# # There are several, so I will export to a csv to fill them:
# write_csv_cr(
#   with_new_ids_to_val_v1_to_edit,
#   file = here("data",
#               "verification",
#               "m_val",
#               "with_new_ids_to_val_v1_to_edit.csv"),
#   row.names = FALSE
# ) # as csv
