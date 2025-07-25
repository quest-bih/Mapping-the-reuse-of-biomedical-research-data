# This script cleans and standardizes all numbat dataset value.
# The goal is to the detect if any of them are present in the "datastet+added" pool.
# If they are, and there's already metadata for them, I'll label them as "in_numbat",
# so that we won't have to extract their metadata again.

# The script uses chunks from "primary" qmd and "manual_validation" script.

# load raw numbat output
load(here("data", "raw", "charite", "master_od_screening_manual_check_2020_2023_v2.1.rda"))

# pull distinct dataset column

numbat_datasets_raw <- master_2020_2023_corr |> 
  select(data_identifier) |> 
  distinct()

# check for encoding issues

numbat_datasets_raw_1_unique <- numbat_datasets_raw |> 
  distinct() |> 
  mutate(temp_unique_id = row_number()) |> 
  relocate(temp_unique_id, .before = 1) # add unique id

View(numbat_datasets_raw_1_unique[!stringi::stri_enc_isutf8(numbat_datasets_raw_1_unique$data_identifier), ]) # view issues

# Both cases where there's a doi and id values are not relevant, and will be removed below

numbat_datasets_raw_2_rm_na <- numbat_datasets_raw_1_unique |>
  dplyr::filter(!temp_unique_id %in% c(1801, 1984)) |> # remove 2 irrelevant cases with encoding issues
  select(-temp_unique_id) |>  # remove temporary unique_id column
  mutate(data_id = tolower(data_identifier)) |> # tolower
  rename(data_identifier_orig = data_identifier) |> # keep original id col
  dplyr::filter(!is.na(data_id), data_id != "null") |> 
  dplyr::filter(!str_starts(data_id, "#")) |>
  dplyr::filter(!str_starts(data_id, "bc:")) 

# Function for accession numbers extraction (Written by Vladislav Nachev):

extract_acc_nr <- function(url_string, pattern) {
  url_string <- tolower(url_string) # standardizing for lower case
  acc_nr <- elements <- NULL
  
  if (str_detect(url_string, "osf|figshare")) {
    return(url_string)  # ✅ Skip processing if OSF or figshare found
  }
  
  if (stringr::str_detect(url_string, "doi\\:")) { # removing "doi" prefixes
    return(url_string |> 
             stringr::str_remove(".*\\: ?") |> 
             stringr::str_squish() |> 
             stringr::str_remove("\\.$"))
  } else if (stringr::str_detect(url_string, "doi(\\.org)?\\/")) {
    return(url_string |> 
             stringr::str_remove(".*doi(\\.org)?\\/") |>
             stringr::str_remove("(\\/|\\.)$")) 
  }  else if (stringr::str_detect(url_string, "git(hu|la)b")) { # handling git
    acc_nr <- tibble::tibble(elements = url_string)
  } else if (stringr::str_detect(url_string, pattern)) { # handling common patterns
    url_string <- stringr::str_remove(url_string, ".*\\?")
    acc_nr <- tibble::tibble(elements = unlist(strsplit(url_string, "&"))) |> 
      dplyr::filter(stringr::str_detect(elements, pattern)) 
  } else {
    acc_nr <- tibble::tibble(elements = unlist(strsplit(url_string, "\\/"))) |> 
      dplyr::filter(stringr::str_length(elements) > 1,
                    !stringr::str_detect(elements, "#|descriptors|files")) |> 
      dplyr::slice_tail(n = 1)
  }
  acc_nr <- acc_nr |> 
    dplyr::mutate(elements = stringr::str_remove(elements, "(\\?.*$)") |>
                    stringr::str_remove("(.*\\=)") |> 
                    stringr::str_remove("https?:?") |> 
                    stringr::str_remove("\\.v?\\d{1,3}$")) |> 
    dplyr::pull(elements)
  
  return(acc_nr)
}

pattern <- "(acc(?!ess)|accession|ccdcid|studyid|id|run|term|acc)\\=" # we build this pattern when looking for url keyvalue pairs

# Applying the function to extract accession numbers automatically

numbat_datasets_raw_3_auto_cleaned <- numbat_datasets_raw_2_rm_na |>
  mutate(data_id_auto_cleaned = map_chr(data_id, \(x) extract_acc_nr(x, pattern))) |> # apply function
  mutate(
    # Extract slug only where applicable:
    # - If 'data_id' contains an osf.io/[slug] or osf.io/preprints/.../[slug],
    #   extract the 5-character slug from the end.
    slug = case_when(
      str_detect(data_id, "osf\\.io/[A-Za-z0-9]+")
      ~ str_extract(data_id, "osf\\.io/(preprints/[^/]+/)?[A-Za-z0-9]{5}") |> 
        str_extract("[A-Za-z0-9]{5}$"),
      .default = NA
    ),
    
    # Apply formatting logic only when data_id has a valid osf.io slug:
    # - If slug and DOI (valid or malformed) are present, format as '10.xxxx/osf.io/slug'.
    # - If only slug is present, format as '//osf.io/slug'.
    # - All other rows remain unchanged.
    data_id_auto_cleaned = case_when(
      str_detect(data_id, "osf\\.io/[A-Za-z0-9]+")
      & !is.na(slug) & str_detect(data_id, "10\\.\\d+|10\\d{4,}")
      ~ paste0(
        str_extract(data_id, "10\\.\\d+|10\\d{4,}") |> 
          str_replace("^10(\\d{4,})$", "10.\\1"),
        "/osf.io/",
        slug
      ),
      str_detect(data_id, "osf\\.io/[A-Za-z0-9]+") & !is.na(slug) ~ paste0("//osf.io/", slug),
      .default = data_id_auto_cleaned
    ),
    
    # Construct a matching key prefixing the slug with 'osf_' when slug is available;
    # otherwise set to NA.
    dataset_for_matching = case_when(
      !is.na(slug) ~ paste0("osf_", slug),
      .default = NA)) |>  
  mutate(
    # Step 1: Fix malformed figshare entries without DOI
    data_id_auto_cleaned = case_when(
      str_detect(data_id, "figshare") &
        !str_detect(data_id, "10\\.\\d+") ~ data_id_auto_cleaned |>
        str_replace(".*(?=figshare\\.com)", "//"),          # Normalize to start at figshare.com
      .default = data_id_auto_cleaned
    )
  ) |>
  mutate(
    # Step 2: Extract valid DOI if present in figshare entry
    data_id_auto_cleaned = case_when(
      str_detect(data_id, "figshare") &
        str_detect(data_id, "10\\.\\d+") ~ str_extract(
          data_id,
          "10\\.\\d{4,9}/[^\\s/]+"                            # Extracts full DOI, avoids trailing slashes/spaces
        ),
      .default = data_id_auto_cleaned
    )
  ) |>
  relocate(data_id_auto_cleaned, slug, dataset_for_matching, .after = data_id) # relocate m_val

### Manual Validation:

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
numbat_datasets_raw_3_auto_cleaned |>
  dplyr::filter(
    str_detect(data_id_auto_cleaned, "^,|,$") | # ","
      str_detect(data_id_auto_cleaned, "^\\.|\\.$") | # "."
      str_detect(data_id_auto_cleaned, "\\s") | # " " anywhere
      str_detect(data_id_auto_cleaned, "^\\s|\\s$") # " " at the beginning / end
  ) |> View()

# Remove (mutate "data_id_auto_cleaned" into "data_id_no_ex_chr")
numbat_datasets_4_m_val_in_prog_ex <- numbat_datasets_raw_3_auto_cleaned |> 
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
save_cr(numbat_datasets_4_m_val_in_prog_ex,
        file = file.path(here("data", "wrangling_steps", "charite", "for_datastet_exc",
                              "numbat_datasets_4_m_val_in_prog_ex.RData")))

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

numbat_datasets_5_m_val_in_prog_pref_lbl <- numbat_datasets_4_m_val_in_prog_ex |> 
  mutate(validated = FALSE) |> # create a column to label the validation
  mutate(validated = case_when(
    map_lgl(data_id_no_ex_chr, ~ any(str_starts(.x, prefixes))) # cases with these prefixes will be considered as validated
    ~ TRUE,
    .default = validated))

# Note: there will be another manual overview on these "validated" cases!

# save
save_cr(numbat_datasets_5_m_val_in_prog_pref_lbl,
        file = file.path(here("data", "wrangling_steps", "charite",
                              "for_datastet_exc",
                              "numbat_datasets_5_m_val_in_prog_pref_lbl.RData")))

# Write a csv for documentation

write_csv_cr(
  numbat_datasets_5_m_val_in_prog_pref_lbl,
  file = here("data",
              "verification",
              "m_val",
              "for_datastet_exc",
              "numbat_datasets_5_m_val_in_prog_pref_lbl.csv"),
  row.names = FALSE
)

# Write another copy of it - this is the file you should work on!

write_csv_cr(
  numbat_datasets_5_m_val_in_prog_pref_lbl,
  file = here("data",
              "verification",
              "m_val",
              "for_datastet_exc",
              "numbat_datasets_6_m_val_done.csv"),
  row.names = FALSE
)

##### Here a manual validation took place. Then below the filled csv is loaded and further processed:

# Load "numbat_datasets_6_m_val_done.csv"

numbat_datasets_6_m_val_done <- read.csv(
  file.path(here("data", "verification", "m_val", "for_datastet_exc", "numbat_datasets_6_m_val_done.csv")),
  header = TRUE,
  sep = ",")

# save as RData
save_cr(numbat_datasets_6_m_val_done,
        file = file.path(here("data", "wrangling_steps", "charite",
                              "for_datastet_exc", "numbat_datasets_6_m_val_done.RData")))

# NOTE: I didn't validate every single row. Just enough to have some detected.

# 2. Get only relevant cases to match with DCC

# create a column with the most standardized data_id
numbat_datasets_7_id_coalesced <- numbat_datasets_6_m_val_done |> 
  mutate(data_id_m_val = na_if(data_id_m_val, "")) |>
  mutate(dataset_for_matching =
           coalesce(data_id_m_val, dataset_for_matching, data_id_no_ex_chr)) |> 
    relocate(dataset_for_matching, .after = "data_id_m_val")

save_cr(numbat_datasets_7_id_coalesced,
        file = file.path(here("data", "wrangling_steps", "charite",
                              "for_datastet_exc", "numbat_datasets_7_id_coalesced.RData"))) # as RData

# load ids col from Blanka's xlsx file (the one that she is filling with metadta)

id_col <- read_excel(here("data",
                     "verification",
                     "m_val",
                     "for_datastet_exc",
                     "dataset_for_matching_col_to_mutate.xlsx"))


# sanity check: verify that non of the ids are in dcc_charite_joined

load(here("data", "wrangling_steps", "dcc_charite", "dcc_charite_joined_4_rm_au_ov.RData")) # load dcc_charite_joined

id_col |> dplyr::filter(dataset_for_matching %in% dcc_charite_joined_4_rm_au_ov$detected_id) # none.

# mutate to label "ignore" if id is in numbat_datasets_7_id_coalesced$ataset_for_matching

id_col_labeled <- id_col |> 
  mutate(ignore = case_when(
    dataset_for_matching %in% numbat_datasets_7_id_coalesced$dataset_for_matching
    ~ "TRUE",
    .default = "FALSE"
  ))

# check if there are any ids to ignore

id_col_labeled |> dplyr::filter(ignore == "TRUE") |> View()

# write dataset and mutated columns into execel
write_xlsx(id_col_labeled, here("data", "verification", "m_val", "for_datastet_exc", "id_col_labeled.xlsx"))

# This mutated column was then pasted into her xlsx file.