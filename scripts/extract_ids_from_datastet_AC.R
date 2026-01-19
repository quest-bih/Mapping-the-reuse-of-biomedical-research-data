# (Please see end of script for pseudo-code)

# Load datastet output for identifier extraction and cleaning
load(here("data", "raw", "datastet", "archive", "all_results_unique.RData"))

# Define function to extract dataset identifiers from text
extract_ids <- function(text) {
  all_matches <- character()  # initialize empty vector to collect matches
  
  # cond1: extract from general-purpose repository mentions
  cond1_pattern <- "(?i)(figshare\\.|zenodo\\.|osf\\.io/|mendeley\\.com/datasets/|dryad\\.)(?:[\\s\\da-z]*[\\s\\d][\\s\\da-z]*?)(?=[\\)\\],#';\">:])"
  cond1 <- str_extract_all(text, regex(cond1_pattern, ignore_case = TRUE))[[1]] |> 
    unique()  # deduplicate
  all_matches <- c(all_matches, cond1)  # add to collected matches
  
  # Helper to skip matches already included
  skip_if_included <- function(new_matches, existing_matches) {
    new_matches[!sapply(new_matches, function(nm) any(
      str_detect(existing_matches, fixed(nm)) |  # check if new is in existing
        str_detect(nm, fixed(existing_matches))    # or existing in new
    ))]
  }
  
  # cond2: extract identifiers with known prefixes followed by numbers
  prefix_pattern <- paste0("(?i)(?<![a-z0-9])(?:", paste(prefixes, collapse = "|"), ")[0-9\\s]+(?=[^0-9\\s])")
  cond2 <- str_extract_all(text, regex(prefix_pattern, ignore_case = TRUE))[[1]] |> 
    unique() |> 
    skip_if_included(all_matches)  # skip overlaps with previous
  all_matches <- c(all_matches, cond2)  # add
  
  # cond3: extract DOIs
  cond3_pattern <- "10\\.[0-9]+/[a-z0-9\\.\\s]+?(?=[\\)\\],#';\">:])"
  cond3 <- str_extract_all(text, regex(cond3_pattern, ignore_case = TRUE))[[1]] |> 
    unique() |> 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond3)
  
  # cond4: extract URLs (//...)
  cond4_pattern <- "//[^)\\],#';\">:]+"
  cond4 <- str_extract_all(text, regex(cond4_pattern, ignore_case = TRUE))[[1]] |> 
    unique() |> 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond4)
  
  # cond5: extract other patterns (fcon, icpsr, etc.)
  other_patterns <- c(
    "fcon_\\s*1000\\.\\s*projects\\.\\s*nitrc\\.\\s*org",
    "(?<!of )[0-9\\s]{6,10}\\b",
    "dip:[0-9\\s]{3}",
    "fr-fcm-[a-z0-9\\s]{4}",
    "collections?(?:[:/])[0-9\\s]{4}",
    "icpsr\\s*[0-9\\s]{4}",
    "sn\\s*[0-9\\s]{4}",
    "search\\.\\s*kg\\.\\s*ebrains\\.\\s*eu",
    "[a-z]{1}[:digit:]{4}",
    "[a-z]{2}[:digit:]{6}",
    "[a-z]{3}[:digit:]{5}",
    "[a-z]{4,6}[:digit:]{3,}",
    "e\\s*n\\s*c\\s*s\\s*r\\s*0\\s*0\\s*0\\s*[0-9]{3}\\s*[a-z]{3}"
  )
  cond5 <- unlist(lapply(other_patterns, function(pat)
    str_extract_all(text, regex(pat, ignore_case = TRUE))[[1]]
  )) |> 
    unique() |> 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond5)
  
  # Remove identifiers containing only numbers
  all_matches <- all_matches[!str_detect(all_matches, "^\\d+$")]
  
  # Return semicolon-separated list of identifiers or NA if none found
  if (length(all_matches) == 0) {
    return(NA_character_)
  }
  return(paste(unique(all_matches), collapse = ";"))
}

# List of known repository and database prefixes
prefixes <- c(
  "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
  "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
  "sr(p|r|x|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
  "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn"
)

# Prepare base dataset: select doi/context/year, lowercase context, deduplicate
datastet_results <- all_results_unique |> 
  select(doi, context, year) |>  # keep relevant columns
  mutate(context = tolower(context)) |>  # lowercase for consistency
  distinct()  # remove duplicates

#save
save_cr(datastet_results, file = file.path(
  here("data", "raw", "datastet", "archive", "datastet_results.RData")))

# Apply extraction function to extract identifiers
# datastet_results_1_ext_ids <- datastet_results |>
#   mutate(extracted_id = vapply(context, extract_ids, FUN.VALUE = character(1)))  

datastet_results_1_ext_ids <- datastet_results |> 
  mutate(
    extracted_id = case_when(
      # keep "rcsb" cases as they are for manual extraction later
      str_detect(context, regex("r\\s*c\\s*s\\s*b")) ~ context,
      # extract ids using the extraction function
      .default = vapply(context, extract_ids, FUN.VALUE = character(1))
    )
  )

#save
save_cr(datastet_results_1_ext_ids, file = file.path(
  here("data", "raw", "datastet", "archive", "datastet_results_1_ext_ids.RData")))

# Reshape extracted_id from semicolon-separated to long format
datastet_results_2_reshaped <- datastet_results_1_ext_ids |> 
  separate_rows(extracted_id, sep = ";") |>  # split into rows
  select(doi, year, context, extracted_id) |>  # keep relevant
  distinct()  # deduplicate

# Initial cleaning of extracted identifiers
datastet_results_3_cleaned <- datastet_results_2_reshaped |>
  dplyr::filter(  # remove non-numeric repository mentions without digits
    !(
      str_detect(extracted_id, "zenodo|dryad|osf|figshare|harvard|mendeley|//github\\.") &
        !str_detect(extracted_id, "[0-9]")
    )
  ) |> 
  dplyr::filter(!str_detect(extracted_id, "^[a-zA-Z][0-9]{1,2}$")) |>  # remove letter + 1-2 digit codes
  dplyr::filter(!is.na(extracted_id)) |>  # remove NA
  dplyr::filter(extracted_id != "") |>  # remove empty strings
  dplyr::filter(  # remove only-letter or only-digit patterns (with/without //, -, or spaces)
    !str_detect(
      extracted_id,
      "^[-\\sa-zA-Z]+$|^[-\\s0-9]+$|^//[-\\sa-zA-Z]+$|^//[-\\s0-9]+$"
    )
  ) |> 
  distinct()  # deduplicate   

# Standardize identifiers step-by-step
datastet_results_4_std <- datastet_results_3_cleaned |> 
  mutate(
    slug = extracted_id |> 
      str_replace_all("\\s", "") |>  # remove spaces
      str_extract("osf\\.io/([A-Za-z0-9]{4,5})") |>  # extract slug from osf.io
      str_remove("osf\\.io/"),  # remove prefix
    data_id_auto_cleaned = case_when(
      str_detect(extracted_id, "zenodo\\.org/record") ~ str_c(  # if zenodo full URL
        "10.5281/zenodo.",
        extracted_id |> 
          str_extract("zenodo\\.org/record/? *([0-9]+)") |>  # extract record id
          str_remove("^zenodo\\.org/record/? *")  # remove prefix
      ),
      str_detect(extracted_id, "zenodo") ~ str_c(  # if zenodo mention
        "10.5281/zenodo.",
        extracted_id |> 
          str_extract("zenodo\\.? *([0-9]+)") |>  # extract id
          str_remove("^zenodo\\.? *")  # remove prefix
      ),
      !is.na(slug) ~ paste0("//osf.io/", slug),  # reconstruct osf URL if slug found
      str_detect(extracted_id, "figshare") ~ str_c(  # figshare handling
        "10.6084/m9.figshare.",
        extracted_id |> 
          str_extract("figshare\\.? *([0-9]+)") |>  # extract id
          str_remove("^figshare\\.? *")  # remove prefix
      ),
      str_detect(extracted_id, "mendeley.*datasets") ~ str_c(  # mendeley datasets
        "10.17632/",
        extracted_id |> 
          str_replace_all("\\s", "") |>  # remove spaces
          str_extract("datasets/([A-Za-z0-9]+)") |>  # extract dataset id
          str_remove("^datasets/")  # remove prefix
      ),
      str_detect(extracted_id, "mendeley") ~ NA_character_,  # drop general mendeley
      str_detect(extracted_id, "dryad") ~ str_c(  # dryad handling
        "10.5061/dryad.",
        extracted_id |> 
          str_replace_all("\\s", "") |>  # remove spaces
          str_extract("dryad\\.?([A-Za-z0-9]+)") |>  # extract id
          str_remove("^dryad\\.?")  # remove prefix
      ),
      str_detect(str_replace_all(extracted_id, "\\s", ""), str_c(prefixes, collapse = "|")) &  # if prefix detected
        !str_detect(extracted_id, "//") &  # no URL
        !str_starts(str_trim(extracted_id), "10\\.") ~ (  # not a DOI
          extracted_id |> 
            str_replace_all("(?<=\\d) \\d{1,2}(\\s*)$", "") |>  # remove trailing small number blocks
            str_replace_all(" \\s*", "") |>  # remove spaces
            str_extract(str_c("^(?:", str_c(prefixes, collapse = "|"), ")[0-9]+")) |>  # extract prefix+numbers
            coalesce(extracted_id)  # fallback: keep as is
        ),
      str_detect(extracted_id, "10\\.") &  # if DOI detected
        !str_detect(extracted_id, "figshare|zenodo|osf|dryad|mendeley") &  # not known repos
        !str_detect(extracted_id, fixed(doi)) ~ extracted_id |> 
        str_replace_all("\\s+", "") |>  # remove spaces
        str_extract("10\\.[^\\s]+"),  # extract DOI
      str_detect(extracted_id, "id=") ~ extracted_id |>  # extract id= values
        str_extract("id=([^\\s/&]+)") |> 
        str_remove("^id="),
      str_detect(extracted_id, "addgene\\.org/") ~ extracted_id |>  # addgene URL cleanup
        str_extract("addgene\\.org/([0-9 \\t]*)") |> 
        str_remove("addgene\\.org/") |> 
        str_remove_all("\\s"),
      .default = extracted_id  # fallback: keep as is
    ),
    dataset_for_matching = case_when(
      !is.na(slug) ~ paste0("osf_", slug),  # create osf slug for matching
      .default = NA
    )
  )

# Filter out identifiers with unwanted patterns post-standardization
datastet_results_5_filtered <- datastet_results_4_std |> 
  dplyr::filter(!(str_starts(data_id_auto_cleaned, "//")
                  & !str_starts(data_id_auto_cleaned, "//osf.io/"))  # remove values starting with "//" unless they're valid OSF links
  ) |>  dplyr::filter(!str_detect(str_replace_all(data_id_auto_cleaned, "\\s", ""), "^or\\d+$")) |>  # remove OR identifiers
  dplyr::filter(!str_detect(str_replace_all(data_id_auto_cleaned, "\\s", ""), "^[a-zA-Z]\\d+$"))  # remove single letter+digit

datastet_results_5_filtered <- datastet_results_4_std |> 
  dplyr::filter(
    !(str_starts(data_id_auto_cleaned, "//") &  # if it starts with "//"
        !str_starts(data_id_auto_cleaned, "//osf.io/"))  # ...but it's not a valid OSF link → remove
  ) |> 
  dplyr::filter(
    !str_detect(  # remove identifiers like "or123"
      str_replace_all(data_id_auto_cleaned, "\\s", ""),  # remove whitespace first
      "^or\\d+$"
    )
  ) |> 
  dplyr::filter(
    !str_detect(  # remove identifiers like "a1", "B9", etc.
      str_replace_all(data_id_auto_cleaned, "\\s", ""),  # remove whitespace
      "^[a-zA-Z]\\d+$"  # single letter followed by 1+ digit
    )
  )



# Remove trailing periods
datastet_results_6_rm_trails <- datastet_results_5_filtered |> 
  mutate(data_id_auto_cleaned = str_remove(data_id_auto_cleaned, "\\.+$"))  # remove trailing dots

# Filter out identifiers that match the paper's DOI exactly
datastet_results_7_rm_same_dois <- datastet_results_6_rm_trails |> 
  mutate(data_id_auto_cleaned = str_replace_all(data_id_auto_cleaned, "\\s+", "")) |>  # remove spaces
  dplyr::filter(!str_detect(data_id_auto_cleaned, fixed(doi)))  # remove if equal to DOI

# Filter out additional cases according to Evgeny's comments

# patterns to filter out
custom_patterns <- c(
  "^lation450",          # contains "lation450"
  "^hiseq",              # contains "hiseq"
  "^human",              # contains "human"
  "^matlab2019",         # contains "matlab2019"
  "^[a-zA-Z]{3}$",      # value is exactly 3 letters long
  "^mm",                # starts with "mm"
  "^nct",               # starts with "nct"
  "ovaseq6000"          # contains "ovaseq6000"
)

# filter out according to the patterns
datastet_results_8_rm_patterns <- datastet_results_7_rm_same_dois |> 
  dplyr::filter(
    !str_detect(                       # keep rows that do NOT match …
      data_id_auto_cleaned,                             # … the target character column
      regex(
        paste(custom_patterns, collapse = "|")  # OR-combined patterns
      )
    )
  )


# Save cleaned, standardized dataset identifiers for use in the ds_datastet notebook
save_cr(datastet_results_8_rm_patterns, file = file.path(
  here("data", "raw", "datastet", "datastet_results_8_rm_patterns.RData")))



# pseudo-code:
# 
# first extract by cond1: 
#   cond0 <- str_extract_all(text, "(?i)(figshare|zenodo|osf|mendeley|harvard|dryad)[^\\s,)]*")
# (actually these patterns: figshare.
# zenodo.
# osf.io/
# mendeley.com/datasets/
# dryad.)
# once one of the patterns is extracted (figshare., zenodo. etc.), allow white spaces in the extraction forwards, because there are also cases like: "https:// doi. org/ 10. 5281/ zenodo. 42237 29." (which should be extracted as "zenodo.4223729").
# so the pattern to look for in cond1 should be: "the pattern" (e.g. zenodo."), then "some digits and or letters and or white spaces" and then stop when reaching "." or ")" or "]" or "," or "#" or "'" or ":" or ";" or ">"
# 
# then extract by cond2, unless the extraction of cond2 is already contained in the extraction of cond1
# (for example if you extracted "zenodo.phs005" in cond1, and cond2 detected "phs005", skip this 
# extraction for cond1 because it's the same extraction!).
# 
# cond2: in cond2, look for the pattern: "prefix", then "some digits or white spaces", and then stop when reaching anything that's not digits or whitespaces.
# prefixes <- c(
#     "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
#     "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
#     "sr(p|r|x|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
#     "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn"
#   )
# 
# then extract by cond3, unless cond1's or cond2's extractions are included in what would be extracted by cond3.
# cond3 aims to find DOIs (again, except if they are already extracted differently using cond1 / cond2): 
#   cond3 <- str_extract_all(text, "10\\.[^\\s,)]+")[[1]]
#  
# cond3: look for the pattern: "10.", then "some digits", then "/", then "some digits and or letters and or white spaces" and or ".", and then stop when reaching or ")" or "]" or "," or "#" or "'" or ":" or ";" or ">"
# 
# then extract by cond4, unless cond1's or cond2's or cond3's extractions are included in what would be extracted by cond4.
# cond4 aims to extract anything from "//" forward, unless its already been extracted differnely using cond1/2/3:
# cond4 <- str_extract_all(text, "//[^\\s,)]+")
# 
# So in cond4, look for "//" and then extract "//" and everything after that, until reaching ")" or "]" or "," or "#" or "'" or ":" or ";" or ">"
# for example if you have the string "//zenodo.dspfijds", cond1 will extract "dspfijds" and will make cond4 redundant even though it has "//".
# 
# lastly, other conditions: any of these patterns, unless their extractions are included in any of the previous conditions' extractions - but allow whitspaces:
#   other_patterns <- c(
#     "fcon_\\s*1000\\.\\s*projects\\.\\s*nitrc\\.\\s*org",
#     "rcsb\\.\\s*org/structure/[^).,#']+",
#     "(?<!of )[0-9\\s]{6,10}\\b",
#     "dip:[0-9\\s]{3}",
#     "fr-fcm-[a-z0-9\\s]{4}",
#     "collections?(?:[:/])[0-9\\s]{4}",
#     "icpsr\\s*[0-9\\s]{4}",
#     "sn\\s*[0-9\\s]{4}",
#     "search\\.\\s*kg\\.\\s*ebrains\\.\\s*eu",
#     "[a-z]{1}[:digit:]{4}",
#     "[a-z]{2}[:digit:]{6}",
#     "[a-z]{3}[:digit:]{5}",
#     "[a-z]{4,6}[:digit:]{3,}",
#     "e\\s*n\\s*c\\s*s\\s*r\\s*0\\s*0\\s*0\\s*[0-9]{3}\\s*[a-z]{3}"
#   )
# 
# if it's just digits - it's not extracted!
# 
# in addition, if there's more than one extraction, it's first separated by ";". for example, if phs505 and up0132 are extracted, then the value will be "phs505;up0132". then it's being separated into 2 different rows with the same DOI
