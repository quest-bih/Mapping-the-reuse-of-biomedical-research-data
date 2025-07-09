library(stringr)
library(dplyr)

extract_ids <- function(text) {
  all_matches <- character()
  
  # cond0
  cond0 <- str_extract_all(text, "(?i)(figshare|zenodo|osf|mendeley|harvard|dryad)[^\\s,)]*")[[1]] |>
    str_remove("[.,;)]$") |>
    unique()
  all_matches <- c(all_matches, cond0)
  
  skip_if_included <- function(new_matches, existing_matches) {
    new_matches[!sapply(new_matches, function(nm) any(str_detect(existing_matches, fixed(nm)) | str_detect(nm, fixed(existing_matches))))]
  }
  
  prefixes <- c(
    "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
    "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
    "sr(p|rx|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
    "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn"
  )
  
  # cond1
  prefix_pattern <- paste0("(?<![a-zA-Z0-9])(?:", paste(prefixes, collapse = "|"), ")[0-9]+(?:\\.[0-9]+)*")
  cond1 <- str_extract_all(text, regex(prefix_pattern, ignore_case = TRUE))[[1]] |>
    unique() |>
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond1)
  
  # cond2
  cond2 <- str_extract_all(text, "10\\.[^\\s,)]+")[[1]] |>
    unique() |>
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond2)
  
  # cond3
  cond3 <- str_extract_all(text, "//[^\\s,)]+")[[1]] |>
    str_remove("^//") |>
    unique() |>
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond3)
  
  # other patterns
  other_patterns <- c(
    "fcon_ ?1000\\.projects\\.nitrc\\.org",
    "(?<!of )[0-9]{6,10}\\b",
    "dip:[0-9]{3}",
    "fr-fcm-[a-z0-9]{4}",
    "collections?(?:[:/])[0-9]{4}",
    "icpsr ?[0-9]{4}",
    "sn ?[0-9]{4}",
    "search\\.kg\\.ebrains\\.eu",
    "[a-z]{1}[:digit:]{4}",
    "[a-z]{2}[:digit:]{6}",
    "[a-z]{3}[:digit:]{5}",
    "[a-z]{4,6}[:digit:]{3,}"
  )
  
  cond_other <- unlist(lapply(other_patterns, function(pat) str_extract_all(text, regex(pat, ignore_case = TRUE))[[1]])) |>
    unique() |>
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond_other)
  
  # remove pure digits
  all_matches <- all_matches[!str_detect(all_matches, "^\\d+$")]
  
  if (length(all_matches) == 0) {
    return(NA_character_)
  }
  return(paste(unique(all_matches), collapse = ";"))
}

datastet_results <- all_results_unique |> 
  select(doi, context, year) |> 
  mutate(context = tolower(context)) |> 
  distinct()
  

# Mutate into the dataframe test
datastet_results_1_ext_ids <- datastet_results  |> 
  mutate(extracted_id = vapply(context, extract_ids, FUN.VALUE = character(1)))

# reshape
datastet_results_2_reshaped <- datastet_results_1_ext_ids |> 
  separate_rows(extracted_id, sep = ";") |> 
  select(doi, extracted_id, year) |> 
  distinct()

# clean
datastet_results_3_cleaned <- datastet_results_2_reshaped |> 
  dplyr::filter(
    !(
      str_detect(extracted_id, "zenodo|dryad|osf|figshare|harvard|mendeley|//github\\.") &
        !str_detect(extracted_id, "[0-9]"))) |> 
  dplyr::filter(!str_detect(extracted_id, "^[a-zA-Z][0-9]{1,2}$")) |> 
  dplyr::filter(!is.na(extracted_id)) |> 
  dplyr::filter(extracted_id != "") |> 
  distinct()

# save as input for ds_datastet...
save_cr(datastet_results_3_cleaned, file = file.path(here("data",
                                                          "raw",
                                                          "datastet",
                                                          "datastet_results_3_cleaned.RData")))