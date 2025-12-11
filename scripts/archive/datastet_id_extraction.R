# This scripts tests extraction of DataStet identifiers. It uses "%>%" as a precaution.

test <- all_results_extracted_ids |> 
  select(doi, year, context) |> 
  mutate(context = tolower(context)) |> 
  distinct()

extract_ids <- function(text) {
  all_matches <- character()
  
  # Condition 0
  cond0 <- str_extract_all(text, "(?i)(figshare|zenodo|osf|mendeley|harvard|dryad)[^\\s,)]*")[[1]] %>% 
    str_remove("[.,;)]$") %>% 
    unique()
  all_matches <- c(all_matches, cond0)
  
  # Helper for skipping included matches
  skip_if_included <- function(new_matches, existing_matches) {
    new_matches[!sapply(new_matches, function(nm) any(str_detect(existing_matches, fixed(nm)) | str_detect(nm, fixed(existing_matches))))]
  }
  
  # Condition 1
  prefixes <- c(
    "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
    "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
    "sr(p|r|x|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
    "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn"
  )
  
  pattern1 <- paste0("(?<![a-zA-Z0-9])(?:", paste(prefixes, collapse = "|"), ")[0-9]+(?:\\.[0-9]+)*")
  cond1 <- str_extract_all(text, regex(pattern1, ignore_case = TRUE))[[1]] %>% 
    unique() %>% 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond1)
  
  # Condition 2
  cond2 <- str_extract_all(text, "10\\.[^\\s,)]+")[[1]] %>% 
    unique() %>% 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond2)
  
  # Condition 3
  cond3 <- str_extract_all(text, "//[^\\s,)]+")[[1]] %>% 
    unique() %>% 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond3)
  
  # Other conditions
  other_patterns <- c(
    "fcon_ ?1000\\.projects\\.nitrc\\.org",
    "(?<!of )[0-9]{6,10}\\b",
    "dip:[0-9]{3}",
    "fr-fcm-[a-z0-9]{4}",
    "collections?(?:[:/])[0-9]{4}",
    "icpsr ?[0-9]{4}",
    "sn ?[0-9]{4}",
    "search\\.kg\\.ebrains\\.eu"
  )
  
  cond_other <- unlist(lapply(other_patterns, function(pat) str_extract_all(text, regex(pat, ignore_case = TRUE))[[1]])) %>% 
    unique() %>% 
    skip_if_included(all_matches)
  all_matches <- c(all_matches, cond_other)
  
  if (length(all_matches) == 0) {
    return(NA_character_)
  }
  
  return(paste(unique(all_matches), collapse = ";"))
}

test$id_extract <- vapply(test$context, extract_ids, FUN.VALUE = character(1))


# reshape
test_reshaped <- test |> 
  separate_rows(extracted_id, sep = ";") |> 
  select(doi, extracted_id, year) |> 
  distinct()

# clean
test_cleaned <- test_reshaped |> 
  dplyr::filter(
    !(
      str_detect(extracted_id, "zenodo|dryad|osf|figshare|harvard|mendeley|//github\\.") &
        !str_detect(extracted_id, "[0-9]"))) |> 
  dplyr::filter(!str_detect(extracted_id, "^[a-zA-Z][0-9]{1,2}$")) |> 
  dplyr::filter(!is.na(extracted_id)) |> 
  dplyr::filter(extracted_id != "") |> 
  distinct()


