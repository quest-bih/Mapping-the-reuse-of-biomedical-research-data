## File: extract_patterns_tidiverse_pipe.R

library(tidyverse)

# Define all your patterns
patterns <- c(
  "G(SE|SM|DS|PL)[0-9]{2,}",
  "PRJ(E|D|N|EB|DB|NB)[0-9]+",
  "SAM(E|D|N)[A-Z]?[0-9]+",
  "[A-Z]{1}[0-9]{5}",
  "[A-Z]{2}[0-9]{6}",
  "[A-Z]{3}[0-9]{5}",
  "[A-Z]{4,6}[0-9]{3,}",
  "GCA_[0-9]{9}\\.[0-9]+",
  "SR(P|R|X|S|Z)[0-9]{3,}",
  "(E|P)-[A-Z]{4}-[0-9]{1,}",
  "(?<!\\.|<)[0-9]{1}[a-z]{1}[[:alnum:]]{2}",
  "MTBLS[0-9]{2,}",
  "fcon_ ?1000\\.projects\\.nitrc\\.org",
  "(?<!of )[0-9]{6,10}\\b",
  "[A-Z]{2,3}_[0-9]{5,}",
  "[A-Z]{2,3}-[0-9]{4,}",
  "[A-Z]{2}[0-9]{5}-[A-Z]{1}",
  "DIP:[0-9]{3}",
  "FR-FCM-[[:alnum:]]{4}",
  "Collections?(:|/)[0-9]{4}",
  "ICPSR [0-9]{4}",
  "SN [0-9]{4}",
  "search.kg.ebrains.eu",
  "//github.com\\S*",
  "//osf.io\\S*",
  "gse\\S*",
  "gsm\\S*",
  "e-mtab-\\S*",
  "egas\\S*",
  "egad\\S*",
  "e-geod\\S*",
  "mk\\S*",
  "mh\\S*",
  "phs\\S*",
  "mn\\S*",
  "mw\\S*",
  "pxd\\S*",
  "srr\\S*",
  "prjeb\\S*",
  "emd-\\S*",
  "gcst\\S*",
  "pdb_\\S*",
  "nm_\\S*",
  "nct\\S*",
  "err\\S*",
  "gds\\S*",
  "msv\\S*",
  "mz\\S*",
  "nc_\\S*",
  "np_\\S*",
  "prjna\\S*",
  "prjca\\S*",
  "srp\\S*",
  "phs\\S*",
  "pgs\\S*",
  "s-bsst\\S*",
  "mt\\S*",
  "kt\\S*",
  "st\\S*",
  "ol\\S*",
  "op\\S*",
  "or\\S*",
  "oq\\S*",
  "scp\\S*",
  "s-biad\\S*",
  "e-tabm\\S*",
  "srx\\S*",
  "empiar\\S*",
  "fr-fcm-z\\S*"
)

extract_patterns <- function(text) {
  matches <- map(patterns, ~str_extract_all(text, regex(.x, ignore_case = TRUE))[[1]]) %>%
    flatten_chr() %>%
    unique()
  if (length(matches) == 0) return(NA_character_) else return(paste(matches, collapse = ";"))
}

df <- all_results_extracted_ids |>
  select(context, url_rawForm, datastet_ids) |> 
  distinct() |> 
  mutate(extracted = sapply(context, extract_patterns))
