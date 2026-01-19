# check osf

library(dplyr)
library(stringr)

# 1. Extract suffixes
numbat_suffixes <- sub("osf_", "", numbat_da_dois_and_ids_9_clean_pairs$dataset_for_matching)
added_suffixes <- sub("osf_", "", added_and_ds_for_matching_4_rm_exist$dataset_for_matching)

# 2. Combine and deduplicate
all_suffixes <- unique(c(numbat_suffixes, added_suffixes))

# 3. Escape special characters in suffixes
escaped_suffixes <- str_replace_all(all_suffixes, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")

# 4. Construct robust regex pattern
pattern <- str_c("osf[^\\s/]*(", str_c(escaped_suffixes, collapse = "|"), ")", collapse = "")

dcc_with_osf <- DCC_corpus_11_std_lbl  |> 
  dplyr::filter(
    str_detect(data_id_or_acc_nr, regex("osf", ignore_case = TRUE)) |
      str_detect(dataset_for_matching, regex("osf", ignore_case = TRUE))
  )

# 5. Filter `dcc` rows where pattern matches in either column
dcc_matched <- dcc_with_osf |> 
  dplyr::filter(
    str_detect(data_id_or_acc_nr, regex(pattern, ignore_case = TRUE)) |
      str_detect(dataset_for_matching, regex(pattern, ignore_case = TRUE))
  )
