dcc_analysis_no_source_overlap_compact <- dcc_charite_joined_final_no_source_overlap |> 
  select(-c(
    doi_lc,
    data_id_lc,
    doi_no_ver_info,
    data_id_auto_cleaned,
    slug_charite,
    data_id_m_val,
    in_dashboard,
    numbat_id_of_da_id,
    numbat_id_of_da_id_lc,
    listed_in_numbat_output,
    validated,
    doi_id_orig_pair,
    doi_id_lc_pair_for_joining,
    doi_id_clean_pair,
    id,
    created,
    updated,
    slug_dcc,
    was_the_id_standardized,
    affiliationsROR,
    fundersROR,
    primary,
    license,
    detected_in_numbat_originally_only_for_added,
    Category,
    authors_charite,
    authors_dcc))

cat(colnames(dcc_analysis_no_source_overlap_compact), sep = "\n")
library(writexl)            

write_xlsx(dcc_analysis_no_source_overlap_compact, here("data", "verification", "verification of sample", "dcc_analysis_no_source_overlap_compact.xlsx"))

