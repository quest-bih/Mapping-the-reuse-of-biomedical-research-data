
# 1. Numbat ---------------------------------------------------------------

# 1. Check sources

numbat_da_dois_and_ids_9_clean_pairs |> 
  select(source) |> 
  unique()

# 2. Check for WS

numbat_da_dois_and_ids_9_clean_pairs |> 
  select(data_id_secondary) |> 
  dplyr::filter(str_detect(data_id_secondary, "\\s")) |> 
  distinct() |> 
  View()

numbat_da_dois_and_ids_9_clean_pairs |> 
  select(dataset_for_matching) |> 
  dplyr::filter(str_detect(dataset_for_matching, "\\s")) |> 
  distinct() |> 
  View()


# 3. Check that dataset_for_matching is coalesced correctly

numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(is.na(dataset_for_matching))



# 2. Numbat-DCC-Joined ----------------------------------------------------

# 1. Check sources

numbat_da_dcc_joined_1_det_id |> 
  select(source_charite) |> 
  unique()

# 2. Check for WS

numbat_da_dcc_joined_1_det_id |> 
  select(data_id_secondary) |> 
  dplyr::filter(str_detect(data_id_secondary, "\\s")) |> 
  distinct() |> 
  View()

numbat_da_dcc_joined_1_det_id |> 
  select(dataset_for_matching) |> 
  dplyr::filter(str_detect(dataset_for_matching, "\\s")) |> 
  distinct() |> 
  View()

numbat_da_dcc_joined_1_det_id |> 
  select(detected_id) |> 
  dplyr::filter(str_detect(detected_id, "\\s")) |> 
  distinct() |> 
  View()


# 2. Additional & DataStet -------------------------------------------------


# 1. Check sources

added_and_ds_for_matching_4_rm_exist |> 
  select(source) |> 
  unique()

# 2. Check for WS

added_and_ds_for_matching_4_rm_exist |> 
  select(dataset_for_matching) |> 
  dplyr::filter(str_detect(dataset_for_matching, "\\s")) |> 
  distinct() |> 
  View()


# 3. Check that additional_ids don't have any numbat identifiers

added_and_ds_for_matching_4_rm_exist |> 
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id) |> 
  View()

# remove added ids ("data_id_auto_cleaned") that already exist as detected_ids of numbat with the same doi of numbat's detected_id

load(here("data", "wrangling_steps", "dcc_charite", "numbat_da_dcc_joined_4_rm_au_ov.RData")) # load matched numbat and da with dcc

test_old <- added_and_ds_for_matching_3_lbl |> 
  dplyr::filter(in_numbat_or_data_articles == "FALSE")

test_new <- added_and_ds_for_matching_3_lbl |> 
  dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id)

test_newer <- added_and_ds_for_matching_4_rm_exist |> 
  dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id)

# 4. Check that dataset_for_matching is coalesced correctly

# make sure that you understand the str (data type) of m_val and dataset_for_matching in order to coalesce correctly!

added_and_ds_for_matching_2_rm_exc |> 
  dplyr::filter(is.na(dataset_for_matching)) # ok.

added_and_ds_for_matching_2_rm_exc |> select(dataset_for_matching) |> distinct() |> View() # ok.

# Check for WS

added_and_ds_for_matching_2_rm_exc |> 
  select(dataset_for_matching) |> 
  dplyr::filter(str_detect(dataset_for_matching, "\\s")) |> 
  distinct() # 2 cases that are not relevant

# then remove ws from dataset_for_matching:

# 5. Get additional DataStet potential ids from new match - DONE!


# Check sources:

# 1. datastet and added are overlapped between them at the end of added_and_datastet qmd:

  dcc_ds_and_added_joined_7_harm  |> 
    group_by(detected_id) |> 
    dplyr::filter(n() > 1, n_distinct(source_charite) > 1) |> 
    distinct(detected_id, source_charite) |> 
    View() 

# 2. datastet and added are NOT overlapped with numbat and DA at the end of added_and_datastet qmd:

  dcc_ds_and_added_joined_7_harm |> 
    select(detected_id) |> 
    distinct() |> 
    dplyr::filter(detected_id %in% numbat_da_dois_and_ids_9_clean_pairs$dataset_for_matching)
  
  dcc_ds_and_added_joined_7_harm |> 
    select(detected_id) |> 
    distinct() |> 
    dplyr::filter(detected_id %in% numbat_da_dois_and_ids_9_clean_pairs$data_id_secondary)
  
  dcc_ds_and_added_joined_7_harm |> 
    select(detected_id) |> 
    distinct() |> 
    dplyr::filter(detected_id %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id)

# 3. numbat and DA are overlapping:
  
  dcc_numbat_da_joined_final |> 
    dplyr::filter(source_charite == "data_articles") |>  
    distinct(detected_id, source_charite, listed_in_numbat_output) |> 
    View()


# check for overlap between the 2 dfs - good, there's none:

test1 <- dcc_numbat_da_joined_final |> select(detected_id, source_charite)
test2 <- dcc_ds_added_joined_final |> select(detected_id, source_charite)




overlaps <- df1 |> 
  dplyr::inner_join(df2, by = "value", suffix = c("_df1", "_df2"))


df1 <- tibble(
  value = c(1, 2, 3, 3, 4, 5),
  source = c("DS", "DS", "DS", "AD", "AD", "AD")
)

df2 <- tibble(
  value = c(6, 7, 8, 8, 9, 10),
  source = c("NU", "NU", "DA", "NU", "DA", "DA")
)

df1 |> 
  dplyr::group_by(value) |> 
  dplyr::filter(dplyr::n_distinct(source) > 1) |> 
  dplyr::distinct(value, source)

df2 |> 
  dplyr::group_by(value) |> 
  dplyr::filter(dplyr::n_distinct(source) > 1) |> 
  dplyr::distinct(value, source)



# The difference between filter (asymmetrical) and intersect (symmetrical):

# ds ad in nu da
ds_added_in_matched_numbat <- ds_added_extracted_ids |>
  dplyr::filter(dataset_for_matching %in% numbat_da_matched_ids$detected_id) |> 
  select(dataset_for_matching) |> distinct() # this line turns the asymmetry into intersect

# nu da in ds ad
numbat_da_in_ds_added <- numbat_da_matched_ids |> 
  dplyr::filter(detected_id %in% ds_added_extracted_ids$dataset_for_matching) |> 
  select(detected_id) |> distinct() # this line turns the asymmetry into intersect

# only in both
intersect_ids <- as.data.frame(intersect(ds_added_extracted_ids$dataset_for_matching,
                                     numbat_da_matched_ids$detected_id))


##### SO LET'S TRY #####

# 1. Find intersecting IDs between ds_added_extracted_ids and numbat_da_matched_ids
intersect_ids <- intersect(
  ds_added_extracted_ids$dataset_for_matching,
  numbat_da_matched_ids$detected_id
)

# 2. Turn intersecting IDs into a tibble
intersect_df <- tibble(dataset_for_matching = intersect_ids) |> 
  
  # 3. Enrich with only doi and source from numbat_da_matched_ids
  dplyr::left_join(
    ds_added_extracted_ids |> dplyr::select(dataset_for_matching, doi, source),
    by = "dataset_for_matching"
  ) |> 
  distinct()

stacked <- dcc_detected_ids_all_sources |> 
  left_join(
    intersect_df |> dplyr::rename(source_df2 = source, doi_df2 = doi),
    by = "dataset_for_matching"
  ) |> 
  tidyr::pivot_longer(
    cols = c(source, source_df2),
    names_to = "source_origin",
    values_to = "source_charite"
  ) |> 
  dplyr::filter(!is.na(source_charite)) |> 
  dplyr::mutate(
    doi_no_ver_info = dplyr::case_when(
      source_origin == "source_charite" ~ doi,     # from df1
      source_origin == "source_df2" ~ doi_df2,
      .default = NA_character_
    )
  ) |> 
  distinct()

dist_int <- intersect_df |> select(doi_charite, detected_id) |> distinct()

stacked |> dplyr::filter(detected_id %in% dist_int$detected_id) |> nrow()

# example for joining:
df1 <- tibble(
  doi    = c(55, 99, 99, 99, 99, 55, 55, 55, 55, 99, 99),
  id     = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10),
  source = c("DA", "DA", "NU", "NU", "NU", "DS", "DS", "AD", "AD", "DS", "AD"),
  col3   = letters[1:11]
)

df2 <- tibble(
  doi    = c(55, 66, 77, 88),
  id     = c(1, 2, 12, 13),
  source = c("NU", "NU", "AD", "AD")
)

df1_stacked <- df1 |> 
  dplyr::left_join(
    df2 |> dplyr::rename(source_df2 = source, doi_df2 = doi),
    by = "id"
  ) |> 
  tidyr::pivot_longer(
    cols = c(source, source_df2),
    names_to = "source_origin",
    values_to = "source"
  ) |> 
  dplyr::filter(!is.na(source)) |> 
  dplyr::mutate(
    doi = dplyr::case_when(
      source_origin == "source"    ~ doi,     # from df1
      source_origin == "source_df2" ~ doi_df2,
      .default = NA_real_
    )
  ) |> 
  dplyr::select(id, doi, source, col3)



##### compare outputs new and old:


test_old <- ds_added_in_matched_numbat_2_id_cols |>
  dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
  mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet"))

# Set output file
sink("comparison.txt")

# Run the comparison and send output to file
waldo::compare(test_old, test, max_diffs = Inf)

# Close the sink connection
sink()

# Conclusion: 2 figshare dataset_for_matching cases in datastet were added as overlapping with numbat.

# Verify from a different angle:
anti_join(test |> select(-c(data_id_m_val, doi_id_clean_pair_with_source)), test_old |> select(-data_id_m_val)) |> View()

test_new <- ds_added_in_matched_numbat_3_doi_cols 

test_old <- ds_added_in_matched_numbat_3_doi_cols |>
  dplyr::filter(source_charite != "additional_ids_in_numbat") |> 
  mutate(source_charite = case_when(source_charite == "datastet_in_numbat" ~ "datastet"))

anti_join(test |> select(-c(data_id_m_val, doi_id_clean_pair_with_source)), test_old |> select(-data_id_m_val)) |> View()

# Do I have added that are already in numbat at all?

dcc_detected_ids_all_sources_1_dup |> 
  dplyr::group_by(detected_id) |> 
  dplyr::filter(
    any(source_charite == "additional_ids") &
      dplyr::n_distinct(source_charite) > 1
  ) |> 
  dplyr::ungroup() |> 
  dplyr::distinct(detected_id) |> 
  View()


# check ws again:
dcc_detected_ids_all_sources_1_dup |> 
  select(data_id_secondary) |> 
  dplyr::filter(str_detect(data_id_secondary, "\\s")) |> 
  distinct()


dcc_detected_ids_all_sources_1_dup |> 
  select(dataset_for_matching) |> 
  dplyr::filter(str_detect(dataset_for_matching, "\\s")) |> 
  distinct()

dcc_detected_ids_all_sources_1_dup |> 
  select(detected_id) |> 
  dplyr::filter(str_detect(detected_id, "\\s")) |> 
  distinct()



# 6. Add new cases to metadata
