
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

# 5. Get additional DataStet potential ids from new match






# load verified ids with metadata from previous run




