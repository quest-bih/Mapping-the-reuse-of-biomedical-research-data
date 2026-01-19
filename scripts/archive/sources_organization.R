# 1. make sure that ds+added has numbat as well ---------------------------

# load filled grouped ds+ad
charite_dois_ids_distinct_filled <- read_excel(here("data",
                                                    "verification",
                                                    "datastet_and_added_summarised",
                                                    "datastet_and_added_filled_raw",
                                                    "charite_dois_ids_distinct_v9.xlsx"))

# check that the "in numbat" actually means "not in matched numbat and not in 200 non matched sampled numbat":

# get grouped ids of ds+ad
t <- charite_dois_ids_distinct_filled |> 
  rename(in_numbat = `ignore - already in numbat and wasn't detected in dcc`) |> 
  select(dataset_for_matching, in_numbat) |> 
  dplyr::filter(in_numbat == "TRUE")

# get only "in matched numbat and in 200 non matched sampled numbat" from grouped ds+ad ids
t |> dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$dataset_for_matching) # not in dataset for matching
t |> dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id) # not in detected dataset

# check that they're not in the original matched dcc_ds list as well:

# get original ids of ds+ad
o <- dcc_ds_and_added_joined_4_rm_au_ov |> 
  select(dataset_for_matching, detected_id, in_numbat_or_data_articles)

# get only "in matched numbat and in 200 non matched sampled numbat" from orig ds+ad

o |> dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$dataset_for_matching) # not in dataset for matching
o |> dplyr::filter(dataset_for_matching %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id) # not in detected dataset

# "detected_id" which is actually "id_originally detected_in_numbat" of course should have matches!
o |> dplyr::filter(detected_id %in% numbat_da_dcc_joined_4_rm_au_ov$dataset_for_matching) # yes
o |> dplyr::filter(detected_id %in% numbat_da_dcc_joined_4_rm_au_ov$detected_id) # yes


# But understand also what happens in the original original, before matching:
# "added_and_ds_for_matching_4_rm_exist" in ds_added qmd means that only ids not in numbat_da were trying to get a match.
# So how come there are numbat_da ids in the matched (original matched and grouped matched?)

# Let's make it clear:

# ds+ad before matching, including numbat+da: this is where I can find ovelapping dastet/numbat matches!
added_and_ds_for_matching_3_lbl |> select(dataset_for_matching) |> distinct()

# ds+ad before matching, excluding numbat+da
added_and_ds_for_matching_4_rm_exist |> select(dataset_for_matching) |> distinct()

# so where did the "TRUE" in_numbat come from in the matched?! from me? from Blanka?

# Well, I didn't have an "in_numbat" col in the grouped_to_fill that I sent Blanka.

# First check that she's right at all:

# maybe it's in the numbat_da to match
charite_dois_ids_distinct_filled |> 
  dplyr::filter(dataset_for_matching %in% numbat_da_dois_and_ids_9_clean_pairs$dataset_for_matching) |> 
  View()

charite_dois_ids_distinct_filled |> 
  dplyr::filter(dataset_for_matching %in% numbat_da_dois_and_ids_9_clean_pairs$data_id_secondary) |> 
  View()

# if it's there, then how did I miss it?
# OMG I got it. it's in the original Numbat but not for numbat for matching. It didn't make the cut for OD.
# It was actually my first guess: it's not in the matched and not in the 200 sampled, so it has to be
# in the numbat non-matched not-sampled!

# So now I have to make sure that the filtered OD ds_ad don't have any TRUE in them!

# let's load the filled clean again:
charite_dois_ids_distinct_filled <- read_excel(here("data",
                                                    "verification",
                                                    "datastet_and_added_summarised",
                                                    "datastet_and_added_filled_raw",
                                                    "charite_dois_ids_distinct_v9.xlsx"))

# and get only OD cases (this is from ds_ad qmd):
to_join <- charite_dois_ids_distinct_filled |>
  dplyr::filter(is_dataset == "y") |>
  dplyr::filter(`data authorship` %in% c("own data", "data authorship"))

# and check if there's any "TRUE":
to_join |> dplyr::filter(`ignore - already in numbat and wasn't detected in dcc` == "TRUE")