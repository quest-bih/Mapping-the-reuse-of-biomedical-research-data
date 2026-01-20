# Here I will check if there were any cases that were accidentally excluded during this step:
#   dplyr::filter(map2_lgl(authors_datastet_and_added, authors_dcc, no_common_element)) |> 
# distinct(doi_datastet_and_added, dataset_for_matching, doi_dcc, .keep_all = TRUE)
# in ds_added... .qmd.

# The bottom line: no ids, doi_charite and doi_dcc were missing, but some added (that were also datastet) were removed.
# So I'll add them retroactively as "duplicates" but under "added" source.

# first check:

# load old files
load(here("data", "wrangling_steps", "dcc_datastet_and_added", "add_added_post_hoc", "dcc_ds_and_added_joined_3_au_info.RData"))
load(here("data", "wrangling_steps", "dcc_datastet_and_added", "add_added_post_hoc", "dcc_ds_and_added_joined_4_rm_au_ov.RData"))


t3 <- dcc_ds_and_added_joined_3_au_info
t4 <- dcc_ds_and_added_joined_4_rm_au_ov

# check what was done originally
t4_old_check <- t3 |> 
  dplyr::filter(map2_lgl(authors_datastet_and_added, authors_dcc, no_common_element)) |> 
  distinct(doi_datastet_and_added, dataset_for_matching, doi_dcc, .keep_all = TRUE)

# what should have been done
t4_new_check <- t3 |> 
  dplyr::filter(map2_lgl(authors_datastet_and_added, authors_dcc, no_common_element)) |> 
  distinct()

waldo::compare(t4_old_check, t4) # sanity check

# get missed cases
anti_join(t4_new_check, t4_old_check) |> View()

# Inspect which cases were accidentally excluded:

# Define the composite key for joining
key_cols <- c("doi_datastet_and_added", "dataset_for_matching", "doi_dcc")

# Step 1: Left join to get all matching rows from new
joined <- t4_old_check |>
  select(all_of(key_cols)) |>
  left_join(t4_new_check, by = key_cols) |>
  dplyr::mutate(across(everything(), as.character))


# Step 2: For each key group, compare rows to old
differences <- joined |>
  pivot_longer(
    cols = -all_of(key_cols),
    names_to = "column",
    values_to = "value"
  ) |>
  group_by(across(all_of(key_cols)), column) |>
  summarise(
    n_unique = n_distinct(value),
    .groups = "drop"
  ) |>
  dplyr::filter(n_unique > 1)  # means the old value differs from at least one in new

differences_summary <- differences |>
  count(column, name = "n_different_cases") |>
  arrange(desc(n_different_cases))

# Conclusion: There were cases of "added" (that were also datastet) that were accidentally removed !

# get (distinct) ids that were accidentally removed as "added":

ids_source_added <- differences |>
  dplyr::filter(column == "source_datastet_and_added") |>
  select(doi_datastet_and_added, dataset_for_matching, doi_dcc) |>
  distinct()

# make sure that there's only 2 unique sources for these cases, which will mean that the other case is "added"
differences |> dplyr::filter(column == "source_datastet_and_added") |> dplyr::filter(n_unique != 2) # yes

# # check them out in the current all_sources:
# 
# dcc_detected_ids_all_sources_7_w_metadata |> 
#   dplyr::filter(detected_id %in% ids_source_added$dataset_for_matching) |> 
#   View()

# SO NOW I HAVE TO ADD THESE CASES AS ADDED TO _7_ (ADD THE DATA ARTICLES ds001226 AS WELL):
# just duplicate the rows where yoou have these ids and source = datastet (and then again for data articles just for ds001226)
# and change source_charite to added

# make sure to save old _3 and _4 and the vars that you created here in the script because you're using them to find which id rows to dupblicate!
# and then run _8_ again.


# AND CHECK METADATA BECAUSE YOU'LL PROBABLY HAVE TO ADD THEM THERE TOO!

# check with dummy data:

df <- tibble::tibble(
  col1 = c(1, 2, 3, 4, 1), 
  col2 = c("a", "b", "c", "d", "e"), 
  col3 = c("x", "y", "z", "p", "p"), 
  col4 = c("r", "t", "h", "g", "x"), 
  source = c("A", "A", "A", "B", "A")
)

ids_to_add_as_source_b <- tibble::tibble(
  col1 = c(1, 2),
  col2 = c("a", "b"),
  col3 = c("x", "y")
)

rows_to_duplicate <- df |>
  inner_join(ids_to_add_as_source_b, by = c("col1", "col2", "col3")) |>
  mutate(source = "B")

df_result <- bind_rows(df, rows_to_duplicate)

# It works, so I'll write the true code under joined_bind_add... .qmd.

# save variable to load there:
save_cr(ids_source_added, file = file.path(here("data", "wrangling_steps", "dcc_datastet_and_added", "add_added_post_hoc", 
                                                            "ids_source_added.RData")))
