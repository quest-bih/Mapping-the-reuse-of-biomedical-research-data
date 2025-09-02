# This script verifies that indeed no matched datastet / added id is in the numbat matched / non-matched list. Indeed that's the case.
# Then it checks if "charite_dois_ids_distinct.xlsx" mistakenly included author overlap cases - it did!
# Then it writes a new version to to exclude author overlap cases.

library(writexl)            

# load numbat for matching and numbat matched
load(here("data", "wrangling_steps", "charite", "charite_dois_and_ids_8_wide.RData"))
load(here("data", "wrangling_steps", "dcc_charite", "dcc_charite_joined_4_rm_au_ov.RData"))

# load datastet and added matched
load(here("data", "wrangling_steps", "dcc_charite", "dcc_ds_and_added_joined_4_rm_au_ov.RData"))

# load files from git
g <- read_excel(here("data", "verification", "datastet_and_added_summarised", "dois_ids_grouped.xlsx")) # grouped
d <- read_excel(here("data", "verification", "datastet_and_added_summarised", "charite_dois_ids_distinct.xlsx")) # distinct

# load files that I sent to the team in order to make sure that they are the same as the files from git
g_teams <- read_excel(here("data", "verification", "datastet_and_added_summarised", "archive", "dois_ids_grouped.xlsx"))
d_teams <- read_excel(here("data", "verification", "datastet_and_added_summarised", "archive", "charite_dois_ids_distinct.xlsx"))

# make sure "teams" files are the same as "git" files
identical(g$dataset_for_matching, g_teams$dataset_for_matching) # identical
identical(d$dataset_for_matching, d_teams$dataset_for_matching) # teams one has 2 more entries

# check d and d_teams discrepancy
d_teams |> select(dataset_for_matching) |> dplyr::filter(!dataset_for_matching %in% d$dataset_for_matching) # it's just 2 NAs

# look for datastet / added matched ids (after removing numbat entries) in numbat matched / non-matched list

# dcc - charite
dcc_ds_and_added_joined |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% dcc_charite_joined_4_rm_au_ov$detected_id)

g |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% dcc_charite_joined_4_rm_au_ov$detected_id)

d |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% dcc_charite_joined_4_rm_au_ov$detected_id)

#numbat for matching - primary dataset
dcc_ds_and_added_joined |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% charite_dois_and_ids_8_wide$dataset_for_matching)

g |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% charite_dois_and_ids_8_wide$dataset_for_matching) 

d |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% charite_dois_and_ids_8_wide$dataset_for_matching)

#numbat for matching - secondary dataset
dcc_ds_and_added_joined |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% charite_dois_and_ids_8_wide$data_id_secondary)

g |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% charite_dois_and_ids_8_wide$data_id_secondary)

d |>
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% charite_dois_and_ids_8_wide$data_id_secondary)

# verify that all ids are indeed in DCC
d |> 
  select(dataset_for_matching) |> 
  distinct() |> 
  dplyr::filter(dataset_for_matching %in% DCC_corpus_11_std_lbl$dataset_for_matching) |>
  View()

# quick overview in dcc on matched
DCC_corpus_11_std_lbl |> 
  select(dataset_for_matching, doi) |> 
  dplyr::filter(dataset_for_matching %in% d$dataset_for_matching) |> 
  select(doi) |> 
  distinct() |> nrow()

# check that 4_rm_au_ov and d are the same:

dcc_ds_and_added_joined_4_rm_au_ov |> 
  select(dataset_for_matching) |> 
  distinct() |>
  dplyr::filter(!dataset_for_matching %in% d$dataset_for_matching)

dcc_ds_and_added_joined_4_rm_au_ov |> 
  select(dataset_for_matching) |> 
  distinct() |>
  dplyr::filter(!dataset_for_matching %in% d$dataset_for_matching)

# here I check datasets that are in "charite_dois_ids_distinct" and not in "4_rm_au_ov"
in_d_and_not_in_4_rm_au_ov <- d |> 
  select(dataset_for_matching) |> 
  distinct() |>
  dplyr::filter(!dataset_for_matching %in% dcc_ds_and_added_joined_4_rm_au_ov$dataset_for_matching)

# there are some (they were author ovelap cases), so I'll remove them.
# create list of ids to remove
ids_to_remove <- d_teams |> 
  dplyr::filter(dataset_for_matching %in% in_d_and_not_in_4_rm_au_ov$dataset_for_matching) |> 
  select(dataset_for_matching)

# create charite_dois_ids_distinct_v1 with excluding ids_to_remove
charite_dois_ids_distinct_v1 <- d_teams |> 
  dplyr::filter(!dataset_for_matching %in% ids_to_remove$dataset_for_matching)

# verification
d_teams |> dplyr::filter(dataset_for_matching %in% ids_to_remove$dataset_for_matching) |> View()

# save (also uploaded in teams)
write_xlsx(charite_dois_ids_distinct_v1, here("data", "verification", "datastet_and_added_summarised","charite_dois_ids_distinct_v1.xlsx"))
