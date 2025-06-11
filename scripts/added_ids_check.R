# this script loads manually added ids for existing matched ids and look for them in the DCC
# then it exclude cases of author overlap


# 1. Load & reshape data ------------------------------------------------------------

# load added ids
added_ids_orig <- read_excel(here("data", "raw", "charite", "additional data ids.xlsx"))

# reshape and clean
added_ids <- added_ids_orig |> 
  rename(more_ids = `further_data_ids (not checked for own vs reuse!)`) |> # rename column
  dplyr::filter(!is.na(more_ids) & more_ids != "none") |> # get only valid entries
  select(-4) |> # get only relevant columns
  separate_rows(more_ids, sep = ",\\s*") |> # reshape
  mutate(more_ids = tolower(more_ids)) |> # lowercase
  mutate(more_ids = case_when(
    more_ids == "phs000356.v2.p1" ~ "phs000356",
    more_ids == "phs001584.v2.p2" ~ "phs001584",
    .default = more_ids
  )) # manually remove version info

# load dcc
load(here("data","wrangling_steps", "dcc", "DCC_corpus_10_id_std.RData"))


# 2. Join -----------------------------------------------------------------

# join
dcc_added_joined <- added_ids |>
  inner_join(DCC_corpus_10_id_std, by = c("more_ids" = "data_id_or_acc_nr"),
             suffix = c("_added", "_dcc")) |>
  select(everything())


# 3. Excluding authors overlap: -------------------------------------------

# Create doi lists for added and dcc to extract info for

# added

added_dois_for_au_and_year_info <- dcc_added_joined |> 
  select(doi_added) |>
  rename(doi = doi_added) |> 
  distinct()

write_csv_cr(
  added_dois_for_au_and_year_info,
  file = here("data",
              "verification",
              "metadata matched",
              "dois to get info of",
              "added_dois_for_au_and_year_info_v1.csv"),
  row.names = FALSE)

# DCC

dcc_dois_for_au_and_year_info_for_added <- dcc_added_joined |> 
  select(doi_dcc) |>
  rename(doi = doi_dcc) |> 
  distinct()

write_csv_cr(
  dcc_dois_for_au_and_year_info_for_added,
  file = here("data",
              "verification",
              "metadata matched",
              "dois to get info of",
              "dcc_dois_for_au_and_year_info_for_added_v1.csv"),
  row.names = FALSE)

### Here I ran the OpenAlex script to get DOIs metadata. And continued:

# Get authors and years info:

load(here("data",
          "verification",
          "metadata matched",
          "added_dois_metadata_v1.RData")) # load added

added_dois_metadata <- final_results # assign

load(here("data",
          "verification",
          "metadata matched",
          "dcc_for_added_dois_metadata_v1.RData")) # load dcc for added

dcc_for_added_dois_metadata <- final_results # assign

rm(final_results) # clean up

# Prepare both metadata lists before joining

added_dois_metadata <- added_dois_metadata |>
  rename(doi_added = doi) |> # rename column
  mutate(doi_added = str_remove(doi_charite, "^https://doi.org/")) # remove prefix

dcc_for_added_dois_metadata <- dcc_for_added_dois_metadata |> 
  rename(doi_dcc = doi) |> # rename column
  mutate(doi_dcc = str_remove(doi_dcc, "^https://doi.org/")) # remove prefix

# Join info into dcc-added list

dcc_added_joined_1_au_info <- dcc_added_joined |> 
  left_join(added_dois_metadata, by = "doi_added", suffix = c("", "_added")) |> 
  left_join(dcc_for_added_dois_metadata, by = "doi_dcc", suffix = c("", "_dcc")) |> 
  rename(publication_year_charite = publication_year,
         authors_charite = authors)

# Function to check if there are no common elements between two semicolon-separated strings
no_common_element <- function(x, y) {
  x_split <- str_split(x, ";")[[1]] # Split column x by semicolons
  y_split <- str_split(y, ";")[[1]] # Split column y by semicolons
  length(intersect(x_split, y_split)) == 0 # TRUE if no common elements
}

# Apply the filter using the function to create a matched list with no "Author Overlap"
dcc_added_joined_2_rm_au_ov <- dcc_added_joined_1_au_info  |> 
  dplyr::filter(map2_lgl(authors_charite, authors_dcc, no_common_element)) |> 
  distinct(doi_added, data_id_merged, doi_dcc, .keep_all = TRUE)

# get doi_dcc+id pairs that are already in dcc_charite_joined, to exclude in the next step

pairs_to_exclude <- anti_join(df2, df1, by = c("col1", "col2"))


dcc_added_joined_3_existing_excluded <- dcc_added_joined_2_rm_au_ov |> 
  anti_join(dcc_charite_joined_8_ds_added_years,
            by = c("doi_dcc" = "doi_dcc", "more_ids" = "data_id_merged"))
            

dcc_added_joined_2_rm_au_ov |>
  dplyr::filter(more_ids %in% dcc_charite_joined_8_ds_added_years$data_id_merged) |> View()
  select(more_ids) |> 
  distinct() |> 
  nrow()

dcc_added_joined_2_rm_au_ov |>
  dplyr::filter(doi_dcc %in% dcc_charite_joined_8_ds_added_years$doi_dcc) |>
  select(more_ids) |> 
  distinct() |> 
  nrow()
