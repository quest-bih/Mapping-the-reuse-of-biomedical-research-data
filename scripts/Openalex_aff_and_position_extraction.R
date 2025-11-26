# This script uses openalex to extract affiliation and position of Charité authors
# and joins it to the final result table to get a table with Charité DOIs and the following information:
# authors, affiliation, position, source_charite, identifier, referncing DOI (doi_dcc)

# 1. Setup ----------------------------------------------------------------

Sys.setenv(LANG = "EN")  # Set environment language to English

if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               openalexR,
               rcrossref,
               tcltk,
               DT, patchwork, RColorBrewer, here, networkD3, readxl, lubridate, stringi)

save_cr <- function(..., file) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  save(..., file = file)
} # wrapper for save() with automatic directory creation

write_csv_cr <- function(x, file, ...) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  write.csv(x, file = file, ...)
} # wrapper for write.csv() with automatic directory creation

# 2. Get Charité DOIs list ------------------------------------------------

# load final result table
load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_8_dedup.RData"))

# extract Charité DOIs column
charite_dois <- dcc_detected_ids_all_sources_8_dedup |> 
  select(doi_charite) |> 
  distinct()

# save
write_csv_cr(charite_dois, file = file.path(here("data",
                                                 "verification",
                                                 "aff_and_position",
                                                 "charite_dois.csv")),
             row.names = FALSE)

# 3. Get info from Openalex -----------------------------------------------

selected_file <- tclvalue(tkgetOpenFile(title = "Please select a CSV file with a \"doi\" column"))

# Load the selected file into a data frame
df <- read.csv(selected_file) |> 
  rename(doi = 1) # rename column to doi if necessary 

# Function to extract OpenAlex metadata
openalex_extract <- function(df) {
  df$doi <- tolower(df$doi)
  dois <- as.vector(df$doi)
  dois <- unique(dois) # Get unique DOIs
  dois <- dois[dois != ""] # Remove blanks from vector
  dois <- paste0("doi:", dois) # Prepend "doi:" to each element in the vector
  dois <- sprintf("%s", dois)
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  # Initialize an empty vector to store DOIs that result in errors
  error_dois <- c()
  
  # Function to process DOIs
  process_dois <- function(dois_to_process) {
    local_error_dois <- c()
    for (doi in dois_to_process) {
      query <- tryCatch({
        # Make the API request
        oa_fetch(
          entity = "works",
          identifier = doi,
          verbose = TRUE
        )
      }, error = function(e) {
        # If an error occurs, print a message and add DOI to error list
        message(paste("Error fetching DOI:", doi))
        message("Error details:", e)
        local_error_dois <<- c(local_error_dois, doi)
        return(NULL)
      })
      
      # Save successful results
      if (!is.null(query)) {
        results_list[[doi]] <<- query # Use <<- to modify the parent scope
      }
    }
    return(local_error_dois)
  }
  
  # Initial processing
  error_dois <- process_dois(dois)
  
  # Retry loop for errors
  while (length(error_dois) > 0) {
    message("The following DOIs had errors:")
    print(error_dois)
    
    retry <- readline(prompt = "Would you like to retry processing them? (y/n): ")
    
    if (tolower(retry) == "y") {
      error_dois <- process_dois(error_dois)
    } else {
      break
    }
  }
  
  return(results_list) # Return the results list
}

NestedDataFrameSize <- function(x) {
  x <- lapply(x, function(y) if (is.data.frame(y)) nrow(y) else 0)
  x <- unlist(x)
  x
}

# UnnestDataFrame <- function(x, pid) {
#   valid_x <- x[!is.na(x) & sapply(x, is.data.frame)]
# 
#   if (length(valid_x) == 0) {
#     return(data.frame())
#   }
# 
#   df.x <- bind_rows(valid_x)
#   tcx <- NestedDataFrameSize(valid_x)
#   df.x$PID <- rep(pid, times = tcx)
# 
#   df.x
# }

UnnestDataFrame <- function(x, pid) {
  valid_x <- x[!is.na(x) & sapply(x, is.data.frame)]
  valid_pid <- pid[!is.na(x) & sapply(x, is.data.frame)] # <-- Filter pid too
  
  if (length(valid_x) == 0) {
    return(data.frame())
  }
  
  df.x <- bind_rows(valid_x)
  tcx <- NestedDataFrameSize(valid_x)
  df.x$PID <- rep(valid_pid, times = tcx) # <-- Use filtered pid
  df.x
}


# Define a function to process each dataframe
process_dataframe <- function(df) {
  # Extract results
  results <- openalex_extract(df)
  
  # Unnest results
  results_unnested <- UnnestDataFrame(results, names(results))
  results_extracted_authors <- UnnestDataFrame(results_unnested$author, results_unnested$doi)
  
  # Process the data
  results_extracted_authors |> 
    rename(doi = PID,
           authors = au_display_name) |> 
    group_by(doi) |> 
    summarise(authors = str_c(authors, collapse = ";"), .groups = "drop") |> 
    full_join(results_unnested, by = "doi") 
    # select(doi, publication_year, authors)
}

# create a list of dataframes
dfs <- list(df)

# The next section (applying the extraction function and saving the output)
# is commented out since it might give different results with each run.
# This is due to some DOIs not always having the correct format for Openalex to extract the information from
# which could cause different DOIs to not be extracted on different runs.
# Info of DOIs that weren't resolved or that Openalex didn't recognize a Charité affiliation in
# or that had any NAs as their affiliation / position values
# is being completed in the next section: "Get DOIs that need their info completed:"
# which is executed on the loaded "final_results.RData" file.

  # Apply function to each dataframe and bind results together
  # final_results <- map_dfr(dfs, process_dataframe)
  # 
  # final_results <- final_results |> 
  #   mutate(doi = str_remove(doi, "^https://doi.org/")) |> # remove prefix
  #   mutate(doi = tolower(doi))
  # 
  # # Save
  # save_path <- file.path(here("data",
  #                             "verification",
  #                             "aff_and_position",
  #                             "final_results.RData"))
  # 
  # save(final_results, file = save_path)

load(here("data", "verification", "aff_and_position", "final_results.RData"))

# Unnest to get authors, affiliation and position for each Charité DOI

authors_flat <- final_results |>
  select(doi, author) |>
  dplyr::filter(!is.na(author), map_lgl(author, is.data.frame)) |>
  mutate(author = map2(author, doi, ~ mutate(.x, doi = .y))) |>
  pull(author) |>                # extract the list of author data.frames
  list_rbind() |>                # now safely bind all inner tables
  select(doi, au_display_name, author_position, institution_display_name)

# Get DOIs that need their info completed:

dois_affiliation_and_position_to_complete <- authors_flat |>
  
  # 1. Get DOIs that Openalex didn't recognize a Charité affiliation in them because it wasn't the first institution
  group_by(doi) |>
  dplyr::filter(!any(stringr::str_detect(institution_display_name, "Charité"))) |>
  distinct(doi) |>
  
  # 2. Get DOIs that Openalex couldn't resolve
  bind_rows(
    dcc_detected_ids_all_sources_8_dedup |>
      distinct(doi_lc) |>
      dplyr::anti_join(authors_flat |> distinct(doi), by = c("doi_lc" = "doi")) |>
      rename(doi = doi_lc)
  ) |> 
  
  # 3. Get DOIs that have NAs in either the "position" or the "affiliation" columns
  bind_rows(
    authors_flat |>
      group_by(doi) |>
      dplyr::filter(
        case_when(
          any(is.na(author_position)) ~ TRUE,
          any(is.na(institution_display_name)) ~ TRUE,
          .default = FALSE
        )
      ) |>
      distinct(doi)
  ) |>
  distinct(doi)

# # Save to send to René for filling up the missing info
# 
# # rda
# save_cr(
#   dois_affiliation_and_position_to_complete,
#   file = here(
#     "data",
#     "verification",
#     "aff_and_position",
#     "dois_to_complete",
#     "dois_affiliation_and_position_to_complete.rda")
# )
# 
# # csv
# write_csv(
#   dois_affiliation_and_position_to_complete,
#   here(
#     "data",
#     "verification",
#     "aff_and_position",
#     "dois_to_complete",
#     "dois_affiliation_and_position_to_complete.csv")
# )

t <- dois_affiliation_and_position_to_complete |> 
  left_join(dcc_detected_ids_all_sources_8_dedup |> 
              select(doi_charite, doi_lc) |> 
              distinct(),
            by = c("doi" = "doi_lc"))

t |>
  dplyr::filter(stringr::str_detect(doi_charite, "[A-Z]")) |>
  pull(doi_charite)

# Load DOIs list with the full info that I got back from René

# Read sheet "Sheet1" from Excel file
dois_from_dimensions_raw <- read_excel(file.path(here("data",
                                                "verification",
                                                "aff_and_position",
                                                "dois_from_dimensions"),
                                                "Dimensions-Publication-2025-11-17_15-05-29 _to_load.xlsx"),
                                 sheet = "Sheet1")

# Get only relevant data

dois_from_dimensions_1_dois_au_aff <- dois_from_dimensions_raw |> 
  rename(
    doi = DOI,
    authors = Authors,
    au_raw_aff = `Authors (Raw Affiliation)`,
    corr_au = `Corresponding Authors`,
    au_aff = `Authors Affiliations`,
    org = `Research Organizations - standardized`) |> 
  select(doi, authors, au_raw_aff, corr_au, au_aff, org)

# Function to reshape data

extract_affiliations <- function(x) {
  if (is.na(x)) return(tibble(author = character(), affs = list()))
  
  x |>
    str_split("\\);\\s*") |>
    unlist() |>
    discard(~ .x == "") |>
    map(~ paste0(.x, ")")) |>
    map(~ {
      author <- str_extract(.x, "^[^\\(]+") |> str_trim()
      affs_raw <- str_match(.x, "\\((.*)\\)") |> pluck(2)
      affs_split <- if (!is.na(affs_raw)) str_split(affs_raw, "\\s*;\\s*")[[1]] else NA_character_
      tibble(author = author, affs = list(affs_split))
    }) |>
    bind_rows()
}

# Reshape data

df_long <- dois_from_dimensions_1_dois_au_aff |>
  rowwise() |>
  mutate(
    authors_df = list(extract_affiliations(authors)),
    raw_df     = list(extract_affiliations(au_raw_aff)),
    corr_df    = list(extract_affiliations(corr_au)),
    aff_df     = list(extract_affiliations(au_aff))
  ) |>
  ungroup() |>
  select(doi, authors_df, raw_df, corr_df, aff_df) |>
  pmap_dfr(function(doi, authors_df, raw_df, corr_df, aff_df) {
    full_join(authors_df, raw_df, by = "author", suffix = c("", "_raw")) |>
      full_join(corr_df, by = "author", suffix = c("", "_corr")) |>
      full_join(aff_df, by = "author", suffix = c("", "_aff")) |>
      mutate(doi = doi) |>
      mutate(
        max_len = pmap_int(list(affs, affs_raw, affs_corr, affs_aff),
                           ~ max(length(..1), length(..2), length(..3), length(..4), na.rm = TRUE))
      ) |>
      pmap_dfr(function(author, affs, affs_raw, affs_corr, affs_aff, doi, max_len) {
        tibble(
          doi = doi,
          authors = rep(author, max_len),
          au_raw_aff = affs_raw |> unlist() |> `length<-`(max_len) |> replace_na(NA_character_),
          corr_au    = affs_corr |> unlist() |> `length<-`(max_len) |> replace_na(NA_character_),
          au_aff     = affs_aff |> unlist() |> `length<-`(max_len) |> replace_na(NA_character_)
        )
      })
  }) |>
  relocate(doi, authors, au_raw_aff, corr_au, au_aff)

# Filter out the rows which have all of the authors
df_long_no_semicolon <- df_long |>
  dplyr::filter(!str_detect(authors, ";\\s"))

# Add position:

df_long_position <- df_long_no_semicolon |>
  group_by(doi) |>
  mutate(
    row_pos = row_number(),
    total = n(),
    raw_position = case_when(
      row_pos == 1 ~ "first",
      row_pos == total ~ "last",
      .default = "middle"
    )
  ) |>
  group_by(doi, authors) |>
  mutate(
    position = case_when(
      any(raw_position == "first") ~ "first",
      any(raw_position == "last") ~ "last",
      .default = "middle"
    )
  ) |>
  ungroup() |>
  select(-row_pos, -total, -raw_position)


# Add manually_added_aff column using case_when and manually fill "position" for these values as well
df_long_man_add_aff <- df_long_position |>
  mutate(
    manually_added_aff = case_when(
      doi == "10.1016/j.stem.2020.11.015" & authors == "Strzelecka, Paulina M." ~ "Charité",
      doi == "10.1182/bloodadvances.2022007714" & authors == "Haas, Simon" ~ "Charité",
      .default = NA_character_)) |> 
  mutate(
    position = case_when(
      doi == "10.1016/j.stem.2020.11.015" & authors == "Strzelecka, Paulina M." ~ "middle",
      doi == "10.1182/bloodadvances.2022007714" & authors == "Haas, Simon" ~ "middle",
      .default = position))

missing_authors <- tibble(
  doi = c(
    rep("10.1038/s41467-020-18367-y", 4),
    rep("10.1038/s41586-021-03767-x", 16),
    "10.1126/science.abo3627",
    "10.1242/dev.201228",
    rep("10.1016/j.bpsgos.2021.07.008", 16),
    rep("10.1038/s41467-020-20603-4", 10),
    "10.1038/s41586-022-04434-5",  
    "10.1038/s41586-022-05275-y",  
    "10.1038/s41588-023-01422-x"
  ),
  authors = c(
    # s41467-020-18367-y ### 1
    "Erk, Susanne", " Lett, Tristram A.", "Heinz, Andreas", "Walter, Henrik",
    
    # s41586-021-03767-x
    "Heidecker, Bettina", "Kurth, Florian", "Sander, Leif E.", "Mayer, Alena",
    "Braun, Alice", "Skurk, Carsten", "Thibeault, Charlotte", "Helbig, Elisa T.",
    "Kraft, Julia", "Lippert, Lena J.", "Suwalski, Phillip", "Ripke, Stephan",
    "Poller, Wolfgang", "Wang, Xiaomin", "Karadeniz, Zehra", "Landmesser, Ulf",
    
    # science.abo3627
    "von Bernuth, Horst",
    
    # dev.201228
    "Malte, Spielmann",
    
    # j.bpsgos.2021.07.008 ### 2
    "Ripke, Stephan", "Abdellaoui, Abdel", "Dolan, Conor V.", "Finucane, Hilary K.",
    "Kraft, Julia", "Mbarek, Hamdi", "Middeldorp, Christel M.", "Nivard, Michel G.",
    "Hottenga, Jouke-Jan", "Thompson, Wesley", "Wang, Yunpeng", "Weinsheimer, Shantel Marie",
    "Willemsen, Gonneke", "Boomsma, Dorret I.", "de Geus, E.J.C.", "Trubetskoy, Vassily",
    
    # s41467-020-20603-4
    "Capper, David",
    
    # s41586-022-04434-5 ### 3
    "Trubetskoy, Vassily", "Panagiotaropoulou, Georgia", "Awasthi, Swapnil", "Braun, Alice",
    "Kraft, Julia", "Skarabis, Nora", "Walter, Henrik", "Ripke, Stephan", "Hahn, Eric", "Ta, Thi Minh Tam",
    
    #s41586-022-05275-y
    "Langenberg, Claudia",
    
    #s41588-023-01422-x
    "Eckardt, Kai-Uwe"
  ),
  position = c(
    
    # s41467-020-18367-y ### 1
    rep("middle", 4),
    
    # s41586-021-03767-x
    rep("middle", 16),
    
    # science.abo3627
    "middle",
    
    # dev.201228
    "last",
    
    # j.bpsgos.2021.07.008 ### 2
    rep("middle", 16),
    
    # s41467-020-20603-4
    "middle",
    
    # s41586-022-04434-5  
    "first", rep("middle", 9),
    
    #s41586-022-05275-y
    "middle",
    
    #s41588-023-01422-x
    "middle"
  ),
  
  manually_added_aff = "Charité"
)

# Ensure same columns as df_long_man_add_aff by adding empty cols
missing_authors <- missing_authors |>
  mutate(
    au_raw_aff = NA_character_,
    corr_au = NA_character_,
    au_aff = NA_character_
  ) |>
  select(doi, authors, au_raw_aff, corr_au, au_aff, position, manually_added_aff)  # aligns column order

# Append
df_long_bind_missing_authors <- bind_rows(df_long_man_add_aff, missing_authors)

# Append one more manual case:
df_long_bind_1_manual <- df_long_bind_missing_authors |> 
  bind_rows(tibble(
    doi = "10.3201/eid2707.204660",
    authors = "Moreira-Soto, Andres",
    au_raw_aff = NA_character_,
    corr_au = NA_character_,
    au_aff = NA_character_,
    position = "middle",
    manually_added_aff = "Charité"))


# Check that everything has "Charité" in at least one column
df_long_bind_1_manual |>
  group_by(doi) |>
  summarise(
    has_charite = any(
      str_detect(coalesce(au_raw_aff, ""), "Charité") |
        str_detect(coalesce(corr_au, ""), "Charité") |
        str_detect(coalesce(au_aff, ""), "Charité") |
        str_detect(coalesce(manually_added_aff, ""), "Charité")
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(!has_charite) |>
  distinct(doi)


# Keep only rows where any relevant column has "Charité"

df_charite <- df_long_bind_1_manual |>
  dplyr::filter(if_any(c(au_raw_aff, corr_au, au_aff, manually_added_aff), ~ str_detect(., "Charité")))

# Append to the OpenAlex df

# 1. remove from authors_flat dois that are in df_charite

# make sure DOIs are lowercased
any(grepl("[A-Z]", authors_flat$doi))
any(grepl("[A-Z]", df_charite$doi))

# remove
author_flat_to_append_to <- authors_flat |> 
  dplyr::filter(!doi %in% df_charite$doi)

# make sure that "author_flat_to_append_to" and "df_charite have all DOIs together
author_flat_to_append_to |> select(doi) |> distinct() |> nrow() # 76
df_charite |> select(doi) |> distinct() |> nrow() # 62
dcc_detected_ids_all_sources_8_dedup |> select(doi_charite) |> mutate(doi_charite = tolower(doi_charite)) |> distinct() |> nrow() # 138

# 2. filter to have only rows that have "Charité" under "institution_display_name"col

author_flat_to_append_to_charite_only <- author_flat_to_append_to |> 
  dplyr::filter(str_detect(institution_display_name, fixed("Charité")))

author_flat_to_append_to_charite_only |> select(doi) |> distinct() |> nrow() # still has the 76 DOIs

# 3. append "author_flat_to_append_to_charite_only" to "df_charite" (Get only distinct doi, authors, position, since now it's only Charité affiliations anyway)

charite_authors_and_position <- author_flat_to_append_to_charite_only |> 
  rename(
    author = au_display_name,
    position = author_position) |> 
  select(-institution_display_name) |> 
  bind_rows(df_charite |> 
              select(doi, authors, position) |> 
              rename(author = authors))

charite_authors_and_position |> select(doi) |> distinct() |> nrow() # check: 138 is correct.

# Save
save_path <- file.path(here("data",
                            "verification",
                            "aff_and_position",
                            "charite_authors_and_position.RData"))

save(charite_authors_and_position, file = save_path)

# Add detected_id and doi_dcc from _8_dedup:

# 1. prepare "dcc_detected_ids_all_sources_8_dedup" for joining

dcc_detected_for_joining <- dcc_detected_ids_all_sources_8_dedup |> 
  select(doi_charite, detected_id, doi_dcc) |> 
  mutate(doi_charite = tolower(doi_charite)) |> 
  distinct() |> 
  rename(doi = doi_charite,
         dataset = detected_id,
         citing_doi = doi_dcc)

# 2. Join
charite_authors_and_position_w_citing_dois <- charite_authors_and_position |> 
  inner_join(dcc_detected_for_joining, by = "doi")

charite_authors_and_position_w_citing_dois |> dplyr::filter(if_any(everything(), is.na))

# Save
save_path <- file.path(here("data",
                            "verification",
                            "aff_and_position",
                            "charite_authors_and_position_w_citing_dois.RData"))

save(charite_authors_and_position_w_citing_dois, file = save_path) # RData

# csv
write_csv_cr(charite_authors_and_position_w_citing_dois, file = file.path(here(
  "data",
  "verification",
  "aff_and_position",
  "charite_authors_and_position_w_citing_dois.csv")),
  row.names = FALSE)


# Create a summary result table for sending automatic emails: authors name, authors family name, doi, id, position, n_citations

charite_authors_and_position_w_citing_dois_summary <- charite_authors_and_position_w_citing_dois |> 
  group_by(author, doi, dataset, position) |> 
  summarise(number_of_citations = n())

# Save
save_path <- file.path(here("data",
                            "verification",
                            "aff_and_position",
                            "charite_authors_and_position_w_citing_dois_summary.RData"))

save(charite_authors_and_position_w_citing_dois_summary, file = save_path) # RData

# csv
write_csv_cr(charite_authors_and_position_w_citing_dois_summary, file = file.path(here(
  "data",
  "verification",
  "aff_and_position",
  "charite_authors_and_position_w_citing_dois_summary.csv")),
  row.names = FALSE)

###

# Archive:
#
# # Get a specific DOI's nested "author" table:
#
# t <- final_results |>
#   dplyr::filter(doi == "10.1002/jimd.12341") |> # change DOI
#   pull(author)
# View(t[[1]])
#
# # Get df without the doi of >3000 authors:
#
# df_filtered <- df |>
#   group_by(doi) |>
#   dplyr::filter(n() > 3000) |>
#   ungroup() |> 
#   select(doi) |> 
#   distinct()
