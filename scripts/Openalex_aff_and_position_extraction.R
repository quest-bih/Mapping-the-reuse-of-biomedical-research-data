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

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

final_results <- final_results |> 
  mutate(doi = str_remove(doi, "^https://doi.org/")) |> # remove prefix
  mutate(doi = tolower(doi))

# Save
save_path <- file.path(here("data",
                            "verification",
                            "aff_and_position",
                            "final_results.RData"))

save(final_results, file = save_path)

# Unnest to get authors, affiliation and position for each Charité DOI

authors_flat <- final_results |>
  select(doi, author) |>
  dplyr::filter(!is.na(author), map_lgl(author, is.data.frame)) |>
  mutate(author = map2(author, doi, ~ mutate(.x, doi = .y))) |>
  pull(author) |>                # extract the list of author data.frames
  list_rbind() |>                # now safely bind all inner tables
  select(doi, au_display_name, author_position, institution_display_name)

# get unresolved dois
dois_not_resolved_flat_to_add <- dcc_detected_ids_all_sources_8_dedup |> 
  select(doi_lc, authors_charite) |> 
  dplyr::filter(!doi_lc %in% final_results$doi) |> 
  distinct() |> 
  rename(doi = doi_lc,
         au_display_name = authors_charite) |> # get unresolved dois
  separate_rows(au_display_name, sep = ";") |>
  mutate(au_display_name = str_trim(au_display_name))

# bind them to authors_flat

charite_dois_aff_and_position_to_fill <- authors_flat |> 
  bind_rows(dois_not_resolved_flat_to_add) |> 
  distinct()

t <- charite_dois_aff_and_position_to_fill |>
  group_by(doi) |>
  dplyr::filter(n() < 3000) |>
  ungroup()

t |> dplyr::filter(is.na(institution_display_name)) |> select(doi) |> distinct() |> nrow()
t |> dplyr::filter(is.na(author_position)) |> select(doi) |> distinct() |> nrow()

t |> dplyr::filter(is.na(institution_display_name)) |> select(doi, institution_display_name) |> group_by(doi) |> nrow()
t |> dplyr::filter(is.na(author_position)) |> select(doi, institution_display_name) |> group_by(doi) |> nrow()


# 5. Complete info manually -----------------------------------------------

charite_dois_aff_and_position_filled <- charite_dois_aff_and_position_to_fill |> 
  mutate(
    institution_display_name = 
      case_when(
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        
        .default = institution_display_name),
    
    author_position =
      case_when(
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
        doi == "" & au_display_name == "" ~ "",
      )
  )
 

###

# Archive:
# # get a specific DOI's nested "author" table
# t <- final_results |>
#   dplyr::filter(doi == "10.1002/jimd.12341") |> # change DOI
#   pull(author)
# View(t[[1]])
  
# t1 <- charite_dois_aff_and_position_to_fill |>
#   group_by(doi) |>
#   dplyr::filter(n() > 3000) |>
#   ungroup() |> 
#   select(doi) |> 
#   distinct()
