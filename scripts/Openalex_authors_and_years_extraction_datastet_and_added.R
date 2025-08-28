if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               openalexR,
               rcrossref,
               tcltk)

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
    full_join(results_unnested, by = "doi") |> 
    select(doi, publication_year, authors)
}


# ds_and_added -----------------------------------------------------------------

# List of dataframes

dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_and_added",
                            "ds_and_added_dois_metadata_v1.RData"))

save(final_results, file = save_path)

# DCC ---------------------------------------------------------------------


dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_and_added",
                            "dcc_of_ds_and_added_dois_metadata_v1.RData"))

save(final_results, file = save_path)


# find out what's left ----------------------------------------------------

# ds_and_added

# load csv
selected_file <- tclvalue(tkgetOpenFile(title = "Please select a CSV file with a \"doi\" column"))
df <- read.csv(selected_file) |> rename(doi = 1)

#check
for_check <- final_results |>
  mutate(doi = str_remove(doi, "^https://doi\\.org/"))

dois_left <- df |> dplyr::filter(!doi %in% for_check$doi)

# add manually the one that's left
final_results <- final_results |> 
  bind_rows(
    tibble(
      doi = "https://doi.org/10.3324/haematol.2020.276048",
      publication_year = 2022,
      authors = "Sarah Grasedieck;Ariene Cabantog;Liam MacPhee;Junbum Im;Christoph Ruess;Burcu Demir;Nadine Sperb;Frank G. Ruecker;Konstanze Doehner;Tobias Herold;Jonathan R. Pollack;Lars Bullinger;Arefeh Rouhi;Florian Kuchenbauer"
    )
  )

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_and_added",
                            "ds_and_added_dois_metadata_v2.RData"))

save(final_results, file = save_path)

# dcc

# load csv
selected_file <- tclvalue(tkgetOpenFile(title = "Please select a CSV file with a \"doi\" column"))
df <- read.csv(selected_file) |> rename(doi = 1)

#check
for_check <- final_results |>
  mutate(doi = str_remove(doi, "^https://doi\\.org/"))

# get dois

  # 1. Get DOIs from for_check where authors is NA
  missing_authors <- for_check |> 
    dplyr::filter(is.na(authors)) |> 
    select(doi)
  
  # 2. Get DOIs that are in df but not in for_check
  missing_in_for_check <- df |> 
    dplyr::filter(!doi %in% for_check$doi)
  
  # 3. Append
  dois_left <- bind_rows(missing_authors, missing_in_for_check)

dcc_ds_and_added_joined_2_rm_self |> 
  dplyr::filter(doi_dcc %in% dois_left$doi) |> 
  dplyr::filter(doi_dcc != "none") |> 
  group_by(dataset_for_matching) |> summarise(n=n()) |>
  View()


  
# add manually




final_results <- final_results |> 
  bind_rows(
    tibble(
      doi = c(
        "https://doi.org/10.3324/haematol.2020.276048",
        "",
        "",
        "",
      publication_year = "2022",
      authors = "Sarah Grasedieck;Ariene Cabantog;Liam MacPhee;Junbum Im;Christoph Ruess;Burcu Demir;Nadine Sperb;Frank G. Ruecker;Konstanze Doehner;Tobias Herold;Jonathan R. Pollack;Lars Bullinger;Arefeh Rouhi;Florian Kuchenbauer"
    )
  ))




# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_and_added",
                            "dcc_of_ds_and_added_dois_metadata_v2.RData"))

save(final_results, file = save_path)


# get authors position ----------------------------------------------------

# I first commented this line in the function: |> select(doi, publication_year, authors)
# Then I ran it to get "final_results"
# And below I'll add the info that I need

# first save all metadata
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_and_added",
                            "dcc_of_ds_and_added_dois_all_metadata_v1.RData"))

save(final_results, file = save_path)

# get authors position

authors_position_to_join <- final_results |>
  mutate(
    doi = str_remove(doi, regex("^https://doi\\.org/")),
    
    position_of_charite_authors = map_chr(author, ~
                                            .x |>
                                            dplyr::mutate(
                                              match_charite = str_detect(institution_display_name, fixed("Charité")) |
                                                str_detect(au_affiliation_raw, fixed("Charité"))
                                            ) |>
                                            dplyr::filter(match_charite) |>
                                            pull(author_position) |>
                                            unique() |>
                                            str_c(collapse = ";")
    ),
    
    position_of_charite_authors = case_when(
      position_of_charite_authors == "" ~ "No Charité Author Detected",
      position_of_charite_authors == "middle" ~ "middle",
      .default = position_of_charite_authors
    )
  ) |>
  select(doi, position_of_charite_authors)

# Note! 

# save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_and_added",
                            "dcc_of_ds_and_added_dois_authors_position_to_join_v1.RData"))

save(authors_position_to_join, file = save_path)


# DCC 25.08.2025 ---------------------------------------------------------------------


dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "dcc_doi_metadata_v2.RData"))

save(final_results, file = save_path)


# Charite 25.08.2025 ---------------------------------------------------------------------


dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "charite_doi_metadata_v2.RData"))

save(final_results, file = save_path)


# ds_in_matched_numbat check 27.08.2025 -----------------------------------

dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "ds_in_matched_numbat.RData"))

save(final_results, file = save_path)
