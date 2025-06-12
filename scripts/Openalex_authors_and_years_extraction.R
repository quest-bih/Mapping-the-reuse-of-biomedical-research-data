if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               openalexR,
               rcrossref,
               tcltk)

selected_file <- tclvalue(tkgetOpenFile(title = "Please select a CSV file a \"doi\" column"))

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


# Charite -----------------------------------------------------------------

# List of dataframes

dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "charite_dois_metadata_20052025.RData"))

save(final_results, file = save_path)

# DCC ---------------------------------------------------------------------

# after running once, I found problematic according to console output:
df <- df |> 
  dplyr::filter(!doi %in% c("10.17605/osf.io/95ayu",
                           "10.17605/osf.io/e8h3q",
                           "10.17605/osf.io/gs2t5",
                           "10.17605/osf.io/gupbf",
                           "10.17605/osf.io/3jk45"))

# Set a "list of dataframes"

df1 <- df |> slice(1:150)
df2 <- df |> slice(151:300)
df3 <- df |> slice(301:450)
df4 <- df |> slice(451:600)
df5 <- df |> slice(601:750)
df6 <- df |> slice(751:900)
df7 <- df |> slice(901:928)

dfs <- list(df1, df2, df3, df4, df5, df6, df7)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "dcc_dois_metadata_20052025.RData"))

save(final_results, file = save_path)


# added -------------------------------------------------------------------

# List of dataframes

dfs <- list(df)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "added_dois_metadata_v1.RData"))

save(final_results, file = save_path)


# DCC for added -----------------------------------------------------------

# List of dataframes

df1 <- df |> slice(1:150)
df2 <- df |> slice(151:224)

dfs <- list(df1, df2)

# Apply function to each dataframe and bind results together
final_results <- map_dfr(dfs, process_dataframe)

# Save
save_path <- file.path(here("data",
                            "verification",
                            "metadata matched",
                            "added_dois_metadata_v1.RData"))

save(final_results, file = save_path)

#### EXPERIMENTAL: get list of DFs (chunks of 150 obs) into a list:

# # Vector of DOIs to remove
# dois_to_remove <- c(
#   "10.17605/osf.io/95ayu",
#   "10.17605/osf.io/e8h3q",
#   "10.17605/osf.io/gs2t5",
#   "10.17605/osf.io/gupbf"
# )
# 
# # Filter and split into chunks of 150
# dfs <- df |>
#   dplyr::filter(!doi %in% dois_to_remove) |>
#   dplyr::mutate(chunk = (row_number() - 1) %/% 150 + 1) |>
#   dplyr::group_split(chunk, .keep = FALSE)
