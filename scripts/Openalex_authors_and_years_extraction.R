# This script gets a csv with a "doi" column in it, and it sends Openalex API requests to extract metadata about them.

# Note: run the first section separately first ("1. Set up, file selection and Openalex API requests").
# If for some reason not all dois are processed, the script will let you try again with the remaining that caused the error.

# When you're satisfied with the number of dois processed, run the rest of the script.
# The output will be saved as an Rdata object and as a csv file in the location of the original input file.

# 1. Set up, file selection and Openalex API requests ---------------------

if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               openalexR,
               rcrossref,
               tcltk)

selected_file <- tclvalue(tkgetOpenFile(title = "Please select a CSV file a \"doi\" column"))

# Load the selected file into a data frame
df <- read.csv(selected_file)

df1 <- df |> slice(1:150) # done
df2 <- df |> slice(151:300) # done
df3 <- df |> slice(301:450) # done
df4 <- df |> slice(451:600) # done
df5 <- df |> slice(601:750) # done
df6 <- df |> slice(751:900) # done
df7 <- df |> slice(901:1050) # done
df8 <- df |> slice(1051:1200) # done
df9 <- df |> slice(1201:1322) # done



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

UnnestDataFrame <- function(x, pid) {
  valid_x <- x[!is.na(x) & sapply(x, is.data.frame)]
  
  if (length(valid_x) == 0) {
    return(data.frame())
  }
  
  df.x <- bind_rows(valid_x)
  tcx <- NestedDataFrameSize(valid_x)
  df.x$PID <- rep(pid, times = tcx)
  
  df.x
}

# Call the function
results <- openalex_extract(df1) # done

results <- openalex_extract(df2) # done

results <- openalex_extract(df3) # done

results <- openalex_extract(df4) # done

results <- openalex_extract(df5) # done

results <- openalex_extract(df6) # done

results <- openalex_extract(df7) # done

  df_8_1 <- df8 |> slice(1:35)
  
  results <- openalex_extract(df_8_1) # done
  
  df_8_1_1 <- df8 |> slice(36:40)
  
  results <- openalex_extract(df_8_1_1) # done
  
  df_8_1_1_2 <- df8 |> slice(41:46)
  
  results <- openalex_extract(df_8_1_1_2) # done
  
  df_8_1_1_3 <- df8 |> slice(47:48)
  
  results <- openalex_extract(df_8_1_1_3) # done
  
  df_8_1_1_4 <- df8 |> slice(50:50)
  
  results <- openalex_extract(df_8_1_1_4) # done
  
  df_8_1_2 <- df8 |> slice(51:70)
  
  results <- openalex_extract(df_8_1_2) # done
  
  df_8_2 <- df8 |> slice(71:150)
  
  results <- openalex_extract(df_8_2) # done

df_8_all_but_49 <- df8 |> slice(-49) 

results <- openalex_extract(df_8_all_but_49) # done

results <- openalex_extract(df9) # done


# 2. Get authors and publication years ------------------------------------

results_unnested <- UnnestDataFrame(results, names(results))

results_extracted_authors <- UnnestDataFrame(results_unnested$author, results_unnested$doi)
  
doi_authors_and_years <- results_extracted_authors |> 
  rename(doi = PID,
         authors = au_display_name) |> 
  group_by(doi) |> 
  summarise(authors = str_c(authors, collapse = ";"), .groups = "drop") |> 
  full_join(results_unnested, by = "doi") |> 
  select(doi, publication_year, authors)


re_1 <- doi_authors_and_years # done

re_2 <- doi_authors_and_years # done

re_3 <- doi_authors_and_years # done

re_4 <- doi_authors_and_years # done

re_5 <- doi_authors_and_years # done

re_6 <- doi_authors_and_years # done

re_7 <- doi_authors_and_years # done

  re_8_1 <- doi_authors_and_years # done
  
  re_8_1_1 <- doi_authors_and_years # done
  
  re_8_1_1_2 <- doi_authors_and_years # done
  
  re_8_1_1_3 <- doi_authors_and_years # done
  
  re_8_1_1_4 <- doi_authors_and_years # done
  
  re_8_1_2 <- doi_authors_and_years # done
  
  re_8_2 <- doi_authors_and_years # done

re_8_all_but_49 <- doi_authors_and_years

re_9 <- doi_authors_and_years # done

dcc_doi_authors_and_years <- re_1 |> 
  bind_rows(
    re_2,
    re_3,
    re_4,
    re_5,
    re_6,
    re_7,
    re_8_all_but_49,
    re_9)

# Save results
  
write.csv(dcc_doi_authors_and_years, "dcc_doi_authors_and_years.csv", row.names = FALSE)
