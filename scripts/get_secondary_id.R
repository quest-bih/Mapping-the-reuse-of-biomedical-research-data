# This script gets secondary ids from EBI (ena) repository

library(httr)
library(jsonlite)
library(readr)
library(dplyr)

# load csv with primary accesion numbers
accessions <- read.csv(
  file.path(here("data",
                 "verification",
                 "metadata all",
                 "datasets_metadata_master_updated",
                 "tables to fill",
                 "no_secondary_id.csv")),
  header = TRUE)

# Define a function to get the secondary study accession for one PRJ*
get_secondary_study_accession <- function(acc) {
  url <- "https://www.ebi.ac.uk/ena/portal/api/search"
  query_params <- list(
    result = "study",
    query = paste0("accession=", acc),
    fields = "secondary_study_accession",
    format = "json"
  )
  
  response <- GET(url, query = query_params)
  
  if (status_code(response) == 200) {
    json_text <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(json_text)
    
    if (length(json_data) > 0 && !is.null(json_data$secondary_study_accession[1])) {
      return(tolower(json_data$secondary_study_accession[1]))
    }
  }
  
  return(NA)
}

# Apply the function to each accession in the dataframe
accessions <- accessions |>
  mutate(secondary_study_accession = sapply(data_id_merged, get_secondary_study_accession)) |> 
  select(data_id_merged, secondary_study_accession)

# save 
write_csv_cr(
  accessions,
  file = file.path(
    here("data",
         "verification",
         "metadata all",
         "datasets_metadata_master_updated",
         "tables to fill",
         "secondary_id_filled.csv")
  ),
  row.names = FALSE
)