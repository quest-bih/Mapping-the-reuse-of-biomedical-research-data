# Load required libraries
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(fs)
library(writexl)

# Root folder
root <- "C:/AVIHAY/git/DCC-v3"

# Get all .RData files
rdata_files <- dir_ls(root, recurse = TRUE, regexp = "\\.RData$")

# Function to check for unwanted string values in character columns
check_strings <- function(df, file_path, object_name) {
  df |>
    dplyr::select(where(is.character)) |>
    purrr::imap_dfr(function(col, col_name) {
      tibble(
        file = file_path,
        object = object_name,
        column = col_name,
        has_NA_str = any(col == "NA", na.rm = TRUE),
        has_NULL_str = any(col == "NULL", na.rm = TRUE),
        has_empty_str = any(col == "", na.rm = TRUE)
      ) |>
        dplyr::filter(has_NA_str | has_NULL_str | has_empty_str)
    })
}

# Main routine with size check
results <- purrr::map_dfr(rdata_files, function(file) {
  if (file_info(file)$size == 0) return(tibble())  # skip empty files
  
  env <- new.env()
  load(file, envir = env)
  
  purrr::map_dfr(ls(env), function(obj_name) {
    obj <- get(obj_name, envir = env)
    if (inherits(obj, "data.frame")) {
      check_strings(obj, file_path = file, object_name = obj_name)
    } else {
      tibble()
    }
  })
})

# save
save_cr(results, file = file.path("C:/AVIHAY/git/DCC-v3/data/verification/check_na_null_blank", "results.RData"))



