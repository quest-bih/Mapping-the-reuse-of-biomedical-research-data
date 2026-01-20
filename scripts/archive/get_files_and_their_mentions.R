library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(writexl)

root_path <- "C:/AVIHAY/git/DCC-v3"  # update if needed

# 1. Data files of interest
data_files <- list.files(
  path = root_path,
  pattern = "\\.(RData|rda|xlsx|csv)$",
  recursive = TRUE,
  full.names = TRUE
)

data_df <- tibble(
  file_path = data_files,
  file_name = basename(data_files),
  is_file_in_archive = str_detect(file_path, regex("archive", ignore_case = TRUE))
)

# 2. Script files (.R / .qmd)
script_files <- list.files(
  path = root_path,
  pattern = "\\.(R|qmd)$",
  recursive = TRUE,
  full.names = TRUE
)

# 3. Scan scripts for data file names
results <- script_files |>
  map_dfr(function(script_path) {
    
    lines <- read_lines(script_path)
    script_text <- str_flatten(lines, collapse = "\n")
    
    data_df |>
      dplyr::filter(str_detect(script_text, fixed(file_name))) |>
      dplyr::mutate(
        script_file_name = basename(script_path),
        script_path = script_path,
        is_script_in_archive = str_detect(script_path, regex("archive", ignore_case = TRUE))
      )
  }) |>
  dplyr::select(
    file_path,
    file_name,
    script_file_name,
    script_path,
    is_file_in_archive,
    is_script_in_archive
  ) |>
  distinct()

# 4. Write output
write_xlsx(
  results,
  file.path(root_path, "results.xlsx")
)
