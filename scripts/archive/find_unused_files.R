library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(fs)
library(writexl)

root_path <- "C:/AVIHAY/git/DCC-v3"  # update if needed

# 1. All files in root + subfolders
all_files <- fs::dir_ls(
  path = root_path,
  recurse = TRUE,
  type = "file"
)

# 2. File metadata
files_df <- tibble(
  file_path = all_files,
  file_name = fs::path_file(all_files),
  file_ext  = fs::path_ext(all_files)
)

# 3. All R and qmd files
code_files <- files_df |>
  dplyr::filter(str_detect(file_ext, "^(R|qmd)$")) |>
  dplyr::pull(file_path)

# 4. Read all code lines
code_text <- code_files |>
  purrr::map(read_lines) |>
  purrr::flatten_chr() |>
  stringr::str_flatten(collapse = "\n")

# 5. Files NOT referenced in any R / qmd file
unused_files <- files_df |>
  dplyr::filter(!str_detect(file_ext, "^(R|qmd)$")) |>
  dplyr::filter(!str_detect(code_text, fixed(file_name)))

# 6. Write output to Excel in root folder
writexl::write_xlsx(
  unused_files,
  fs::path(root_path, "unused_files.xlsx")
)
