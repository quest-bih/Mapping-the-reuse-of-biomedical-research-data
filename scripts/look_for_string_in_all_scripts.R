library(fs)
library(purrr)
library(stringr)
library(tibble)
library(dplyr)

find_files_with_text <- function(root = ".", pattern = "ds_age_cit_cor_pre") { # change string here
  fs::dir_ls(path = root, recurse = TRUE, type = "file", regexp = "\\.(R|qmd)$") |>
    tibble::tibble(path = _) |>
    dplyr::mutate(
      content = purrr::map(path, ~ tryCatch(readLines(.x, warn = FALSE), error = function(e) character())),
      has_match = purrr::map_lgl(content, ~ stringr::str_detect(.x, fixed(pattern)) |> any())
    ) |>
    dplyr::filter(has_match) |>
    dplyr::pull(path)
}


find_files_with_text("C:/AVIHAY/git/DCC-v3") # change target folder(s) here
