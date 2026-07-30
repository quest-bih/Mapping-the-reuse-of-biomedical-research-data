# This script looks for a string in all of the qmd / R files in a root folder and all of its subfolders.

library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(writexl)

root_path <- "C:/AVIHAY/git/DCC-v3"  # <- update this if you want to change the root folder path 

files <- list.files(
  path = root_path, 
  pattern = "\\.(qmd|R)$", 
  recursive = TRUE, 
  full.names = TRUE
)

results <- files |> 
  map_dfr(function(file) {
    lines <- read_lines(file)
    matches <- which(str_detect(lines, fixed("v10"))) # <- change the string you want to look for here
    
    if (length(matches) == 0) return(NULL)
    
    tibble(
      file_path = file,
      line_number = matches,
      line_text = lines[matches]
    )
  })
