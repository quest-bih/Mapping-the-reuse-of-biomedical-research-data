library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(writexl)

root_path <- "C:/AVIHAY/git/DCC-v3"  # <-- update this

files <- list.files(
  path = root_path, 
  pattern = "\\.(qmd|R)$", 
  recursive = TRUE, 
  full.names = TRUE
)

results <- files |> 
  map_dfr(function(file) {
    lines <- read_lines(file)
    matches <- which(str_detect(lines, fixed("datajournal_articles - analysis of citations v10.xlsx")))
    
    if (length(matches) == 0) return(NULL)
    
    tibble(
      file_path = file,
      line_number = matches,
      line_text = lines[matches]
    )
  })

write_xlsx(results, "summarise_n_equals_n_usage.xlsx")
