library(fs)
library(here)
library(stringr)

root <- here()

excluded_names <- c(
  "archive", ".Rproj.user", ".Rhistory", ".RData", ".Ruserdata",
  ".ipynb_checkpoints", ".quarto", "_output", ".DS_Store", 
  "notebooks/.DS_Store", ".git", "figure-ipynb", "index_files", "sankey_interactive_plot_2_files"
)

# Check if any part of a path matches excluded names
is_excluded_path <- function(path) {
  parts <- path_split(path)[[1]]
  any(tolower(parts) %in% tolower(excluded_names))
}

# Count only direct files inside a folder (not recursive)
count_files_in_folder <- function(folder_path) {
  files <- dir_ls(folder_path, type = "file", recurse = FALSE, all = TRUE)
  length(files)
}

# Recursive tree printer: print count only if > 0
print_tree <- function(path, indent = "") {
  folders <- dir_ls(path, type = "directory", all = TRUE, recurse = FALSE)
  
  for (folder in folders) {
    rel_path <- path_rel(folder, start = root)
    if (is_excluded_path(rel_path)) next
    
    name <- path_file(folder)
    file_count <- count_files_in_folder(folder)
    
    suffix <- dplyr::case_when(
      file_count > 0 ~ paste0(" (", file_count, ")"),
      .default = ""
    )
    
    cat(indent, "├── ", name, suffix, "\n", sep = "")
    
    print_tree(folder, indent = paste0(indent, "│   "))
  }
}

# Start printing from root
cat(path_file(root), "\n")
print_tree(root)
