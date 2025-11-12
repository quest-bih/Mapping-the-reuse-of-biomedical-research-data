# Load packages
library(fs)
library(here)

# Get the project root
root <- here()

fs::dir_tree(path = here(), recurse = TRUE, max_depth = 2, type = "directory")

