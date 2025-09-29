# 0. Setup ----------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, htmlwidgets, glmmTMB, lsr, car)

# wrappers for save. write.csv() and write_xlsx with automatic directory creation

save_cr <- function(..., file) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  save(..., file = file)
} # wrapper for save() with automatic directory creation

write_csv_cr <- function(x, file, ...) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  write.csv(x, file = file, ...)
} 

write_xlsx_cr <- function(x, file, ...) {
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  writexl::write_xlsx(x, path = file, ...)
}

# Function to write updated file to next version

metadata_update <- function(obj) {
  # Get object name as a string
  obj_name <- deparse(substitute(obj))
  
  # Extract suffix (e.g., "032" from datasets_metadata_master_updated_032)
  suffix <- sub(".*_(\\d+)$", "\\1", obj_name)
  
  # Define base directory
  base_dir <- here("data", "verification", "metadata all", "datasets_metadata_master_updated")
  
  # Paths
  rda_path <- file.path(base_dir, "rda", paste0("datasets_metadata_master_updated_", suffix, ".RData"))
  csv_path <- file.path(base_dir, "csv", paste0("datasets_metadata_master_updated_", suffix, ".csv"))
  xlsx_path <- file.path(base_dir, "xlsx", paste0("datasets_metadata_master_updated_", suffix, ".xlsx"))
  
  # Save in all formats
  save_cr(list = obj_name, file = rda_path)
  write_csv_cr(obj, file = csv_path, row.names = FALSE)
  write_xlsx_cr(obj, file = xlsx_path)
}

# fucntion to load latest metadata version

load_latest_metadata_update <- function() {
  # Define target directory using here()
  rda_dir <- here("data", "verification", "metadata all", "datasets_metadata_master_updated", "rda")
  
  # List all .RData files
  files <- list.files(rda_dir, pattern = "\\.RData$", full.names = TRUE)
  
  # Extract numeric suffixes from filenames
  suffixes <- sub(".*_([0-9]+)\\.RData$", "\\1", files)
  suffixes_num <- as.integer(suffixes)
  
  # Find file with highest suffix
  max_index <- which.max(suffixes_num)
  latest_file <- files[max_index]
  
  # Load the file
  loaded_vars <- load(latest_file, envir = .GlobalEnv)
  
  # Extract and print the base file name without extension
  file_base <- tools::file_path_sans_ext(basename(latest_file))
  current_suffix <- sub(".*_(\\d+)$", "\\1", file_base)
  next_suffix <- sprintf("%03d", as.integer(current_suffix) + 1)
  next_file_base <- sub("_(\\d+)$", paste0("_", next_suffix), file_base)
  message("✅ Loaded: ", file_base, "\nNext file version: ", next_file_base)
  
  # Return the object invisibly
  invisible(get(loaded_vars[1], envir = .GlobalEnv))
}

# 1. matched vs non matched -----------------------------------------------

# load latest master file
load_latest_metadata_update() 

# get only relevant cases (matched + sample of 200 non-matched)

data_for_glm <- datasets_metadata_master_updated_018 |> 
  dplyr::filter(!is.na(covid_related)) |> 
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id,
         dataset_for_matching,
         in_dcc,
         das_for_analysis,
         human_data,
         covid_related,
         license_for_analysis) |> 
  mutate(dataset = coalesce(detected_id, dataset_for_matching)) |> 
  select(-c(detected_id, dataset_for_matching)) |> 
  dplyr::filter(!is.na(das_for_analysis)) |> 
  distinct() |> 
  mutate(in_dcc = as.factor(in_dcc))


model <- glm(in_dcc ~ human_data + covid_related + license_for_analysis + das_for_analysis,
             data = data_for_glm,
             family = "binomial")

glm_null <- glm(in_dcc ~ 1, data = data_for_glm, family = "binomial") # defien a null model
anova_result <- anova(glm_null, model, test = "Chisq") # overall model signifiance

summary_model <- summary(model)

# check for multicollinearity
vif(model) # none are > 5-10

odds_ratios <- round(exp(coef(model)), 2) # get how lokely is a human/covid/licensed/das dataset to be reused

# 2.datasets age-citations relationship -----------------------------------

# load data file
load(here("data", "tables_for_plots", "ds_age_cit_cor_prep.RData")) # fig-age-citation

# model

model_nb <- glmmTMB(
  n_citations ~ ds_age_when_cited + (1 | detected_in_dcc),
  data = ds_age_cit_cor_prep,
  family = nbinom2
)

summary(model_nb)