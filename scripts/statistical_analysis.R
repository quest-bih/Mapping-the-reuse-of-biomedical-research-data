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

data_for_glm <- datasets_metadata_master_updated_020 |> 
  dplyr::filter(!is.na(covid_related)) |> 
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id,
         dataset_for_matching,
         in_dcc,
         das_for_analysis,
         human_data,
         covid_related,
         license_for_analysis,
         is_detected_id_doi) |> 
  mutate(dataset = coalesce(detected_id, dataset_for_matching)) |> 
  select(-c(detected_id, dataset_for_matching)) |> 
  dplyr::filter(!is.na(das_for_analysis)) |> 
  distinct() |> 
  mutate(in_dcc = as.factor(in_dcc))

data_for_glm |>dplyr::filter(if_any(everything(), is.na)) # check for NAs

model <- glm(in_dcc ~ human_data + covid_related + license_for_analysis + das_for_analysis + is_detected_id_doi,
             data = data_for_glm,
             family = "binomial")

glm_null <- glm(in_dcc ~ 1, data = data_for_glm, family = "binomial") # define a null model
anova_result <- anova(glm_null, model, test = "Chisq") # overall model significance

summary_model <- summary(model)

# Extract raw p-values (as before)
raw_p <- summary_model$coefficients[, "Pr(>|z|)"]
raw_p <- raw_p[!str_detect(names(raw_p), "Intercept")]

# Apply correction (e.g., Benjamini-Hochberg / FDR)
corrected_p <- p.adjust(raw_p, method = "fdr")

# Rename like before
names(corrected_p) <- names(corrected_p) |>
  str_remove("TRUE") |>
  (\(x) paste0("p_adj_", x))()


# check for multicollinearity
vif(model) # none are > 5-10

odds_ratios <- round(exp(coef(model)), 2) # get how likely is a human/covid/licensed/das dataset to be reused

# 2.datasets age-citations relationship -----------------------------------

# Prepare table for analysis

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_8_dedup.RData")) # load dedup final results

ids_and_years <- dcc_detected_ids_all_sources_8_dedup |> 
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id, charite_id_year, publication_year_dcc) |> 
  mutate(age = as.numeric(publication_year_dcc) - as.numeric(charite_id_year)) |> 
  dplyr::filter(age >= 0 & age <= 3) |> 
  group_by(detected_id, age) |> 
  summarise(number_of_citations_in_age = n())

# save as RData
save_cr(ids_and_years, file = file.path(here("data",
                                             "inputs_for_quick_render",
                                             "ids_and_years.RData")))

# model

model_nb <- glmmTMB(
  number_of_citations_in_age ~ age + (1 | detected_id),
  data = ids_and_years,
  family = nbinom2
)

summary(model_nb)

