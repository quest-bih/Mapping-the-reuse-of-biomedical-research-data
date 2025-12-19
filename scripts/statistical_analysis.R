# 0. Setup ----------------------------------------------------------------
install.packages("future", dependencies = TRUE) # install

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, htmlwidgets, glmmTMB, lsr, car, broom,
               performance, insight)

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

# 1. χ²: matched vs non matched -----------------------------------------------

# load latest master file
load_latest_metadata_update() 

# get only relevant cases (matched + sample of 200 non-matched)

data_for_glm <- datasets_metadata_master_updated_021 |> 
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
  mutate(in_dcc = as.factor(in_dcc)) |> 
  dplyr::filter(dataset != "10.18112/openneuro.ds001226") # remove this overlapping case from Numbat cases, as it belongs to Data Articles

data_for_glm |>dplyr::filter(if_any(everything(), is.na)) # check for NAs

model <- glm(in_dcc ~ human_data + covid_related + license_for_analysis + das_for_analysis + is_detected_id_doi,
             data = data_for_glm,
             family = "binomial")

glm_null <- glm(in_dcc ~ 1, data = data_for_glm, family = "binomial") # define a null model
anova_result <- anova(glm_null, model, test = "Chisq") # overall model significance

# save as RData
save_cr(anova_result, file = file.path(here("data",
                                                 "tables_for_plots",
                                                 "anova_result.RData")))

summary_model_chi <- summary(model)

# save as RData
save_cr(summary_model_chi, file = file.path(here("data",
                                             "tables_for_plots",
                                             "summary_model_chi.RData")))

# Extract raw p-values (as before)
raw_p <- summary_model_chi$coefficients[, "Pr(>|z|)"]
raw_p <- raw_p[!str_detect(names(raw_p), "Intercept")]

# Apply correction (FDR)
corrected_p <- p.adjust(raw_p, method = "fdr")

# Rename like before
names(corrected_p) <- names(corrected_p) |>
  str_remove("TRUE") |>
  (\(x) paste0("p_adj_", x))()

# check for multicollinearity
vif(model) # none are > 5-10

odds_ratios <- round(exp(coef(model)), 2) # get how likely is a human/covid/licensed/das dataset to be reused

# Fixed Effects: Odds Ratios, 95% CI, and p-values
results_glm <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)

# Model Fit
fit_glm <- data.frame(
  AIC = AIC(model)
)

# save as RData
save_cr(fit_glm, file = file.path(here("data",
                                           "tables_for_plots",
                                           "fit_glm.RData")))

# save as RData
save_cr(odds_ratios, file = file.path(here("data",
                                                 "tables_for_plots",
                                                 "odds_ratios.RData")))


# prepare table for plot

clean_labels <- c(
  "covid_relatedTRUE"        = "Covid Related",
  "human_dataTRUE"           = "Human Data",
  "license_for_analysisTRUE" = "CC Licensed",
  "das_for_analysisTRUE"     = "In DAS",
  "is_detected_id_doiTRUE"   = "DOI"
)

tidy_model_matched_non_matched <- model |>
  broom::tidy(conf.int = TRUE) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    term_clean = dplyr::recode(term, !!!clean_labels),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      .default        = ""
    )
  )

# save as RData
save_cr(tidy_model_matched_non_matched, file = file.path(here("data",
                                         "tables_for_plots",
                                         "tidy_model_matched_non_matched.RData")))

# Contingency tables

# list of predictors to analyze
predictors <- c(
  "das_for_analysis",
  "human_data",
  "covid_related",
  "license_for_analysis",
  "is_detected_id_doi"
)

# function to build one contingency table
make_table <- function(var) {
  data_for_glm |>
    dplyr::count(in_dcc, !!sym(var)) |>
    tidyr::pivot_wider(
      names_from = !!sym(var),
      values_from = n,
      names_prefix = paste0(var, "_")
    ) |>
    mutate(variable = var, .before = 1)
}

# apply to each predictor
contingency_tables <- predictors |> map(make_table)

# name each list element for clarity
names(contingency_tables) <- predictors

# save as RData
save_cr(contingency_tables, file = file.path(here("data",
                                                  "tables_for_plots",
                                                  "contingency_tables.RData")))

# 2. GLMM: datasets age-citations relationship: 7 datasets only -----------------------------------

# Prepare table for analysis

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_8_dedup.RData")) # load dedup final results

ids_and_years <- dcc_detected_ids_all_sources_8_dedup |> 
  dplyr::filter(source_charite != "data_articles") |> # exclude data articles cases
  select(detected_id, charite_id_year, publication_year_dcc) |> 
  mutate(age = as.numeric(publication_year_dcc) - as.numeric(charite_id_year)) |> 
  dplyr::filter(age >= 0 & age <= 3) |> # get only datasets that have ages 0, 1, 2, and or 3
  group_by(detected_id, age) |> 
  summarise(number_of_citations_in_age = n()) # get number of citations for each dataset in each age

ids_and_years_only_0_3_ages <- ids_and_years |>
  dplyr::group_by(detected_id) |>
  dplyr::filter(dplyr::n_distinct(age) == 4) |> # get only datsets that have all four ages (0, 1, 2 & 3)
  dplyr::ungroup()

# model

# GLMM was chosen in order to handle non-normal data + grouped structure simultaneously.

model_nb <- glmmTMB( # mixed = considering that it's the same datasets between the years.
  number_of_citations_in_age ~ age + (1 | detected_id), # age-citation relationship with datasets as random effects 
  data = ids_and_years_only_0_3_ages,
  family = nbinom2 # takes into account the right-skewed-tail distribution of the data (v > m)
)

summary_model_glm <- summary(model_nb)

# save as RData
save_cr(ids_and_years_only_0_3_ages, file = file.path(here("data",
                                                           "inputs_for_quick_render",
                                                           "ids_and_years_only_0_3_ages.RData")))

save_cr(ids_and_years, file = file.path(here("data",
                                             "inputs_for_quick_render",
                                             "ids_and_years.RData")))

save_cr(model_nb, file = file.path(here("data",
                                        "tables_for_plots",
                                        "model_nb.RData")))

save_cr(summary_model_glm, file = file.path(here("data",
                                                 "tables_for_plots",
                                                 "summary_model_glm.RData")))



# 3. GLMM: datasets age-citations relationship: All datasets --------------------

# This is done only for documentation and for reporting the model's stats in the article.
# The model is not significant and no futher tests were conducted to find main simple effects.

# I will use "ids_and_years" that was created in the beginning of section (2) above.

# model

# GLMM was chosen in order to handle non-normal data + grouped structure simultaneously.

model_nb_all <- glmmTMB( # mixed = considering that it's the same datasets between the years.
  number_of_citations_in_age ~ age + (1 | detected_id), # age-citation relationship with datasets as random effects 
  data = ids_and_years,
  family = nbinom2 # takes into account the right-skewed-tail distribution of the data (v > m)
)

summary_model_glm_all <- summary(model_nb_all)

save_cr(model_nb_all, file = file.path(here("data",
                                        "tables_for_plots",
                                        "model_nb_all.RData")))

save_cr(summary_model_glm_all, file = file.path(here("data",
                                                 "tables_for_plots",
                                                 "summary_model_glm_all.RData")))


# 4. Print Outputs --------------------------------------------------------

# Define output .txt file
output_file <- here::here("results.txt")
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

# Start capturing output
sink(output_file)

# Intro text
cat(
  "This file was created by the script \"scripts/statistical_analysis.R\".\n",
  "It contains all relevant statistics for both tests performed during the analysis:\n",
  "χ² and GLMM. The GLMM was performed on a subset of 7 datasets identifiers. \n\n",
  sep = ""
)

# χ²

{
  cat("Logistic Regression Results:\n\n")
  print(results_glm)
  cat("\n\n")
}

{
  cat("Model Fit (AIC):\n\n")
  print(fit_glm)
  cat("\n\n")
}

{
  cat("Overall Model Chi-Square Test (Likelihood Ratio Test):\n\n")
  print(anova_result)
  cat("\n\n")
}

{
  cat(sprintf("Chi-squared test: χ²(%d) = %.2f, p = %.3g\n\n\n", chisq_df, chisq_stat, chisq_p))
}

# GLMM

{
  cat("Fixed Effects Estimates:\n\n")
  print(summary(model_nb)$coefficients$cond)
  cat("\n\n")
}

{
  cat("Fixed Effects: IRRs with 95% Confidence Intervals:\n\n")
  ci <- confint(model_nb, parm = "beta_", method = "Wald")
  irr_ci <- exp(ci)
  print(irr_ci)
  cat("\n\n")
}

{
  cat("Fixed Effects: p-values:\n\n")
  print(summary(model_nb)$coefficients$cond[, "Pr(>|z|)", drop = FALSE])
  cat("\n\n")
}

{
  cat("Incidence Rate Ratios (IRRs):\n\n")
  print(exp(fixef(model_nb)$cond))
  cat("\n\n")
}

{
  cat("Random Effects Variance Components:\n\n")
  print(VarCorr(model_nb))
  cat("\n\n")
}

{
  cat("Model Fit Metrics:\n\n")
  cat(sprintf("AIC = %.2f\n", AIC(model_nb)))
  r2_vals <- performance::r2(model_nb)
  cat(sprintf("Marginal R² (fixed effects) = %.3f\n", r2_vals$R2_marginal))
  cat(sprintf("Conditional R² (fixed + random) = %.3f\n\n\n", r2_vals$R2_conditional))
}

# Stop capture
sink()
