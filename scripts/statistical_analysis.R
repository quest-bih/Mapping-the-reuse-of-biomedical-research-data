
# 0. Setup ----------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, htmlwidgets, glmmTMB)

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

# get data
df <- datasets_metadata_master_updated_012 |>
  dplyr::filter(
    !is.na(data_availability_statement)
    & !is.na(license)
    & !is.na(human_data)
    & !is.na(covid_related)
  ) |> 
  select(dataset_for_matching, in_dcc, data_availability_statement, license, human_data, covid_related) |> 
  distinct() |> 
  mutate(
    data_availability_statement = case_when(
      data_availability_statement %in% c("TRUE", "yes") ~ "TRUE",
      .default = "FALSE"),
    license = case_when(
      license %in% c("CC0", "CC BY", "cc-by", " CC BY-NC-SA", "cc0") ~ "TRUE",
      .default = "FALSE"
    ))

# convert to logical
df_cleaned <- df |>
  mutate(across(c(in_dcc, data_availability_statement, license, human_data, covid_related), ~ .x == "TRUE"))

# Create 2x2 table and run chi-square test for 'data_availability_statement'
tab_das <- table(df_cleaned$in_dcc, df_cleaned$data_availability_statement)
chisq.test(tab_das)

# Create 2x2 table and run chi-square test for 'license'
tab_license <- table(df_cleaned$in_dcc, df_cleaned$license)
chisq.test(tab_license)

# Create 2x2 table and run chi-square test for 'human_data'
tab_human <- table(df_cleaned$in_dcc, df_cleaned$human_data)
chisq.test(tab_human)

# Create 2x2 table and run chi-square test for 'covid_related'
tab_covid <- table(df_cleaned$in_dcc, df_cleaned$covid_related)
chisq.test(tab_covid)

# Run chi-square tests
p_das     <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$data_availability_statement))$p.value
p_license <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$license))$p.value
p_human   <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$human_data))$p.value
p_covid   <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$covid_related))$p.value

# Combine raw p-values
p_raw <- c(p_das, p_license, p_human, p_covid)
names(p_raw) <- c("data_availability_statement", "license", "human_data", "covid_related")

# Adjusted p-values
p_bonferroni <- p.adjust(p_raw, method = "bonferroni")
p_holm       <- p.adjust(p_raw, method = "holm")
p_fdr        <- p.adjust(p_raw, method = "fdr")

# Output all results together
tibble(
  variable = names(p_raw),
  p_raw = p_raw,
  p_bonferroni = p_bonferroni,
  p_holm = p_holm,
  p_fdr = p_fdr
)

df_summary <- df_cleaned |>
  group_by(in_dcc) |>
  summarise(
    data_availability_statement = mean(data_availability_statement) * 100,
    license = mean(license) * 100,
    human_data = mean(human_data) * 100,
    covid_related = mean(covid_related) * 100,
    .groups = "drop"
  )

df_plot <- df_summary |>
  pivot_longer(
    cols = -in_dcc,
    names_to = "variable",
    values_to = "percent_true"
  ) |>
  mutate(
    variable = recode(variable,
                      data_availability_statement = "DAS"
    ),
    in_dcc = recode(as.character(in_dcc),
                    "TRUE" = "Yes",
                    "FALSE" = "No"
    )
  )


custom_colors <- c(
  "Yes" = "#1B9E77",
  "No" = "#E85F22")

ggplot(df_plot, aes(x = variable, y = percent_true, fill = in_dcc)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(percent_true, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    values = custom_colors,
    name = "Datasets are\nmentioned in DCC"
  ) +
  labs(
    title = "",
    x = "",
    y = "% TRUE"
  ) +
  ylim(0, 105) +
  theme_classic(base_size = 18) +
  theme(
    legend.title = element_text(size = 18, color = "black"),
    legend.text  = element_text(size = 16, color = "black"),
    axis.text    = element_text(size = 16, color = "black"),
    axis.title   = element_text(size = 18, color = "black"),
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5, color = "black")
  )

# Output just fdr
tibble(
  variable = names(p_raw),
  `p value` = p_fdr
)


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
