# This script creates variables that could then be referenced in index.qmd


# 1. Setup ----------------------------------------------------------------

Sys.setenv(LANG = "EN")  # Set environment language to English

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, DT, patchwork, RColorBrewer, here, tcltk, networkD3, readxl, lubridate, stringi, fs, lme4)

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
} # wrapper for write.csv() with automatic directory creation

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

# 2. Get results files ----------------------------------------------------

# Load relevant tables to extract variables from

# 1. Metadata

metadata <- load_latest_metadata_update()

metadata_no_da <- metadata |>
  dplyr::filter(source_charite != "data_articles") |>
  dplyr::filter(dataset_for_matching != "10.18112/openneuro.ds001226") # remove this overlapping case from Numbat cases, as it belongs to Data Articles

# 2. Matched

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_7_w_metadata.RData"))

load(here("data", "wrangling_steps", "all_sources_binded", "dcc_detected_ids_all_sources_8_dedup.RData"))

detected_dedup <- dcc_detected_ids_all_sources_8_dedup
detected_dup <- dcc_detected_ids_all_sources_7_w_metadata

detected_dedup_no_da <- dcc_detected_ids_all_sources_8_dedup |> dplyr::filter(source_charite != "data_articles")

### 2.1 Matched Numbat before and after exclusion of author overlap cases

# before
load(here("data", "wrangling_steps", "dcc_charite", "numbat_da_dcc_joined_3_au_info.RData"))

num_au_info <- numbat_da_dcc_joined_3_au_info |> 
  dplyr::filter(source_charite == "numbat") |> 
  dplyr::filter(dataset_for_matching != "10.18112/openneuro.ds001226") |> # remove this overlapping case from Numbat cases, as it belongs to Data Articles
  dplyr::filter(!stringr::str_detect(data_identifier, "1\\.0\\.1")) # 7 identifiers of the same dataset

# after
load(here("data", "wrangling_steps", "dcc_charite", "numbat_da_dcc_joined_4_rm_au_ov.RData"))

num_rm_au_ov <- numbat_da_dcc_joined_4_rm_au_ov |> 
  dplyr::filter(source_charite == "numbat") |> 
  dplyr::filter(dataset_for_matching != "10.18112/openneuro.ds001226") |> # remove this overlapping case from Numbat cases, as it belongs to Data Articles
  dplyr::filter(!stringr::str_detect(data_identifier, "1\\.0\\.1")) # 7 identifiers of the same dataset

# 3. For matching

## 3.1 Numbat

load(here("data", "wrangling_steps", "charite", "numbat_da_dois_and_ids_9_clean_pairs.RData"))

num_for_match <- numbat_da_dois_and_ids_9_clean_pairs |> 
  dplyr::filter(source == "numbat") |> 
  dplyr::filter(dataset_for_matching != "10.18112/openneuro.ds001226") |> # remove this overlapping case from Numbat cases, as it belongs to Data Articles
  dplyr::filter(!stringr::str_detect(data_identifier, "1\\.0\\.1")) # 7 identifiers of the same dataset

## 3.2 Additional IDs

load(here("data", "wrangling_steps", "datastet", "added_and_ds_for_matching_4_rm_exist.RData"))

ad_for_match <- added_and_ds_for_matching_4_rm_exist |> 
  dplyr::filter(source == "additional_ids") |> 
  distinct()

ad_non_matched <- ad_for_match |> 
  dplyr::filter(!dataset_for_matching %in% detected_dedup$detected_id) |> 
  select(dataset_for_matching) |> 
  distinct()

# 4. Analyses

# 4.1 Chi square matched: non-matched analysis

load(here("data", "tables_for_plots", "summary_model_chi.RData"))
load(here("data", "tables_for_plots", "fit_glm.RData"))
load(here("data", "tables_for_plots", "anova_result.RData"))
load(here("data", "tables_for_plots", "odds_ratios.RData"))

# 4.2 GLM: age-citation relationship

# 7 datasets with all 0-3 ages
load(here("data", "tables_for_plots", "summary_model_glm.RData"))
load(here("data", "tables_for_plots", "model_nb.RData"))

# all 123 datasets that have any 0-3 ages
load(here("data", "tables_for_plots", "summary_model_glm_all.RData"))
load(here("data", "tables_for_plots", "model_nb_all.RData"))


# 5. Data Articles

load(here("data", "tables_for_plots", "counts_ids_and_dois_da.RData")) # counts of flowchart

load(here("data", "wrangling_steps", "data_articles", "data_articles_dois_v10.RData")) # for number of reusing papers

# 6. DCC
load(here("data", "wrangling_steps", "dcc", "DCC_corpus_11_std_lbl.RData")) # DCC wrangled
load(here("data", "inputs_for_quick_render", "doi_is_id_count.RData")) # number of cases in DCC where doi = id

# clean up
rm(dcc_detected_ids_all_sources_7_w_metadata,
   dcc_detected_ids_all_sources_8_dedup,
   added_and_ds_for_matching_4_rm_exist,
   datasets_metadata_master_updated_022)

# 3. Build results tables --------------------------------------------------

# 1. Counts

res <- tibble(
  value = c(
    "placeholder",
    "total_published_charite_dois",            # All published Charite papers 2020-2023
    "ds_matched_ids_count_dist",               # DS   ids dist detected
    "n_matched_ids_count_dist",                # N+AD ids dist detected      
    "all_matched_ids_count_dist",              # All  ids dist detected
    
    "ds_non_matched_ids_count_dist",           # DS   ids dist not natched
    "n_non_matched_ids_count_dist",            # N+AD ids dist not matched

    "max_mentions_of_single_id",               # All  max-ref of 1 id
    "all_ids_with_1_mention_count_dist",       # All  ids with 1 mention
    "n_ids_with_1_mention_count_dist",         # N    ids with 1 mention
    
    "n_ids_for_match_count",                   # N+AD ids for matching
    "n_2nd_for_match_count",                   # N    2nd ids for matching

    "n_ids_gen_rep_for_match_count",            # N   only general purpose repos ids for matching
    
    "n_dois_for_match",                        # N    dois for matching
    "ad_dois_for_match",                       # AD   dois for matching
    
    "n_mentions",                              # N    refs dist detected
    "ds_mentions",                             # DS   refs dist detected
    "all_mentions",                            # All  refs dist detected
     
    "all_mentions_geo",                        # All  refs dist detected in GEO
    "all_mentions_mendeley",                   # All  refs dist detected in mendeley
    "all_mentions_emdb",                       # All  refs dist detected in emdb
    
    "all_matched_ids_count_dist_geo",          # All  ids dist detected in GEO          
    
    "n_mentions_gen",                          # N    refs dist detected in general purpose only
    
    "all_mentions_gen",                        # All  refs dist detected in general purpose only
    
    "all_mentions_gen_and_discp_non_omcis",    # All  refs dist detected in general purpose / disciplinary that are non-omics

    "human_data",                              # All  ids dist detected human-data
    
    "all_matched_charite_dois",                # All  Charite's dois dist detected
    
    "all_matched_dcc_dois",                    # All  Mentioning dois dist detected
    
    "da_dois",                                 # DA charite's data articles
    "da_ids",                                  # DA charite's datasets
    "da_mentions",                             # DA confirmed citations
    "da_citing_papers",                        # DA number of reusing papers
    "da_est_mentions",                         # DA estimated citations by extrapolation
    
    "dcc_datasets",                            # Number of dcc_datasets (in the corpus)
    "dcc_mentions",                            # Number of dcc_mentions (in the corpus)
    "dcc_rep",                                 # Number of dcc_repositories (in the corpus)
    "dcc_journals",                            # Number of dcc_journals (in the corpus)
    "dcc_papers",                              # Number of dcc_papers (in the corpus)
    "dcc_doi_is_dataset",                      # Number of dcc_doi = dcc_dataset (in the corpus)
    
    "non_matched_200_sampled_das_f",           # N ids dist not matched, with metadata, das = FALSE
    
    "ds_matched_dois_unique",                  # Number of matched DS unique articles' DOIs (that are also not in Numbat) 
    
    "au_ov_ids",                               # N ids omitted when removing cases with authors overlap
    
    "au_ov_doi_charite",                       # N charité dois omitted when removing cases with authors overlap
    
    "au_ov_doi_dcc",                           # N mentioning dois omitted when removing cases with authors overlap
    
    "au_ov_mentions",                          # N mentions omitted when removing cases with authors overlap)
    
    "n_matched_ids_count_dist_gen_rep"         # N+AD ids dist detected: Only General-purpose repositories
    
    ),
  count = c(
    
    # placeholder
    999,
    
    # total_published_charite_dois (2020-2023): Inserted manually by the number of PDFs we fed into DataStet
    4457+4924+2171+4196,
    
    # ds_matched_ids_count_dist
    detected_dup |> dplyr::filter(source_charite == "datastet") |> select(detected_id) |> distinct() |> nrow(),

    # n_matched_ids_count_dist
    detected_dedup |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> select(detected_id) |> distinct() |> nrow(),
    
    # all_matched_ids_count_dist
    detected_dedup_no_da |> select(detected_id) |> distinct() |> nrow(),
    
    # ds_non_matched_ids_count_dist (sanity check: returns 0)
    metadata |> dplyr::filter(in_dcc == "FALSE" & source_charite == "datastet") |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # n_non_matched_ids_count_dist (including additional ids that weren't detected)
    (num_for_match |> 
      dplyr::filter(!dataset_for_matching %in% detected_dedup$detected_id) |>
      select(dataset_for_matching) |> 
      dplyr::filter(!is.na(dataset_for_matching)) |> 
      distinct() |> 
      nrow()) + (ad_non_matched |> nrow()) - 2, # minus 2 because 2 of the matched are secondary ids, so they need to be removed from the non-matched count here

    # max_mentions_of_single_id
    detected_dedup_no_da |>
      distinct(detected_id, doi_dcc) |>
      group_by(detected_id) |> 
      summarise(n=n()) |> 
      arrange(desc(n)) |> 
      slice(1) |>
      pull(n),
    
    # all_ids_with_1_mention_count_dist
    detected_dedup_no_da |>
      distinct(detected_id, doi_dcc) |>
      group_by(detected_id) |> 
      summarise(n=n()) |>
      dplyr::filter(n == "1") |>
      nrow(),
    
    # n_ids_with_1_mention_count_dist
    detected_dedup_no_da |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> 
      distinct(detected_id, doi_dcc) |>
      group_by(detected_id) |> 
      summarise(n=n()) |>
      dplyr::filter(n == "1") |>
      nrow(),
    
    # n_ids_for_match_count (including additional)
    (num_for_match |>
      select(dataset_for_matching) |>
      dplyr::filter(!is.na(dataset_for_matching)) |>
      distinct() |> 
      nrow()) + (ad_for_match |> 
                   select(dataset_for_matching) |> 
                   dplyr::filter(!is.na(dataset_for_matching)) |>
                   distinct() |> 
                   nrow()),
    
    # n_2nd_for_match_count
    num_for_match |> select(data_id_secondary) |> dplyr::filter(!is.na(data_id_secondary)) |> distinct() |> nrow(),
    
    # n_ids_gen_rep_for_match_count
    (numbat_da_dois_and_ids_9_clean_pairs |>
      dplyr::filter(
        str_detect(
          dataset_for_matching,
          regex("osf|zenodo|figshare|10\\.7910/dvn|10\\.17632|10\\.17863/cam|dryad", ignore_case = TRUE)
        )
      ) |>
      select(dataset_for_matching) |> 
      distinct() |>
      nrow()) + (ad_for_match |> 
                   dplyr::filter(
                     str_detect(
                       dataset_for_matching,
                       regex("osf|zenodo|figshare|10\\.7910/dvn|10\\.17632|10\\.17863/cam|dryad", ignore_case = TRUE)
                       )
                     ) |>
                   select(dataset_for_matching) |> 
                   distinct() |>
                   nrow()),
    
    # n_dois_for_match
    num_for_match |> select(doi_no_ver_info) |> distinct() |> nrow(),
    
    # ad_dois_for_match (They are included in n_dois_for_match)
    ad_for_match |> dplyr::filter(source == "additional_ids") |> select(doi) |> distinct() |> nrow(),
    
    # n_mentions
    detected_dedup |> dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # ds_mentions (no overlap)   
    detected_dedup |> dplyr::filter(source_charite == "datastet") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions (no overlap)  
    detected_dedup_no_da |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_geo
    detected_dedup_no_da |> dplyr::filter(repository == "gene expression omnibus (geo)") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_mendeley
    detected_dedup_no_da |> dplyr::filter(repository == "mendeley") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_emdb
    detected_dedup_no_da |> dplyr::filter(repository == "the electron microscopy data bank (emdb)") |> distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_matched_ids_count_dist_geo
    detected_dedup_no_da |> dplyr::filter(repository == "gene expression omnibus (geo)") |> distinct(detected_id) |> nrow(),
    
    # n_mentions_gen
    detected_dedup_no_da |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |> 
      dplyr::filter(repository %in% c(
        "osf",
        "dryad",
        "zenodo",
        "figshare",
        "mendeley",
        "harvard dataverse",
        "apollo - university of cambridge repository")) |>
      distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_gen
    detected_dedup_no_da |>
      dplyr::filter(repository %in% c(
        "osf",
        "dryad",
        "zenodo",
        "figshare",
        "mendeley",
        "harvard dataverse",
        "apollo - university of cambridge repository")) |>
      distinct(detected_id, doi_dcc) |> nrow(),
    
    # all_mentions_gen_and_discp_non_omcis
    detected_dedup_no_da |> dplyr::filter(repository %in% c(
      "the electron microscopy data bank (emdb)",
      "openneuro",
      "zenodo",
      "figshare",
      "mendeley",
      "harvard dataverse",
      "apollo - university of cambridge repository")) |>
      distinct(detected_id, doi_dcc) |> nrow(),
    
    # human_data ids
    detected_dedup_no_da |> dplyr::filter(human_data == "TRUE") |> select(detected_id) |> distinct() |> nrow(),
    
    # all_matched_charite_dois 
    detected_dedup_no_da |> select(doi_no_ver_info) |> distinct() |> nrow(),
    
    # all_matched_dcc_dois
    detected_dedup_no_da |> select(doi_dcc) |> distinct() |> nrow(),
    
    # The 5 data articles analysis counts below were verified with BI and EB:
    
    # da_dois" (DA charite's data articles)
    counts_ids_and_dois_da |> dplyr::filter(category == "da_doi") |> dplyr::pull(count) |> unlist() |> as.integer() -7, # 7 non-valid entries are excluded from the count
    
    # da_ids" (DA charite's datasets)
    counts_ids_and_dois_da |> dplyr::filter(category == "da_datasets") |> dplyr::pull(count) |> unlist() |> as.integer() + 1, # 35
    
    # da_mentions" (DA confirmed citations)
    counts_ids_and_dois_da |> dplyr::filter(category == "n_citations") |> dplyr::pull(count) |> unlist() |> as.integer() + 65, # 1728 citations overall
    
    # da_citing_papers" (DA number of reusing papers)
    counts_ids_and_dois_da |> dplyr::filter(category == "reusing_papaers") |> dplyr::pull(count) |> unlist() |> as.integer(), # 113 confirmed data citations
    
    # da_est_mentions (DA estimated citations by extrapolation)
    846,
    
    # dcc_datasets Number of dcc_datasets (in the corpus)
    DCC_corpus_11_std_lbl |> select(dataset_for_matching) |> distinct() |> nrow(),
    
    # dcc_mentions (Number of dcc_mentions (in the corpus))
    DCC_corpus_11_std_lbl |> select(dataset_for_matching, doi) |> distinct() |> nrow(),
    
    # dcc_rep (Number of dcc_repositories (in the corpus))
    DCC_corpus_11_std_lbl |> select(repository) |> distinct() |> nrow(),
    
    # dcc_journals (Number of dcc_journals (in the corpus))
    DCC_corpus_11_std_lbl |> select(journal) |> distinct() |> nrow(),
    
    # dcc_papers (Number of dcc_papers (in the corpus))
    DCC_corpus_11_std_lbl |> select(doi) |> distinct() |> nrow(),
    
    # dcc_doi_is_dataset (Number of dcc_doi = dcc_dataset (in the corpus))
    doi_is_id_count,
    
    # non_matched_200_sampled_das_f
    metadata_no_da |> 
      dplyr::filter(in_dcc == "FALSE"
                    & das_for_analysis %in% c("FALSE")) |>
      select(dataset_for_matching) |> 
      distinct() |> 
      nrow(),
    
    # ds_matched_dois_unique: (Number of matched DS unique articles' DOIs (that are also not in Numbat)
    detected_dedup_no_da |> 
      dplyr::filter(source_charite == "datastet") |> 
      select(doi_lc) |> 
      distinct() |> 
      dplyr::filter(!doi_lc %in% (
        detected_dedup_no_da |>
          dplyr::filter(source_charite != "datastet") |> # "Additional"'s DOIs are just a subset of "Numbat"'s DOIs
          pull(doi_lc))) |> 
      distinct(doi_lc) |>
      nrow(),
    
    # au_ov_ids (Number of ids omitted when removing cases with authors overlap)
    num_au_info |>
      select(detected_id) |>
      distinct() |>
      anti_join(num_rm_au_ov |>
                  select(detected_id) |>
                  distinct()) |>
      nrow(),
    
    # au_ov_doi_charite (Number of doi_charite omitted when removing cases with authors overlap)
    num_au_info |>
      select(doi_lc) |>
      distinct() |>
      anti_join(num_rm_au_ov |>
                  select(doi_lc) |>
                  distinct()) |>
      nrow(),
    
    # au_ov_doi_dcc (Number of doi_dcc omitted when removing cases with authors overlap)
    num_au_info |>
      select(doi_dcc) |>
      distinct() |>
      anti_join(num_rm_au_ov |>
                  select(doi_dcc) |>
                  distinct()) |>
      nrow(),
    
    # au_ov_mentions (Number of mentions omitted when removing cases with authors overlap)
    num_au_info |>
      select(doi_dcc, detected_id) |>
      distinct() |>
      anti_join(num_rm_au_ov |>
                  select(doi_dcc, detected_id) |>
                  distinct()) |>
      nrow(),
    
    # n_matched_ids_count_dist_gen_rep
    detected_dedup |>
      dplyr::filter(source_charite %in% c("numbat", "additional_ids")) |>
      dplyr::filter(repository %in% c(
        "osf",
        "dryad",
        "zenodo",
        "figshare",
        "mendeley",
        "harvard dataverse",
        "apollo - university of cambridge repository")) |>
      select(detected_id) |>
      distinct() |>
      nrow()
    
    )
  )

res <- res |> pivot_wider(names_from = value, values_from = count) |> mutate(placeholder = "__")

# save as RData
save_cr(res, file = file.path(here("data",
                                   "inputs_for_quick_render",
                                   "res.RData")))


# 4. Build Analyses results tables ----------------------------------------

# Chi square results:

# 1. Extract raw p-values (excluding intercept)
coeffs <- summary_model_chi$coefficients
raw_p <- coeffs[, "Pr(>|z|)"]
raw_p <- raw_p[!str_detect(names(raw_p), "Intercept")]

# 2. Rename raw p-values
raw_p_named <- raw_p |>
  setNames(names(raw_p) |> str_remove("TRUE") |> (\(x) paste0("p_", x))())

# 3. Adjust p-values (FDR by default)
adj_p <- p.adjust(raw_p, method = "fdr")

# 4. Rename adjusted p-values
adj_p_named <- adj_p |>
  setNames(names(adj_p) |> str_remove("TRUE") |> (\(x) paste0("p_adj_", x))())

# 5. Extract overall model p-value and format
p_model <- anova_result$`Pr(>Chi)`[2] |> as.numeric()
p_model <- ifelse(p_model < 1e-6, "<0.000001", formatC(p_model, format = "f", digits = 6))
names(p_model) <- "p_model"

# 6. Get odds ratios (excluding intercept)
or_vals <- odds_ratios[!str_detect(names(odds_ratios), "Intercept")]
or_vals_named <- or_vals |>
  setNames(names(or_vals) |> str_remove("TRUE") |> (\(x) paste0("odds_ratios_", x))())

# 7. Extract χ² test stat and p-value
chisq_stat <- anova_result$Deviance[2]
chisq_df <- anova_result$Df[2]
chisq_p <- anova_result$`Pr(>Chi)`[2]

chisq_stat_named  <- setNames(chisq_stat, "chisq_stat")
chisq_df_named  <- setNames(chisq_df, "chisq_df")
chisq_p_named <- setNames(chisq_p, "chisq_p")

fit_glm_named  <- setNames(fit_glm$AIC, "fit_glm")

# 8. Combine into final 1-row tibble
stat_chi_metadata_res <- tibble(
  !!!as.list(raw_p_named),
  !!!as.list(adj_p_named),
  p_model = p_model,
  !!!as.list(or_vals_named),
  !!!as.list(chisq_stat_named),
  !!!as.list(chisq_df_named),
  !!!as.list(chisq_p_named),
  !!!as.list(fit_glm_named)
)

# save as RData
save_cr(stat_chi_metadata_res, file = file.path(here("data",
                                   "inputs_for_quick_render",
                                   "stat_chi_metadata_res.RData")))

# GLMM: age-citation relationship model results (subset of 7 datasets)

# Model with 7 datasets that have all 0, 1, 2, 3 citation years:

  load(here("data", "tables_for_plots", "summary_model_glm.RData"))

  # Fixed effect estimate and p-value
  age_estimate <- summary_model_glm$coefficients$cond["age", "Estimate"]
  age_se <- summary_model_glm$coefficients$cond["age", "Std. Error"]
  age_p <- summary_model_glm$coefficients$cond["age", "Pr(>|z|)"]
  
  # IRR and 95% CI (exponentiated)
  ci <- confint(model_nb, parm = "beta_", method = "Wald")
  ci_age <- ci["age", ]  # this is on the log scale
  irr_age <- exp(age_estimate)
  irr_ci_lower <- exp(ci_age[1])
  irr_ci_upper <- exp(ci_age[2])
  
  # Extract the SD of the random intercept (first random effect group)
  vc <- VarCorr(model_nb) # get var-cov structure
  re_sd <- attr(vc$cond[[1]], "stddev")[1]
  
  # Model fit metrics
  aic_val <- AIC(model_nb)
  r2_vals <- performance::r2(model_nb)
  r2_marginal <- r2_vals$R2_marginal
  r2_conditional <- r2_vals$R2_conditional

# Model with all 123 datasets that have any of 0, 1, 2, 3 citation years:
  
  load(here("data", "tables_for_plots", "summary_model_glm_all.RData"))
  
  # Fixed effect estimate and p-value
  age_estimate_all <- summary_model_glm_all$coefficients$cond["age", "Estimate"]
  age_se_all <- summary_model_glm_all$coefficients$cond["age", "Std. Error"]
  age_p_all <- summary_model_glm_all$coefficients$cond["age", "Pr(>|z|)"]
  
  # IRR and 95% CI (exponentiated)
  ci_all <- confint(model_nb_all, parm = "beta_", method = "Wald")
  ci_age_all <- ci_all["age", ]  # this is on the log scale
  irr_age_all <- exp(age_estimate_all)
  irr_ci_lower_all <- exp(ci_age_all[1])
  irr_ci_upper_all <- exp(ci_age_all[2])
  
# Create a tibble with models' stats
stat_glmm_age_res <- tibble(
  
  # All stats for model with 7 datasets that have all 0, 1, 2, 3 citation years:
  estimate_age = age_estimate,
  irr_age = irr_age,
  irr_ci_lower = irr_ci_lower,
  irr_ci_upper = irr_ci_upper,
  p_age = age_p,
  sd_random_intercept = re_sd,
  aic = aic_val,
  r2_marginal = r2_marginal,
  r2_conditional = r2_conditional,
  
  # Only relevant model stats for model with all 123 datasets that have any of 0, 1, 2, 3 citation years:
  
  irr_age_all = irr_age_all,
  irr_ci_lower_all = irr_ci_lower_all,
  irr_ci_upper_all = irr_ci_upper_all,
  p_age_all = age_p_all
)

# save as RData
save_cr(stat_glmm_age_res, file = file.path(here("data",
                                     "inputs_for_quick_render",
                                     "stat_glmm_age_res.RData")))