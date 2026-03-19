---
title: "README"
author: "Avihay Cohen"
date: "2025-11-12"
output: html_document
---

## Project Title

*Investigating the Reuse of Biomedical Data Using the DCC*

## Overview

-   This project uses the Data Citation Corpus (DCC) to map the reuse of datasets shared alongside articles published by researchers from Charité
-   The data processing steps are described below under "Notebooks logic"
-   All notebooks contain text and comments describing the data processing steps
-   All raw data was processed and analyzed in R (R Studio) using a Quarto manuscript project

------------------------------------------------------------------------

## Installation and Setup:

Clone the repository to your local machine. Then, in RStudio's Console, run `renv::restore()` in order to install the exact R package versions recorded in `renv.lock` and recreate the project library on your machine.

Rendering **index.qmd** will recreate the article.

### Data Workflow Overview

Input: Raw data is stored in `data/raw/`

Processing:

-   Quarto notebooks in `notebooks/` load, clean and transform the data
-   R scripts in `scripts/` contain additional data processing steps

Output: Processed data is stored in `data/wrangling_steps` as the final step in each folder

Plots:

Aggregated variables are generated for the manuscript

Rendering: index.qmd compiles the final paper

## Notebooks logic

### `notebooks/dcc_load_prep.qmd`

-   **Description**: Loads and cleans the DCC
-   **Output**: `data/wrangling_steps/dcc/DCC_corpus_11_std_lbl.RData`

### `notebooks/ds_primary_load_prep_match_clean.qmd`

-   **Description**: Loads and cleans extracted Charité datasets and data articles, looks for them in the DCC, cleans the results table
-   **Output**: `data/wrangling_steps/dcc_charite/numbat_da_dcc_joined_4_rm_au_ov.RData`

### `notebooks/ds_added_and_datastet_load_prep_match_clean.qmd`

-   **Description**: Loads and cleans additionally extracted Charité datasets and dataset extracted from DataStet, looks for them in the DCC, cleans the results table
-   **Output**: `data/wrangling_steps/dcc_datastet_and_added/dcc_ds_and_added_joined_8_ex.RData.RData`

### `notebooks/joined_bind_add_metadata_verify.qmd`

-   **Description**: Joins both outputs above to one result table, adds metadata and verifies results to create a final result table
-   **Output**: `data/wrangling_steps/all_sources_binded/dcc_detected_ids_all_sources_8_dedup.RData`

### `notebooks/plots/plots_prep.qmd`

-   **Description**: Using results tables to create tables to feed into the plots code
-   **Output**: \`\`

### `notebooks/plots.qmd`

-   **Description**: Creating plots from tables created in "plots.qmd"
-   **Output**: \`\`

## Generating the paper

quarto::quarto_render("index.qmd") will create an "index.html" file with the rendered paper.

## Citation

## License

## Contact

Name: Email: Affiliation:
