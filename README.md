---
title: "README"
author: "Avihay Cohen"
date: "2026-26-03"
output: html_document
---

## Project Title

*Investigating the Reuse of Biomedical Data Using the DCC*

## Overview

-   This project uses the Data Citation Corpus (DCC) to map the reuse of datasets shared alongside articles published by researchers from Charité
-   The data processing steps are described below under "Notebooks logic"
-   All notebooks contain text and comments describing the data processing and analysis steps
-   All raw data was processed and analyzed in R (R Studio) using a Quarto manuscript project

------------------------------------------------------------------------

## Installation and Setup:

Clone the repository to your local machine. Then, in RStudio's Console, run `renv::restore()` in order to install the exact R package versions recorded in `renv.lock` and recreate the project library on your machine.

Rendering `index.qmd` will recreate the article.

### Data Workflow Overview

Input: Raw data is stored in `data/raw/`

Processing:

-   Quarto notebooks in `notebooks/` load, clean and transform the data
-   R scripts in `scripts/` contain additional data processing steps

Output: Processed data is stored in `data/wrangling_steps` as the final step of data wrangling in each folder

Aggregated variables are generated for the manuscript in `scripts/vars_for_paper.R`.

Rendering: index.qmd compiles the final article.

## Notebooks logic and order of execution

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

-   **Description**: Joins both outputs of the two notebooks above to one result table, adds metadata and verifies results to create a final result table
-   **Output**: `data/wrangling_steps/all_sources_binded/dcc_detected_ids_all_sources_8_dedup.RData`

### `notebooks/plots/plots_prep.qmd`

-   **Description**: Uses results tables to create tables to feed into the plots code

### `notebooks/plots.qmd`

-   **Description**: Creates plots from tables created in "plots_prep.qmd". These plots are then referenced in the manuscript (`index.qmd`).

## Generating the article in RStudio

1. Open `Investigating the Reuse of Biomedical Data Using the DCC.Rproj`
2. Open `index.qmd`
3. In Rstudio's terminal:
  * `quarto render --to pdf` will recreate the article in a pdf format
  * `quarto render --to html` will recreate the article in a html format

## Software environment

This project was developed using:

- R version 4.3.3 (2024-02-29 ucrt)
- RStudio 2024.4.0.735
- Quarto 1.6.42

Package versions are documented in `renv.lock`.

## Citation

If you use this repository, please cite:

Avihay Cohen, Blanka Ivanovic, Anastasiia Iarkaeva, Vladislav Nachev, Evgeny Bobrov, 2026. **zenodo repo name**. Zenodo.  
DOI: https://doi.org/10.5281/zenodo.19221824

The corresponding code is available on GitHub:  
**github release url**

## License

- Code in this repository is licensed under the MIT License.
- Text, documentation, and data are licensed under CC BY 4.0.

See `LICENSE` and `LICENSE-text-data.md` for details.

Note: Third-party data or materials may have different licenses.

## Contact

Name: Evgeny Bobrov
Email: evgeny.bobrov@bih-charite.de
Affiliation: Berlin Institute of Health at Charité - Universitätsmedizin Berlin (BIH), QUEST Center for Responsible Research
