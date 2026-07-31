---
title: "README"
author: "Avihay Cohen"
date: "07-04-2026"
output: html_document
---

## Project Title

**Matching data references and institutional output to map the reuse of biomedical research data**

## Overview

-   This project uses the 3rd version of the Data Citation Corpus (DCC) to map the reuse of datasets shared alongside articles published by researchers from Charité
-   The DCC (v3) is available at https://doi.org/10.5281/zenodo.14897662 
-   The data processing steps are described below under "Notebooks logic"
-   All notebooks contain text and comments describing the data processing and analysis steps
-   All raw data was processed and analyzed in R (RStudio) using a Quarto manuscript project
-   The article is available in the root project folder in both HTML (`index.html`) and PDF (`index.pdf`) formats

------------------------------------------------------------------------

## Installation, Setup and Reproducing the Article:

1. Clone the repository to your local machine
2. In RStudio, open `Investigating the Reuse of Biomedical Data Using the DCC.Rproj`
3. In RStudio's console, run `renv::restore()` to recreate the environment needed to generate the article on your machine
4. Open `index.qmd`
5. In RStudio's terminal:
  * `quarto render --to pdf` will generate the article in a PDF format
  * `quarto render --to html` will generate the article in an HTML format (Please note that depending on your system and browser, the HTML might open automatically for you or not, but the file will be created in any case)

### Data Workflow Overview

Input: Raw data is stored in `data/raw/`

Processing:

-   Quarto notebooks in `notebooks/` load, clean and transform the data
-   R scripts in `scripts/` contain additional data processing steps

Output:

-   Processed data is stored in `data/wrangling_steps` as the final step of data wrangling in each folder

Results:

-   In the root project folder, `results.txt` contains all results of statistical analyses
-   Result variables are generated in `scripts/vars_for_paper.R` and are referenced in the manuscript (`index.qmd`)

Rendering:

-   `index.qmd` compiles the final article.

## Notebooks logic and order of execution

**Note:** Numeric suffixes in the `.RData` output file names (e.g., _4) refer to sequential processing steps in the data pipeline and should not be interpreted as version numbers.

### `notebooks/dcc_load_prep.qmd`

-   **Description**: Loads and cleans the DCC
-   **Output**: `data/wrangling_steps/dcc/DCC_corpus_11_std_lbl.RData`

### `notebooks/ds_primary_load_prep_match_clean.qmd`

-   **Description**: Loads and cleans extracted Charité datasets and data articles, looks for them in the DCC, cleans the results table
-   **Output**: `data/wrangling_steps/dcc_charite/numbat_da_dcc_joined_4_rm_au_ov.RData`

### `notebooks/ds_added_and_datastet_load_prep_match_clean.qmd`

-   **Description**: Loads and cleans additionally extracted Charité datasets and datasets extracted from DataStet, looks for them in the DCC, cleans the results table
-   **Output**: `data/wrangling_steps/dcc_datastet_and_added/dcc_ds_and_added_joined_8_ex.RData`

### `notebooks/joined_bind_add_metadata_verify.qmd`

-   **Description**: Joins both outputs of the two notebooks above into one result table, adds metadata and verifies results to create a final result table
-   **Output**: `data/wrangling_steps/all_sources_binded/dcc_detected_ids_all_sources_8_dedup.RData`

### `notebooks/plots/plots_prep.qmd`

-   **Description**: Uses result tables to create tables to feed into the plots code

### `notebooks/plots.qmd`

-   **Description**: Creates plots from tables created in "plots_prep.qmd". These plots are then referenced in the manuscript (`index.qmd`).

### `archive` folders

-   **Description**: Internal files and folders that were kept for the sake of the project's integrity. They might be referenced in the code for documentation purposes, but they should be overall ignored.

## Software environment

This project was developed using:

- R version 4.3.3 (2024-02-29 ucrt)
- RStudio 2024.4.0.735
- Quarto 1.6.42

Package versions are documented in `renv.lock`.

## License

- Code in this repository is licensed under the MIT License.
- Text, documentation, and data are licensed under CC BY 4.0.

See `LICENSE` and `LICENSE-text-data.md` for details.

Note: Third-party data or materials may have different licenses.

## Archived citable version

Releases of this repository are archived in Zenodo. Release v1.0.0 is available at https://doi.org/10.5281/zenodo.19235631. 

Please cite this repository using the following citation:
Cohen, A., Ivanović, B., Iarkaeva, A., Nachev, V., & Bobrov, E. (2026). quest-bih/Mapping-the-reuse-of-biomedical-research-data: Initial Release (data-citation-corpus). Zenodo. https://doi.org/10.5281/zenodo.19235630

## Contact

Name: Avihay Cohen
Email: avihay.cohen@bih-charite.de
Affiliation: Berlin Institute of Health at Charité - Universitätsmedizin Berlin (BIH), QUEST Center for Responsible Research

Name: Evgeny Bobrov
Email: evgeny.bobrov@bih-charite.de
Affiliation: Berlin Institute of Health at Charité - Universitätsmedizin Berlin (BIH), QUEST Center for Responsible Research
