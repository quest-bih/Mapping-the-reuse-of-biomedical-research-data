This folder contains the qmd notebooks that create the analysis pipeline.

	1. dcc_load_prep: loading and preprocessing the DCC

	2. ds_primary_load_prep_match_clean: loading and prepearing list of Charité datasets extracted from ODDPub ("primary" source of datasets)

	3. ds_added_and_datastet_load_prep_match_clean: loading and prepearing list of Charité datasets extracted from DataStet and additional datasets from reused datasets found from matching ODDPub with DCC

	4. joined_bind_add_metadata_verify: binding all sources together to create a final result table with added metadata of datasets and articles, then cleaning and verifying it

In addition, it contains the notebooks plots_prep (where tables from the data are created and saved) and plots (where these tables are being loaded to create the plots for the article)