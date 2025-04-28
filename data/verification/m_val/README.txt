The last modified csv file is the one with the most recent manual validation.

_16122024_AC.csv:
	phs000424 was removed due to it being a false positive.
	SRR12801265 was removed due to being not found in the paper

The qmd code loads this file and filter to get only cases where "valid" = TRUE (manually validated as valid, in which case ) or "valid" = " " (no need for manual validation).

The identifiers column that the qmd uses is:
"charite_data_id_or_acc_nr_merged" (see data dictionary in current folder for details)

