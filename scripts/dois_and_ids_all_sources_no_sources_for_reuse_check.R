# check for reuse cases within this list using the proprietary dimensions database

# get dois+ids distinct

dcc_detected_ids_all_sources_2_rm_ad_ov_for_reuse_check <- dcc_detected_ids_all_sources_2_rm_ad_ov |> 
  select(doi_charite,
         doi_lc,
         data_identifier,
         data_id_lc,
         data_id_secondary,
         detected_id) |> 
  distinct()


dcc_detected_charite_ids_for_reuse_check <- dcc_detected_ids_all_sources_2_rm_ad_ov_for_reuse_check

# write as xlsx, csv and rda

if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)

# set path
base_path <- "C:/AVIHAY/Projects/DCC Project/DCC Local Files/charite datasets detected in dcc and their dois for reuse check"
file_name <- "dcc_detected_charite_ids_for_reuse_check"
full_base <- file.path(base_path, file_name)

# Save as CSV
write.csv(dcc_detected_charite_ids_for_reuse_check, paste0(full_base, ".csv"), row.names = FALSE)

# Save as XLSX
write.xlsx(dcc_detected_charite_ids_for_reuse_check, paste0(full_base, ".xlsx"))

# Save as RDA
save(dcc_detected_charite_ids_for_reuse_check, file = paste0(full_base, ".rda"))



