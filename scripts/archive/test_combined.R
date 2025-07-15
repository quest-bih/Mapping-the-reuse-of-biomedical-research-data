datastet_results_4_std <- datastet_results_3_cleaned |> 
  mutate(
    slug = extracted_id |> 
      str_replace_all("\\s", "") |> 
      str_extract("osf\\.io/([A-Za-z0-9]{4,5})") |> 
      str_remove("osf\\.io/"),
    extracted_id_std = case_when(
      str_detect(extracted_id, "zenodo\\.org/record") ~ str_c(
        "10.5281/zenodo.",
        extracted_id |> 
          str_extract("zenodo\\.org/record/? *([0-9]+)") |> 
          str_remove("^zenodo\\.org/record/? *")
      ),
      str_detect(extracted_id, "zenodo") ~ str_c(
        "10.5281/zenodo.",
        extracted_id |> 
          str_extract("zenodo\\.? *([0-9]+)") |> 
          str_remove("^zenodo\\.? *")
      ),
      !is.na(slug) ~ paste0("//osf.io/", slug),
      str_detect(extracted_id, "figshare") ~ str_c(
        "10.6084/m9.figshare.",
        extracted_id |> 
          str_extract("figshare\\.? *([0-9]+)") |> 
          str_remove("^figshare\\.? *")
      ),
      str_detect(extracted_id, "mendeley.*datasets") ~ str_c(
        "10.17632/",
        extracted_id |> 
          str_replace_all("\\s", "") |> 
          str_extract("datasets/([A-Za-z0-9]+)") |> 
          str_remove("^datasets/")
      ),
      str_detect(extracted_id, "mendeley") ~ NA_character_,
      str_detect(extracted_id, "dryad") ~ str_c(
        "10.5061/dryad.",
        extracted_id |> 
          str_replace_all("\\s", "") |> 
          str_extract("dryad\\.?([A-Za-z0-9]+)") |> 
          str_remove("^dryad\\.?")
      ),
      str_detect(str_replace_all(extracted_id, "\\s", ""), str_c(prefixes, collapse = "|")) &
        !str_detect(extracted_id, "//") &
        !str_starts(str_trim(extracted_id), "10\\.") ~ (
          extracted_id |> 
            str_replace_all("(?<=\\d) \\d{1,2}(\\s*)$", "") |> 
            str_replace_all(" \\s*", "") |> 
            str_extract(str_c("^(?:", str_c(prefixes, collapse = "|"), ")[0-9]+")) |> 
            coalesce(extracted_id)
        ),
      str_detect(extracted_id, "10\\.") &
        !str_detect(extracted_id, "figshare|zenodo|osf|dryad|mendeley") &
        !str_detect(extracted_id, fixed(doi)) ~ extracted_id |> 
        str_replace_all("\\s+", "") |> 
        str_extract("10\\.[^\\s]+"),
      str_detect(extracted_id, "id=") ~ extracted_id |> 
        str_extract("id=([^\\s/&]+)") |> 
        str_remove("^id="),
      str_detect(extracted_id, "addgene\\.org/") ~ extracted_id |> 
        str_extract("addgene\\.org/([0-9 \\t]*)") |> 
        str_remove("addgene\\.org/") |> 
        str_remove_all("\\s"),
      .default = extracted_id
    ),
    dataset_for_matching = case_when(
      !is.na(slug) ~ paste0("osf_", slug),
      .default = NA
    )
  )

datastet_results_5_filtered <- datastet_results_4_std |> 
  dplyr::filter(!(str_detect(extracted_id_std, "//"))) |> 
  dplyr::filter(!str_detect(str_replace_all(extracted_id_std, "\\s", ""), "^or\\d+$")) |> 
  dplyr::filter(!str_detect(str_replace_all(extracted_id_std, "\\s", ""), "^[a-zA-Z]\\d+$"))


datastet_results_6_rm_trails <- datastet_results_5_filtered |> 
  mutate(extracted_id_std = str_remove(extracted_id_std, "\\.+$"))

datastet_results_7_rm_same_dois <- datastet_results_6_rm_trails |> 
    mutate(extracted_id_std = str_replace_all(extracted_id_std, "\\s+", "")) |> 
    dplyr::filter(!str_detect(extracted_id_std, fixed(doi)))

