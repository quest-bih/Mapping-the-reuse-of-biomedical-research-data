prefixes <- c(
  "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
  "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
  "sr(p|r|x|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
  "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn", "rs"
)

# first filter out any "//" that isn't one of the other cases:
test_1_no_ursl_and_or <- test |> 
  dplyr::filter(
    !(str_detect(extracted_id, "//") &
        !str_detect(extracted_id, "figshare|zenodo|osf|dryad|mendeley") &
        !str_detect(extracted_id, "10\\.") &
        !str_detect(str_replace_all(extracted_id, "\\s", ""), str_c(prefixes, collapse = "|")) &
        !str_detect(extracted_id, "addgene|10xgenomics|id=") ) &
    !str_detect(str_replace_all(extracted_id, "\\s", ""), "^or\\d+$")
    )


# zenodo

test <- datastet_results_3_cleaned |> mutate(row_num = row_number())

test_zenodo <- test |> dplyr::filter(str_detect(extracted_id, "zenodo"))


test_zenodo_std <- test_zenodo |> 
  mutate(
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
      .default = extracted_id
    )
  )

# zenodo. 81814 15 should be zenodo.8181415

test_osf <- test |> dplyr::filter(str_detect(extracted_id, "osf"))

test_osf_std <- test_osf |> 
  mutate(
    slug = extracted_id |> 
      str_replace_all("\\s", "") |>  # remove all whitespace anywhere
      str_extract("osf\\.io/([A-Za-z0-9]{4,5})") |> 
      str_remove("osf\\.io/"),
    extracted_id_std = case_when(
      !is.na(slug) ~ paste0("//osf.io/", slug),
      .default = extracted_id
    ),
    dataset_for_matching = case_when(
      !is.na(slug) ~ paste0("osf_", slug),
      .default = NA
    )
  )

# figshare
test_figshare <- test |> dplyr::filter(str_detect(extracted_id, "figshare"))

test_figshare_std <- test_figshare |> 
  mutate(
    extracted_id_std = case_when(
      str_detect(extracted_id, "figshare") ~ str_c(
        "10.6084/m9.figshare.",
        extracted_id |> 
          str_extract("figshare\\.? *([0-9]+)") |> 
          str_remove("^figshare\\.? *")
      ),
      .default = extracted_id
    )
  )

#mendeley
test_mendeley <- test |> dplyr::filter(str_detect(extracted_id, "mendeley"))

test_mendeley_std <- test_mendeley |> 
  mutate(
    extracted_id_std = case_when(
      str_detect(extracted_id, "mendeley.*datasets") ~ str_c(
        "10.17632/",
        extracted_id |> 
          str_replace_all("\\s", "") |> 
          str_extract("datasets/([A-Za-z0-9]+)") |> 
          str_remove("^datasets/")
      ),
      str_detect(extracted_id, "mendeley") ~ NA_character_,
      .default = extracted_id
    )
  )

#dryad
test_dryad <- test |> dplyr::filter(str_detect(extracted_id, "dryad"))

test_dryad_std <- test_dryad |> 
  mutate(
    extracted_id_std = case_when(
      str_detect(extracted_id, "dryad") ~ str_c(
        "10.5061/dryad.",
        extracted_id |> 
          str_replace_all("\\s", "") |> 
          str_extract("dryad\\.?([A-Za-z0-9]+)") |> 
          str_remove("^dryad\\.?")
      ),
      .default = extracted_id
    )
  )

#prefixes
test_prefixes <- test |> 
  dplyr::filter(str_detect(str_replace_all(extracted_id, "\\s", ""), str_c(prefixes, collapse = "|"))) |>
  dplyr::filter(!str_detect(extracted_id, "//")) |> 
  dplyr::filter(!str_starts(str_trim(extracted_id), "10\\."))


test_prefixes_std <- test_prefixes |> 
  mutate(
    extracted_id_std = case_when(
      str_detect(str_replace_all(extracted_id, "\\s", ""), str_c(prefixes, collapse = "|")) &
        !str_detect(extracted_id, "//") &
        !str_starts(str_trim(extracted_id), "10\\.") ~ (
          extracted_id |> 
            str_replace_all("(?<=\\d) \\d{1,2}(\\s*)$", "") |>  # remove ' space + 1-2 digits' only at end
            str_replace_all(" \\s*", "") |>  # remove all remaining spaces
            str_extract(str_c("^(?:", str_c(prefixes, collapse = "|"), ")[0-9]+")) |> 
            coalesce(extracted_id)
        ),
      .default = extracted_id
    )
  )

#dois
test_dois_std <- test_dois |> 
  mutate(
    extracted_id_std = case_when(
      str_detect(extracted_id, "10\\.") &
        !str_detect(extracted_id, "figshare|zenodo|osf|dryad|mendeley") &
        !str_detect(extracted_id, fixed(doi)) ~ extracted_id |> 
        str_replace_all("\\s+", "") |> 
        str_extract("10\\.[^\\s]+"),
      .default = extracted_id
    )
  )

