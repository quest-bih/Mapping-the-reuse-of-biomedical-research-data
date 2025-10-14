t1 <- detected_no_sources_overlap |>
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id, doi_dcc) |> 
  distinct() |>
  group_by(detected_id) |>
  summarise(n = n()) |> 
  arrange(detected_id, n)

t2 <- detected_no_sources_overlap |>
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id, doi_dcc) |> 
  distinct() |> 
  group_by(detected_id) |>
  summarise(n = n_distinct(doi_dcc)) |> 
  arrange(detected_id, n)

t3 <- detected_no_sources_overlap |>
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id, doi_dcc) |> 
  distinct() |> 
  group_by(detected_id) |>
  summarise(n = n_distinct(doi_dcc)) |> 
  arrange(detected_id, n)

t4 <- detected_no_sources_overlap |>
  dplyr::filter(source_charite != "data_articles") |> 
  select(detected_id, doi_dcc) |> 
  distinct() |> 
  group_by(detected_id) |>
  summarise(n = n()) |> 
  arrange(detected_id, n)


waldo::compare(t1, t2)
waldo::compare(t3, t4)
waldo::compare(t1, t3)

t1 |> 
  full_join(t2, by = "detected_id", suffix = c(".t1", ".t2")) |> 
  mutate(
    status = case_when(
      n.t1 == n.t2 ~ "equal",
      .default      = "changed"
    )
  ) |> 
  dplyr::filter(status == "changed")

t3 |> 
  full_join(t4, by = "detected_id", suffix = c(".t3", ".t4")) |> 
  mutate(
    status = case_when(
      n.t3 == n.t4 ~ "equal",
      .default      = "changed"
    )
  ) |> 
  dplyr::filter(status == "changed")

t1 |> 
  full_join(t3, by = "detected_id", suffix = c(".t1", ".t3")) |> 
  mutate(
    status = case_when(
      n.t1 == n.t3 ~ "equal",
      .default      = "changed"
    )
  ) |> 
  dplyr::filter(status == "changed")


rep_dt_1 <- detected_no_sources_overlap |> 
  mutate(repository = case_when(
    repository %in% c("ncbi dbgap",
                      "ncbi dbgap (database of genotypes and phenotypesgenotypes and phenotypes)") ~ "NCBI dbGaP",
    repository == "figshare" ~ "Figshare",
    repository == "the european genome-phenome archive(ega)" ~ "EGA",
    repository == "gene expression omnibus (geo)" ~ "GEO",
    repository == "pride proteomics identification database" ~ "PRIDE",
    repository == "ncbi reference sequence database" ~ "NCBI RefSeq",
    repository == "european nucleotide archive" ~ "ENA",
    repository == "the protein data bank" ~ "PDB",
    repository == "mendeley" ~ "Mendeley",
    repository == "bioproject" ~ "Bioproject",
    repository == "arrayexpress" ~ "Arrayexpress",
    repository == "openneuro" ~ "Openneuro",
    repository == "uniprot" ~ "Uniprot",
    repository == "zenodo" ~ "Zenodo",
    repository == "harvard dataverse" ~ "Harvard Dataverse",
    repository == "the electron microscopy data bank (emdb)" ~ "EMDB",
    repository == "the international genome sample resource" ~ "IGSR",
    repository == "the international genome sample resource" ~ "IGSR",
    repository == "apollo - university of cambridge repository" ~ "Apollo",
    .default = repository)) |>  # shorten rep names for a nicer appearance
  select(repository, doi_dcc, detected_id) |>
  group_by(repository) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

rep_dt_2 <- detected_no_sources_overlap |> 
  mutate(repository = case_when(
    repository %in% c("ncbi dbgap",
                      "ncbi dbgap (database of genotypes and phenotypesgenotypes and phenotypes)") ~ "NCBI dbGaP",
    repository == "figshare" ~ "Figshare",
    repository == "the european genome-phenome archive(ega)" ~ "EGA",
    repository == "gene expression omnibus (geo)" ~ "GEO",
    repository == "pride proteomics identification database" ~ "PRIDE",
    repository == "ncbi reference sequence database" ~ "NCBI RefSeq",
    repository == "european nucleotide archive" ~ "ENA",
    repository == "the protein data bank" ~ "PDB",
    repository == "mendeley" ~ "Mendeley",
    repository == "bioproject" ~ "Bioproject",
    repository == "arrayexpress" ~ "Arrayexpress",
    repository == "openneuro" ~ "Openneuro",
    repository == "uniprot" ~ "Uniprot",
    repository == "zenodo" ~ "Zenodo",
    repository == "harvard dataverse" ~ "Harvard Dataverse",
    repository == "the electron microscopy data bank (emdb)" ~ "EMDB",
    repository == "the international genome sample resource" ~ "IGSR",
    repository == "the international genome sample resource" ~ "IGSR",
    repository == "apollo - university of cambridge repository" ~ "Apollo",
    .default = repository)) |>  # shorten rep names for a nicer appearance
  select(repository, doi_dcc, detected_id) |>
  group_by(repository) |> 
  summarise(n = n_distinct(doi_dcc, detected_id)) |> 
  arrange(desc(n))


detected_no_sources_overlap |> 
  dplyr::filter(source_charite != "data_articles") |> # exclude data articles detected
  select(detected_id, doi_dcc) |> 
  distinct() |> 
  nrow()


detected_no_sources_overlap |>
  select(doi_dcc, detected_id, publication_year_dcc) |> 
  distinct() |> 
  group_by(doi_dcc, detected_id) |> 
  summarise(n = n()) |> 
  dplyr::filter(n>1)

t1 <- detected_no_sources_overlap |>
  select(doi_dcc, detected_id, publication_year_dcc) |>
  distinct()

t1 |> head(30)


t2 <- t1 |>
  count(detected_id, publication_year_dcc, name = "count")


t3 <- t2 |> 
  mutate(
    detected_id = case_when(
      !detected_id %in% top_7$detected_id ~ "Others",
      .default = detected_id),
    publication_year_dcc = as.character(publication_year_dcc),
    publication_year_dcc = case_when(
      publication_year_dcc %in% c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012") ~ "<=2012",
      .default = publication_year_dcc)) |> 
  group_by(detected_id, publication_year_dcc) |> 
  summarise(count = n(), .groups = "drop") |> 
  dplyr::filter(!publication_year_dcc %in% c("2023", "2024"))

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "6y2f") |> 
  dplyr::filter(publication_year_dcc == "2019") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "gse55235") |> 
  dplyr::filter(publication_year_dcc == "2015") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "6y2e") |> 
  dplyr::filter(publication_year_dcc == "2019") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "gse44076") |> 
  dplyr::filter(publication_year_dcc == "2019") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "gse14764") |> 
  dplyr::filter(publication_year_dcc == "") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "6y2g") |> 
  dplyr::filter(publication_year_dcc == "2017") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(detected_id == "gse45547") |> 
  dplyr::filter(publication_year_dcc == "2013") |> 
  select(doi_dcc) |> 
  distinct() |> 
  nrow()

detected_no_sources_overlap |> 
  dplyr::filter(source_charite != "data_articles") |> 
  select(repository) |> 
  distinct() |> 
  arrange()

#


not_mentioned_years <- datasets_metadata_master_latest |>
  dplyr::filter(source_charite != "data_articles") |> 
  dplyr::filter(in_dcc == "FALSE") |>
  dplyr::filter(!is.na(covid_related)) |>
  select(charite_id_year, dataset_for_matching) |>
  distinct() |>
  dplyr::filter(!is.na(charite_id_year)) |> 
  group_by(charite_id_year) |>
  summarise(n = n())


# venn

detected_no_sources_overlap |>
  select(detected_id, source_charite) |>
  distinct() |> 
  group_by(source_charite) |> 
  summarise(n = n_distinct(detected_id))


detected_no_sources_overlap |>
  select(detected_id, source_charite) |>
  distinct() |> 
  group_by(source_charite) |> 
  summarise(n = n())
