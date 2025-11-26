extract_affiliations <- function(x) {
  if (is.na(x)) return(tibble(author = character(), affs = list()))
  
  x |>
    str_split("\\);\\s*") |>
    unlist() |>
    discard(~ .x == "") |>
    map(~ paste0(.x, ")")) |>
    map(~ {
      author <- str_extract(.x, "^[^\\(]+") |> str_trim()
      affs_raw <- str_match(.x, "\\((.*)\\)") |> pluck(2)
      affs_split <- if (!is.na(affs_raw)) str_split(affs_raw, "\\s*;\\s*")[[1]] else NA_character_
      tibble(author = author, affs = list(affs_split))
    }) |>
    bind_rows()
}

df_long <- dois_from_dimensions_1_dois_au_aff |>
  rowwise() |>
  mutate(
    authors_df = list(extract_affiliations(authors)),
    raw_df     = list(extract_affiliations(au_raw_aff)),
    corr_df    = list(extract_affiliations(corr_au)),
    aff_df     = list(extract_affiliations(au_aff))
  ) |>
  ungroup() |>
  select(doi, authors_df, raw_df, corr_df, aff_df) |>
  pmap_dfr(function(doi, authors_df, raw_df, corr_df, aff_df) {
    full_join(authors_df, raw_df, by = "author", suffix = c("", "_raw")) |>
      full_join(corr_df, by = "author", suffix = c("", "_corr")) |>
      full_join(aff_df, by = "author", suffix = c("", "_aff")) |>
      mutate(doi = doi) |>
      mutate(
        max_len = pmap_int(list(affs, affs_raw, affs_corr, affs_aff),
                           ~ max(length(..1), length(..2), length(..3), length(..4), na.rm = TRUE))
      ) |>
      pmap_dfr(function(author, affs, affs_raw, affs_corr, affs_aff, doi, max_len) {
        tibble(
          doi = doi,
          authors = rep(author, max_len),
          au_raw_aff = affs_raw |> unlist() |> `length<-`(max_len) |> replace_na(NA_character_),
          corr_au    = affs_corr |> unlist() |> `length<-`(max_len) |> replace_na(NA_character_),
          au_aff     = affs_aff |> unlist() |> `length<-`(max_len) |> replace_na(NA_character_)
        )
      })
  }) |>
  relocate(doi, authors, au_raw_aff, corr_au, au_aff)

# Filter out the rows which have all of the authors
df_long_no_semicolon <- df_long |>
  dplyr::filter(!str_detect(authors, ";\\s"))

# Add position:

df_long_position <- df_long_no_semicolon |>
  group_by(doi) |>
  mutate(
    row_pos = row_number(),
    total = n(),
    raw_position = case_when(
      row_pos == 1 ~ "first",
      row_pos == total ~ "last",
      .default = "middle"
    )
  ) |>
  group_by(doi, authors) |>
  mutate(
    position = case_when(
      any(raw_position == "first") ~ "first",
      any(raw_position == "last") ~ "last",
      .default = "middle"
    )
  ) |>
  ungroup() |>
  select(-row_pos, -total, -raw_position)


# Add manually_added_aff column using case_when and manually fill "position" for these values as well
df_long_man_add_aff <- df_long_position |>
  mutate(
    manually_added_aff = case_when(
      doi == "10.1016/j.stem.2020.11.015" & authors == "Strzelecka, Paulina M." ~ "Charité",
      doi == "10.1182/bloodadvances.2022007714" & authors == "Haas, Simon" ~ "Charité",
      .default = NA_character_)) |> 
  mutate(
    position = case_when(
      doi == "10.1016/j.stem.2020.11.015" & authors == "Strzelecka, Paulina M." ~ "middle",
      doi == "10.1182/bloodadvances.2022007714" & authors == "Haas, Simon" ~ "middle",
      .default = position))
  
missing_authors <- tibble(
  doi = c(
    rep("10.1038/s41467-020-18367-y", 4),
    rep("10.1038/s41586-021-03767-x", 16),
    "10.1126/science.abo3627",
    "10.1242/dev.201228",
    rep("10.1016/j.bpsgos.2021.07.008", 16),
    rep("10.1038/s41467-020-20603-4", 10),
    "10.1038/s41586-022-04434-5",  
    "10.1038/s41586-022-05275-y",  
    "10.1038/s41588-023-01422-x"
    ),
  authors = c(
    # s41467-020-18367-y ### 1
    "Erk, Susanne", " Lett, Tristram A.", "Heinz, Andreas", "Walter, Henrik",
    
    # s41586-021-03767-x
    "Heidecker, Bettina", "Kurth, Florian", "Sander, Leif E.", "Mayer, Alena",
    "Braun, Alice", "Skurk, Carsten", "Thibeault, Charlotte", "Helbig, Elisa T.",
    "Kraft, Julia", "Lippert, Lena J.", "Suwalski, Phillip", "Ripke, Stephan",
    "Poller, Wolfgang", "Wang, Xiaomin", "Karadeniz, Zehra", "Landmesser, Ulf",
    
    # science.abo3627
    "von Bernuth, Horst",
    
    # dev.201228
    "Malte, Spielmann",
    
    # j.bpsgos.2021.07.008 ### 2
    "Ripke, Stephan", "Abdellaoui, Abdel", "Dolan, Conor V.", "Finucane, Hilary K.",
    "Kraft, Julia", "Mbarek, Hamdi", "Middeldorp, Christel M.", "Nivard, Michel G.",
    "Hottenga, Jouke-Jan", "Thompson, Wesley", "Wang, Yunpeng", "Weinsheimer, Shantel Marie",
    "Willemsen, Gonneke", "Boomsma, Dorret I.", "de Geus, E.J.C.", "Trubetskoy, Vassily",
    
    # s41467-020-20603-4
    "Capper, David",
    
    # s41586-022-04434-5 ### 3
    "Trubetskoy, Vassily", "Panagiotaropoulou, Georgia", "Awasthi, Swapnil", "Braun, Alice",
    "Kraft, Julia", "Skarabis, Nora", "Walter, Henrik", "Ripke, Stephan", "Hahn, Eric", "Ta, Thi Minh Tam",
    
    #s41586-022-05275-y
    "Langenberg, Claudia",
    
    #s41588-023-01422-x
    "Eckardt, Kai-Uwe"
    ),
  position = c(
    
    # s41467-020-18367-y ### 1
    rep("middle", 4),
    
    # s41586-021-03767-x
    rep("middle", 16),
    
    # science.abo3627
    "middle",
    
    # dev.201228
    "last",
    
    # j.bpsgos.2021.07.008 ### 2
    rep("middle", 16),
    
    # s41467-020-20603-4
    "middle",
    
    # s41586-022-04434-5  
    "first", rep("middle", 9),
    
    #s41586-022-05275-y
    "middle",
    
    #s41588-023-01422-x
    "middle"
    ),
  
  manually_added_aff = "Charité"
)

# Ensure same columns as df_long_man_add_aff by adding empty cols
missing_authors <- missing_authors |>
  mutate(
    au_raw_aff = NA_character_,
    corr_au = NA_character_,
    au_aff = NA_character_
  ) |>
  select(names(df_clean))  # aligns column order

# Append
df_long_bind_missing_authors <- bind_rows(df_long_man_add_aff, missing_authors)

# Append one more manual case:
df_long_bind_1_manual <- df_long_bind_missing_authors |> 
  bind_rows(tibble(
    doi = "10.3201/eid2707.204660",
    authors = "Moreira-Soto, Andres",
    au_raw_aff = NA_character_,
    corr_au = NA_character_,
    au_aff = NA_character_,
    position = "middle",
    manually_added_aff = "Charité"))


# Check that everything has "Charité" in at least one column
df_long_bind_1_manual |>
  group_by(doi) |>
  summarise(
    has_charite = any(
      str_detect(coalesce(au_raw_aff, ""), "Charité") |
        str_detect(coalesce(corr_au, ""), "Charité") |
        str_detect(coalesce(au_aff, ""), "Charité") |
        str_detect(coalesce(manually_added_aff, ""), "Charité")
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(!has_charite) |>
  distinct(doi)


# Keep only rows where any relevant column has "Charité"
df_charite <- df_long_bind_1_manual |>
  dplyr::filter(if_any(c(au_raw_aff, corr_au, au_aff, manually_added_aff), ~ str_detect(., "Charité")))


