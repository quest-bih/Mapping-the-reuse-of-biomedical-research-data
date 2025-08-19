
# 1. matched vs non matched -----------------------------------------------

# get data
df <- datasets_metadata_master_updated_012 |>
  dplyr::filter(
    !is.na(data_availability_statement)
    & !is.na(license)
    & !is.na(human_data)
    & !is.na(covid_related)
  ) |> 
  select(dataset_for_matching, in_dcc, data_availability_statement, license, human_data, covid_related) |> 
  distinct() |> 
  mutate(
    data_availability_statement = case_when(
      data_availability_statement %in% c("TRUE", "yes") ~ "TRUE",
      .default = "FALSE"),
    license = case_when(
      license %in% c("CC0", "CC BY", "cc-by", " CC BY-NC-SA", "cc0") ~ "TRUE",
      .default = "FALSE"
    ))

# convert to logical
df_cleaned <- df |>
  mutate(across(c(in_dcc, data_availability_statement, license, human_data, covid_related), ~ .x == "TRUE"))

# Create 2x2 table and run chi-square test for 'data_availability_statement'
tab_das <- table(df_cleaned$in_dcc, df_cleaned$data_availability_statement)
chisq.test(tab_das)

# Create 2x2 table and run chi-square test for 'license'
tab_license <- table(df_cleaned$in_dcc, df_cleaned$license)
chisq.test(tab_license)

# Create 2x2 table and run chi-square test for 'human_data'
tab_human <- table(df_cleaned$in_dcc, df_cleaned$human_data)
chisq.test(tab_human)

# Create 2x2 table and run chi-square test for 'covid_related'
tab_covid <- table(df_cleaned$in_dcc, df_cleaned$covid_related)
chisq.test(tab_covid)

# Run chi-square tests
p_das     <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$data_availability_statement))$p.value
p_license <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$license))$p.value
p_human   <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$human_data))$p.value
p_covid   <- chisq.test(table(df_cleaned$in_dcc, df_cleaned$covid_related))$p.value

# Combine raw p-values
p_raw <- c(p_das, p_license, p_human, p_covid)
names(p_raw) <- c("data_availability_statement", "license", "human_data", "covid_related")

# Adjusted p-values
p_bonferroni <- p.adjust(p_raw, method = "bonferroni")
p_holm       <- p.adjust(p_raw, method = "holm")
p_fdr        <- p.adjust(p_raw, method = "fdr")

# Output all results together
tibble(
  variable = names(p_raw),
  p_raw = p_raw,
  p_bonferroni = p_bonferroni,
  p_holm = p_holm,
  p_fdr = p_fdr
)

df_summary <- df_cleaned |>
  group_by(in_dcc) |>
  summarise(
    data_availability_statement = mean(data_availability_statement) * 100,
    license = mean(license) * 100,
    human_data = mean(human_data) * 100,
    covid_related = mean(covid_related) * 100,
    .groups = "drop"
  )

df_plot <- df_summary |>
  pivot_longer(
    cols = -in_dcc,
    names_to = "variable",
    values_to = "percent_true"
  ) |>
  mutate(
    variable = recode(variable,
                      data_availability_statement = "DAS"
    ),
    in_dcc = recode(as.character(in_dcc),
                    "TRUE" = "Yes",
                    "FALSE" = "No"
    )
  )


custom_colors <- c(
  "Yes" = "#1B9E77",
  "No" = "#E85F22")

ggplot(df_plot, aes(x = variable, y = percent_true, fill = in_dcc)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(percent_true, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    values = custom_colors,
    name = "Datasets are\nmentioned in DCC"
  ) +
  labs(
    title = "",
    x = "",
    y = "% TRUE"
  ) +
  ylim(0, 105) +
  theme_classic(base_size = 18) +
  theme(
    legend.title = element_text(size = 18, color = "black"),
    legend.text  = element_text(size = 16, color = "black"),
    axis.text    = element_text(size = 16, color = "black"),
    axis.title   = element_text(size = 18, color = "black"),
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5, color = "black")
  )

# Output just fdr
tibble(
  variable = names(p_raw),
  `p value` = p_fdr
)
