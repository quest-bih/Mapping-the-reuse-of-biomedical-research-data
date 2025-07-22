df <- datasets_metadata_master_updated_009 |>
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
      .default = "FALSE"))


# 1. Is the proportion of human_data different between in_dcc groups?
chisq.test(table(df$in_dcc, df$human_data))

# 2. Is the proportion of covid_related different between in_dcc groups?
chisq.test(table(df$in_dcc, df$covid_related))

# 2. Is the proportion of covid_related different between in_dcc groups?
chisq.test(table(df$in_dcc, df$data_availability_statement))


# View observed counts
table(df$in_dcc, df$human_data)
table(df$in_dcc, df$covid_related)


# View expected counts (for diagnostics)
chisq.test(table(df$in_dcc, df$human_data))$expected
chisq.test(table(df$in_dcc, df$covid_related))$expected

df |>
  count(in_dcc, human_data) |>
  group_by(in_dcc) |>
  mutate(percent = n / sum(n)) |>
  ggplot(aes(x = in_dcc, y = percent, fill = human_data)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "In DCC",
    y = "Percentage of Datasets",
    fill = "Human Data",
    title = "Proportion of Human Datasets by DCC Status"
  ) +
  theme_minimal()

df |>
  count(in_dcc, covid_related) |>
  group_by(in_dcc) |>
  mutate(percent = n / sum(n)) |>
  ggplot(aes(x = in_dcc, y = percent, fill = covid_related)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "In DCC",
    y = "Percentage of Datasets",
    fill = "COVID-related",
    title = "Proportion of COVID-related Datasets by DCC Status"
  ) +
  theme_minimal()


### HEATMAPS

# Plot

plot_data <- df |>
  select(in_dcc, covid_related, human_data, data_availability_statement) |>
  pivot_longer(cols = -in_dcc, names_to = "variable", values_to = "value") |>
  count(in_dcc, variable, value) |>
  group_by(in_dcc, variable) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(label = ifelse(value, "TRUE", "FALSE"))

# Plot with improved layout
ggplot(plot_data, aes(x = in_dcc, y = label, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Heatmap of Binary Variable Percentages by in_dcc Group",
    x = "In DCC",
    y = NULL,  # Remove y-axis title to clean it up
    fill = "%"
  ) +
  facet_wrap(~variable, scales = "free_y", strip.position = "top") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 13),
    panel.spacing = unit(1.2, "lines"),        # more space between facets
    strip.placement = "outside",               # move strip outside of axis
    strip.background = element_blank()
  )

# only TRUE

# Create plot_data: keep only TRUE values
binary_data <- df |>
  select(in_dcc, covid_related, human_data, data_availability_statement) |>
  pivot_longer(cols = -in_dcc, names_to = "variable", values_to = "value") |>
  dplyr::filter(value == TRUE) |>
  count(in_dcc, variable) |>
  group_by(variable) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

# Plot only TRUE-level percentages
ggplot(binary_data, aes(x = in_dcc, y = variable, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Proportion of TRUE Values by in_dcc Group",
    x = "In DCC",
    y = NULL,
    fill = "%"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 13)
  )


### ARCHIVE

descriptives <- variables |> 
  map_dfr(~{
    df |>
      dplyr::filter(!is.na(!!sym(.x))) |> 
      group_by(in_dcc, !!sym(.x)) |>
      summarise(n = n(), .groups = "drop") |>
      group_by(in_dcc) |>
      mutate(
        pct = round(n / sum(n) * 100, 1),
        variable = .x,
        value = as.character(!!sym(.x))
      ) |>
      select(variable, value, in_dcc, n, pct)
  })


# Prepare plot data
plot_data <- descriptives |>
  dplyr::filter(!is.na(value)) |>
  mutate(
    label = paste0(value),
    variable = factor(variable, levels = c("human_data", "covid_related"))  # Optional: order
  )

# Plot
ggplot(plot_data, aes(x = in_dcc, y = label, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Heatmap of Value Percentages by in_dcc Group",
    x = "in_dcc",
    y = "Value",
    fill = "%"
  ) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )


### COMBINED?

library(patchwork)

# Prepare data for the categorical variable "license"
license_data <- df |>
  select(in_dcc, license) |>
  dplyr::filter(!is.na(license)) |>
  count(in_dcc, license) |>
  group_by(license) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

# Plot
ggplot(license_data, aes(x = in_dcc, y = license, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "License Type Distribution by in_dcc Group",
    x = "In DCC",
    y = NULL,
    fill = "%"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 13)
  )


# p1: binary_data plot
p1 <- ggplot(binary_data, aes(x = in_dcc, y = variable, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Proportion of TRUE Values", x = "In DCC", y = NULL, fill = "%") +
  theme_minimal(base_size = 13)

# p2: license_data plot
p2 <- ggplot(license_data, aes(x = in_dcc, y = license, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "License Type Distribution", x = "In DCC", y = NULL, fill = "%") +
  theme_minimal(base_size = 13)

# Combine vertically
p1 / p2 + plot_layout(heights = c(1, 1.2))

