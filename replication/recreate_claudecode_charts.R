library(tidyverse)
library(readr)
library(scales)

input_file <- "outputs_prep_v2/survey_master_cleaned_v2.csv"
out_dir <- "replication/charts_ggplot2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

cols <- c("#1b9e77", "#d95f02", "#7570b3")

age_col <- "1.1. Ёшингиз:"
gender_col <- "1.2. Респондент жинси"
edu_col <- "1.4. Маълумотингиз:"
income_col <- "1.10. Уй хўжалигингиз аъзоларининг жами ойлик даромади?"

required_cols <- c("has_loan", "region", "kredit_turi", "holat1", age_col, gender_col, edu_col, income_col)

df <- read_csv(input_file, show_col_types = FALSE)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns in cleaned v2 dataset: ", paste(missing_cols, collapse = ", "))
}

df <- df |> mutate(has_loan = as.integer(has_loan))
borrowers <- df |> filter(has_loan == 1)

save_chart <- function(plot, name, w = 10, h = 6) {
  ggsave(file.path(out_dir, name), plot = plot, width = w, height = h, dpi = 320)
}

# 1. Regional distribution
chart1 <- df |>
  count(region, has_loan, name = "n") |>
  mutate(status = if_else(has_loan == 1, "Кредити мавжудлар", "Кредитга эга бўлмаганлар")) |>
  ggplot(aes(x = n, y = fct_reorder(region, n, .fun = sum), fill = status)) +
  geom_col(position = "stack") +
  geom_text(
    data = ~ .x |> group_by(region) |> summarise(total = sum(n), .groups = "drop"),
    aes(x = total + 5, y = region, label = total),
    inherit.aes = FALSE,
    size = 3.2
  ) +
  scale_fill_manual(values = cols[1:2]) +
  labs(title = "Респондентларнинг ҳудудий тақсимланиши", x = "Респондентлар сони", y = NULL, fill = NULL) +
  coord_cartesian(clip = "off")
save_chart(chart1, "chart_01_regional_distribution_ggplot2.png", 12, 7)

# 2. Age distribution
age_labels <- c("18-25", "26-35", "36-45", "46-55", "56+")
chart2_data <- df |>
  mutate(
    age_group = cut(.data[[age_col]], breaks = c(0, 25, 35, 45, 55, Inf), labels = age_labels, right = TRUE),
    status = if_else(has_loan == 1, "Кредити мавжудлар", "Кредитга эга бўлмаганлар")
  ) |>
  count(age_group, status, name = "n")

chart2 <- chart2_data |>
  ggplot(aes(x = age_group, y = n, fill = status)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75), vjust = -0.2, size = 3.1) +
  scale_fill_manual(values = cols[1:2]) +
  labs(title = "Age group distribution", subtitle = "Borrowers vs. non-borrowers", x = "Age group", y = "Number of respondents", fill = NULL)
save_chart(chart2, "chart_02_age_distribution_ggplot2.png", 10, 6)

# 3. Gender composition
chart3_data <- df |>
  mutate(gender = .data[[gender_col]], status = if_else(has_loan == 1, "Кредити мавжудлар", "Кредитга эга бўлмаганлар")) |>
  filter(!is.na(gender), gender != "") |>
  count(status, gender, name = "n") |>
  group_by(status) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

chart3 <- chart3_data |>
  ggplot(aes(x = status, y = pct, fill = gender)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), position = position_stack(vjust = 0.5), color = "white", size = 3.2) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Gender composition by borrowing status", subtitle = "100% stacked bars for clearer comparison", x = NULL, y = "Share of respondents", fill = "Gender")
save_chart(chart3, "chart_03_gender_ggplot2.png", 9, 6)

# 4. Income distribution
income_order <- c("5 млн сўмгача", "5-10 млн сўм", "10 - 20 млн сўм", "20-50 млн сўм", "50 млн сўмдан юқори")
chart4_data <- df |>
  mutate(income = factor(.data[[income_col]], levels = income_order), status = if_else(has_loan == 1, "Кредити мавжудлар", "Кредитга эга бўлмаганлар")) |>
  filter(!is.na(income)) |>
  count(status, income, name = "n") |>
  group_by(status) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

chart4 <- chart4_data |>
  ggplot(aes(x = income, y = pct, fill = status)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), position = position_dodge(width = 0.75), vjust = -0.25, size = 3) +
  scale_fill_manual(values = cols[1:2]) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Household monthly income distribution", x = "Income bracket", y = "Share of respondents", fill = NULL)
save_chart(chart4, "chart_04_income_ggplot2.png", 11, 6)

# 5. Credit type
chart5_data <- borrowers |>
  count(kredit_turi, name = "n") |>
  mutate(pct = n / sum(n)) |>
  arrange(pct)

chart5 <- chart5_data |>
  ggplot(aes(x = pct, y = fct_reorder(kredit_turi, pct))) +
  geom_col(fill = cols[3]) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), hjust = -0.1, size = 3.1) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Distribution of credit types among borrowers", x = "Share of borrowers", y = NULL)
save_chart(chart5, "chart_05_credit_type_ggplot2.png", 10, 6)

# 6. Repayment status
chart6_data <- df |>
  filter(!is.na(holat1), holat1 != "") |>
  count(holat1, name = "n") |>
  mutate(pct = n / sum(n))

chart6 <- chart6_data |>
  ggplot(aes(x = fct_reorder(holat1, n), y = n, fill = holat1)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")), vjust = -0.35, size = 3) +
  scale_fill_manual(values = c("#2ca25f", "#f39c12", "#e74c3c")) +
  labs(title = "Repayment status distribution", x = NULL, y = "Number of respondents") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_chart(chart6, "chart_06_repayment_status_ggplot2.png", 12, 6)

# 7. Income sources comparison
income_sources <- names(df)[str_detect(names(df), fixed("1.9. Сиз асосан нималардан доимий даромад топасиз?/") )]

chart7_data <- df |>
  mutate(status = if_else(has_loan == 1, "Кредити мавжудлар", "Кредитга эга бўлмаганлар")) |>
  select(status, all_of(income_sources)) |>
  pivot_longer(-status, names_to = "source", values_to = "value") |>
  mutate(value = as.numeric(value), source = str_remove(source, "^1\\.9\\. Сиз асосан нималардан доимий даромад топасиз\\?/")) |>
  group_by(status, source) |>
  summarise(pct = mean(value, na.rm = TRUE), .groups = "drop")

chart7 <- chart7_data |>
  ggplot(aes(x = pct, y = fct_reorder(source, pct), fill = status)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_x_continuous(labels = percent_format()) +
  scale_fill_manual(values = cols[1:2]) +
  labs(title = "Income sources used by respondents", subtitle = "Multiple-response question", x = "Share selecting source", y = NULL, fill = NULL)
save_chart(chart7, "chart_07_income_sources_ggplot2.png", 12, 7)

# 8. Education
chart8_data <- df |>
  mutate(education = .data[[edu_col]], status = if_else(has_loan == 1, "Кредити мавжудлар", "Кредитга эга бўлмаганлар")) |>
  filter(!is.na(education), education != "") |>
  count(status, education, name = "n") |>
  group_by(status) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

chart8 <- chart8_data |>
  ggplot(aes(x = status, y = pct, fill = education)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Education level by borrowing status", x = NULL, y = "Share of respondents", fill = "Education")
save_chart(chart8, "chart_08_education_ggplot2.png", 10, 6)

message("Saved 8 ggplot2 reproduction charts to: ", out_dir)
