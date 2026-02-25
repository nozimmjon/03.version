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

cols <- c("#1b9e77", "#d95f02", "#7570b3", "#7570b3", "#e7298a")

first_source_col <- "2.2. Қўшимча маблағ зарур бўлганда биринчи бўлиб қаерга ёки кимга мурожаат қиласиз?"
reason_col <- "2.6. Қарз, кредит, насия нима сабабдан олинган?"

df <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(has_loan = as.integer(has_loan))
borrowers <- df |> filter(has_loan == 1)

required_cols <- c("has_loan", "region", "holat1", first_source_col, reason_col)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns in cleaned v2 dataset: ", paste(missing_cols, collapse = ", "))
}

save_chart <- function(plot, name, w = 10, h = 6) {
  ggsave(file.path(out_dir, name), plot = plot, width = w, height = h, dpi = 320)
}

# 9. First source when extra funds needed
chart9_data <- df |>
  filter(!is.na(.data[[first_source_col]]), .data[[first_source_col]] != "") |>
  count(source = .data[[first_source_col]], name = "n") |>
  mutate(pct = n / sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 6)

chart9 <- chart9_data |>
  ggplot(aes(x = n, y = fct_reorder(source, n))) +
  geom_col(fill = cols[1]) +
  geom_text(aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")), hjust = -0.1, size = 3.1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "First source when extra funds are needed",
    subtitle = "Top 6 response categories",
    x = "Number of respondents",
    y = NULL
  )
save_chart(chart9, "chart_09_first_source_ggplot2.png", 11, 6)

# 10. First source by region
source_simple <- function(x) {
  case_when(
    str_detect(x, "Банк") ~ "Bank",
    str_detect(x, "Оила|дўстлар|таниш") ~ "Family/friends",
    str_detect(x, "олмасдан|ечим") ~ "No borrowing",
    str_detect(x, "Расмий насия|Микромолия|Ломбард") ~ "Other formal",
    TRUE ~ "Other"
  )
}

chart10_data <- df |>
  filter(!is.na(.data[[first_source_col]]), .data[[first_source_col]] != "") |>
  mutate(source_simple = source_simple(.data[[first_source_col]])) |>
  count(region, source_simple, name = "n") |>
  group_by(region) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

chart10 <- chart10_data |>
  ggplot(aes(x = pct, y = fct_reorder(region, pct, .fun = sum), fill = source_simple)) +
  geom_col(position = "fill") +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "First source by region",
    subtitle = "Composition of source categories within each region",
    x = "Share",
    y = NULL,
    fill = "Source"
  )
save_chart(chart10, "chart_10_source_by_region_ggplot2.png", 13, 8)

# 11. Why informal borrowing
informal_cols <- names(df)[str_starts(names(df), fixed("2.2.а.")) & str_detect(names(df), fixed("/"))]
informal_n <- df |>
  select(all_of(informal_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(total = rowSums(across(everything()), na.rm = TRUE)) |>
  summarise(n = sum(total > 0)) |>
  pull(n)

chart11_data <- df |>
  select(all_of(informal_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(mask = rowSums(across(everything()), na.rm = TRUE) > 0) |>
  filter(mask) |>
  select(-mask) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "reason", values_to = "n") |>
  mutate(
    reason = str_remove(reason, "^2\\.2\\.а\\..*/"),
    pct = n / informal_n
  ) |>
  arrange(pct)

chart11 <- chart11_data |>
  ggplot(aes(x = pct, y = fct_reorder(reason, pct))) +
  geom_col(fill = cols[2]) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 0.1), " (N=", n, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Reasons for choosing informal borrowing", subtitle = paste0("Among respondents selecting informal sources (N=", informal_n, ")"), x = "Share", y = NULL)
save_chart(chart11, "chart_11_why_informal_ggplot2.png", 12, 7)

# 12. Why commercial banks
bank_cols <- names(df)[str_starts(names(df), fixed("2.2.б ")) & str_detect(names(df), fixed("/"))]
bank_n <- df |>
  select(all_of(bank_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(total = rowSums(across(everything()), na.rm = TRUE)) |>
  summarise(n = sum(total > 0)) |>
  pull(n)

chart12_data <- df |>
  select(all_of(bank_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  mutate(mask = rowSums(across(everything()), na.rm = TRUE) > 0) |>
  filter(mask) |>
  select(-mask) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "reason", values_to = "n") |>
  mutate(
    reason = str_remove(reason, "^2\\.2\\.б .*?/"),
    pct = n / bank_n
  ) |>
  arrange(pct)

chart12 <- chart12_data |>
  ggplot(aes(x = pct, y = fct_reorder(reason, pct))) +
  geom_col(fill = cols[1]) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 0.1), " (N=", n, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Reasons for choosing commercial banks", subtitle = paste0("Among respondents selecting bank sources (N=", bank_n, ")"), x = "Share", y = NULL)
save_chart(chart12, "chart_12_why_banks_ggplot2.png", 12, 7)

# 13. Credit purpose among borrowers
purpose_cols <- names(df)[str_starts(names(df), fixed("2.7. Ушбу қарзни")) & str_detect(names(df), fixed("/"))]
chart13_data <- borrowers |>
  select(all_of(purpose_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "purpose", values_to = "n") |>
  mutate(
    purpose = str_remove(purpose, "^2\\.7\\. Ушбу қарзни .*?/"),
    pct = n / nrow(borrowers)
  ) |>
  arrange(pct)

chart13 <- chart13_data |>
  ggplot(aes(x = pct, y = fct_reorder(purpose, pct))) +
  geom_col(fill = cols[3]) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 0.1), " (N=", n, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Credit purposes among borrowers", subtitle = paste0("Multiple-response; borrowers only (N=", nrow(borrowers), ")"), x = "Share", y = NULL)
save_chart(chart13, "chart_13_credit_purpose_ggplot2.png", 13, 8)

# 14. Borrowing reason
chart14_data <- borrowers |>
  filter(!is.na(.data[[reason_col]]), .data[[reason_col]] != "") |>
  count(reason = .data[[reason_col]], name = "n") |>
  mutate(pct = n / sum(n))

chart14 <- chart14_data |>
  ggplot(aes(x = fct_reorder(reason, n), y = n, fill = reason)) +
  geom_col(show.legend = FALSE, width = 0.65) +
  geom_text(aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")), vjust = -0.25, size = 3.3) +
  labs(title = "Main reason for taking loan/credit", x = NULL, y = "Number of respondents")
save_chart(chart14, "chart_14_borrowing_reason_ggplot2.png", 10, 6)

# 15. Top credit purposes by repayment status
status_groups <- df |>
  mutate(
    holat1_clean = coalesce(holat1, ""),
    repay_group = case_when(
      str_detect(holat1_clean, "NPL") ~ "NPL (3+ months)",
      str_detect(holat1_clean, "1-3 ой") ~ "1-3 month delay",
      str_detect(holat1_clean, "ўз вақтида") ~ "On-time",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(repay_group), has_loan == 1)

purpose_top8 <- chart13_data |>
  arrange(desc(n)) |>
  slice_head(n = 8) |>
  pull(purpose)

purpose_map <- tibble(raw = purpose_cols) |>
  mutate(purpose = str_remove(raw, "^2\\.7\\. Ушбу қарзни .*?/")) |>
  filter(purpose %in% purpose_top8)

chart15_data <- status_groups |>
  select(repay_group, all_of(purpose_map$raw)) |>
  pivot_longer(-repay_group, names_to = "raw", values_to = "value") |>
  mutate(value = as.numeric(value)) |>
  left_join(purpose_map, by = "raw") |>
  group_by(repay_group, purpose) |>
  summarise(pct = mean(value, na.rm = TRUE), .groups = "drop")

chart15 <- chart15_data |>
  ggplot(aes(x = purpose, y = pct, fill = repay_group)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Top credit purposes by repayment status", x = NULL, y = "Share within status", fill = "Repayment status") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_chart(chart15, "chart_15_purpose_by_status_ggplot2.png", 14, 8)

# 16. Decision factors
decision_cols <- names(df)[str_starts(names(df), fixed("2.10. Қарз (кредит, насия) олишга қарор қилишингизга нима таъсир кўрсатди?/"))]

chart16_data <- borrowers |>
  select(all_of(decision_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "factor", values_to = "n") |>
  mutate(
    factor = str_remove(factor, fixed("2.10. Қарз (кредит, насия) олишга қарор қилишингизга нима таъсир кўрсатди?/")),
    pct = n / nrow(borrowers)
  ) |>
  arrange(pct)

chart16 <- chart16_data |>
  ggplot(aes(x = pct, y = fct_reorder(factor, pct))) +
  geom_col(fill = cols[5]) +
  geom_text(aes(label = paste0(percent(pct, accuracy = 0.1), " (N=", n, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Factors influencing borrowing decisions", subtitle = paste0("Borrowers only (N=", nrow(borrowers), ")"), x = "Share", y = NULL)
save_chart(chart16, "chart_16_decision_factors_ggplot2.png", 12, 7)

message("Saved block 2 ggplot2 reproduction charts (09-16) to: ", out_dir)
