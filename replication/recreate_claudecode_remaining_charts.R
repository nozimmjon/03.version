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

status_levels <- c("NPL (3+ months)", "1-3 month delay", "On-time")
status_colors <- c("NPL (3+ months)" = "#e74c3c", "1-3 month delay" = "#f39c12", "On-time" = "#27ae60")

df <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(
    has_loan = as.integer(has_loan),
    is_npl = str_detect(holat1 %||% "", "NPL|3 ой"),
    repay_group = case_when(
      str_detect(holat1 %||% "", "NPL|3 ой") ~ "NPL (3+ months)",
      str_detect(holat1 %||% "", "1-3") ~ "1-3 month delay",
      str_detect(holat1 %||% "", "вақтида") ~ "On-time",
      TRUE ~ NA_character_
    )
  )

borrowers <- df |> filter(has_loan == 1)
fam <- borrowers |> filter(kredit_turi == "oilaviy")

req <- c(
  "2.4.2. Таҳминан оилавий даромадингизнинг қанча қисми (неча фоизи) қарзни қайтариш учун сарфланади?",
  "2.4.4. Даромадингиз ойма-ой қай даражада ўзгариб туради?",
  "3.4. Қарз (кредит, насия) олишдан аввал тузилган шартнома билан танишганмисиз?",
  "3.5. Қарз (кредит, насия) олишдан аввал унинг шартлари, шу жумладан, фоиз тўловларини тўлиқ тушунганмисиз?",
  "3.14 Қарз (кредит, насия) олганингиздан афсусланасизми?",
  "2.9. Қарз (кредит, насия) қайси кўринишда олинган?",
  "3.8. Сизнингча қарз (кредит, насия) тўловини неча кунга кечиктириш мумкин (нормал ҳолат) деб биласиз?",
  "3.12 Ўз вақтида кредитни тўлаш сизнинг кредит рейтингизни ошириши мумкинлиги ҳақидаги маълумот олиш тўлов интизомини оширишга рағбатлантирадими?",
  "3.13 Кечикиш кредит тарихига қандай таъсир қилишини биласизми?"
)
missing <- setdiff(req, names(df))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

save_chart <- function(p, f, w = 11, h = 6) ggsave(file.path(out_dir, f), p, width = w, height = h, dpi = 320)

col_dti <- req[1]
col_vol <- req[2]
col_contract <- req[3]
col_terms <- req[4]
col_regret <- req[5]
col_form <- req[6]
col_norm <- req[7]
col_crating <- req[8]
col_impact <- req[9]

# 17 DTI by repayment status
c17 <- borrowers |>
  filter(!is.na(repay_group), !is.na(.data[[col_dti]]), .data[[col_dti]] != "") |>
  count(repay_group, dti = .data[[col_dti]], name = "n") |>
  group_by(repay_group) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(dti, pct, fill = repay_group)) +
  geom_col(position = position_dodge(width = .75), width = .65) +
  scale_fill_manual(values = status_colors, breaks = status_levels) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "DTI distribution by repayment status", x = NULL, y = "Share", fill = "Status") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_chart(c17, "chart_17_dti_by_status_ggplot2.png", 12, 6)

# 18 Volatility by repayment status
c18 <- borrowers |>
  filter(!is.na(repay_group), !is.na(.data[[col_vol]]), .data[[col_vol]] != "") |>
  count(repay_group, volatility = .data[[col_vol]], name = "n") |>
  group_by(repay_group) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(volatility, pct, fill = repay_group)) +
  geom_col(position = position_dodge(width = .75), width = .65) +
  scale_fill_manual(values = status_colors, breaks = status_levels) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Income volatility by repayment status", x = NULL, y = "Share", fill = "Status") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_chart(c18, "chart_18_volatility_by_status_ggplot2.png", 12, 6)

# 19 NPL reasons (3.7)
q37_cols <- names(df)[str_starts(names(df), "3.7.") & str_detect(names(df), fixed("/"))]
c19 <- borrowers |>
  select(all_of(q37_cols)) |>
  mutate(across(everything(), as.numeric)) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "reason", values_to = "n") |>
  mutate(reason = str_remove(reason, "^3\\.7\\. .*?/"), pct = n / nrow(borrowers)) |>
  arrange(pct) |>
  ggplot(aes(pct, fct_reorder(reason, pct))) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = paste0(percent(pct, .1), " (N=", n, ")")), hjust = -0.1, size = 2.8) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0, .2))) +
  labs(title = "Reasons for late/non-payment (Q3.7)", x = "Share of borrowers", y = NULL)
save_chart(c19, "chart_19_npl_reasons_ggplot2.png", 13, 8)

plot_binary_by_status <- function(col_name, title, fname){
  d <- borrowers |>
    filter(!is.na(repay_group), !is.na(.data[[col_name]]), .data[[col_name]] != "") |>
    count(repay_group, response = .data[[col_name]], name = "n") |>
    group_by(repay_group) |>
    mutate(pct = n / sum(n)) |>
    ungroup()
  p <- ggplot(d, aes(repay_group, pct, fill = response)) +
    geom_col(width = .65) +
    geom_text(aes(label = percent(pct, .1)), position = position_stack(vjust=.5), color = "white", size = 3) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = title, x = NULL, y = "Share", fill = NULL)
  save_chart(p, fname, 10, 6)
}

# 20-23
plot_binary_by_status(col_contract, "Read contract by repayment status", "chart_20_contract_by_status_ggplot2.png")
plot_binary_by_status(col_terms, "Understood terms by repayment status", "chart_21_terms_by_status_ggplot2.png")
plot_binary_by_status(col_regret, "Regret after borrowing by repayment status", "chart_22_regret_by_status_ggplot2.png")

c23 <- borrowers |>
  filter(!is.na(.data[[col_form]]), .data[[col_form]] != "", !is.na(.data[[col_contract]]), .data[[col_contract]] != "") |>
  count(form = .data[[col_form]], read_contract = .data[[col_contract]], name = "n") |>
  group_by(form) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(form, pct, fill = read_contract)) +
  geom_col(width = .65) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Online vs offline: contract awareness", x = NULL, y = "Share", fill = NULL)
save_chart(c23, "chart_23_online_vs_offline_ggplot2.png", 10, 6)

# 25 collection methods by status (Q3.9 multi)
q39_cols <- names(df)[str_starts(names(df), "3.9.") & str_detect(names(df), fixed("/"))]
c25 <- borrowers |>
  filter(!is.na(repay_group)) |>
  select(repay_group, all_of(q39_cols)) |>
  pivot_longer(-repay_group, names_to = "method", values_to = "v") |>
  mutate(v = as.numeric(v), method = str_remove(method, "^3\\.9\\. .*?/")) |>
  group_by(repay_group, method) |>
  summarise(pct = mean(v, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(method, pct, fill = repay_group)) +
  geom_col(position = position_dodge(width=.75), width=.65) +
  scale_fill_manual(values = status_colors, breaks = status_levels) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Collection/payment priority methods by status", x = NULL, y = "Share", fill = "Status") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
save_chart(c25, "chart_25_collection_by_status_ggplot2.png", 14, 8)

# 26 effective reminders (use Q3.10/3.11 proxies if present)
rem_cols <- names(df)[str_starts(names(df), "3.10") | str_starts(names(df), "3.11")]
if (length(rem_cols) > 0) {
  c26 <- borrowers |>
    select(all_of(rem_cols)) |>
    pivot_longer(everything(), names_to = "factor", values_to = "v") |>
    mutate(v = as.numeric(v)) |>
    group_by(factor) |>
    summarise(pct = mean(v, na.rm = TRUE), .groups = "drop") |>
    mutate(factor = str_remove(factor, "^3\\.(10|11)\\.?")) |>
    ggplot(aes(pct, fct_reorder(factor, pct))) +
    geom_col(fill = "#16a085") +
    scale_x_continuous(labels = percent_format()) +
    labs(title = "Effective reminders / relationship factors", x = "Share", y = NULL)
  save_chart(c26, "chart_26_effective_reminders_ggplot2.png", 12, 6)
}

# 27-30 family credit
q27b_cols <- names(df)[str_starts(names(df), "2.7.б.") & str_detect(names(df), fixed("/"))]
q27v_cols <- names(df)[str_starts(names(df), "2.7.в.") & str_detect(names(df), fixed("/"))]
q27a_cols <- names(df)[str_starts(names(df), "2.7.a ") & str_detect(names(df), fixed("/"))]

if (nrow(fam) > 0) {
  c27 <- fam |>
    select(all_of(q27b_cols)) |>
    summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "reason", values_to = "n") |>
    mutate(reason = str_remove(reason, "^2\\.7\\.б\\. .*?/"), pct = n / nrow(fam)) |>
    arrange(pct) |>
    ggplot(aes(pct, fct_reorder(reason, pct))) + geom_col(fill = "#c0392b") +
    geom_text(aes(label = paste0(percent(pct, .1), " (N=", n, ")")), hjust = -0.1, size = 2.8) +
    scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0,.2))) +
    labs(title = "Family credit: NPL reasons", x = "Share", y = NULL)
  save_chart(c27, "chart_27_family_npl_reasons_ggplot2.png", 12, 7)

  c28 <- fam |>
    select(all_of(q27v_cols)) |>
    summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "support", values_to = "n") |>
    mutate(support = str_remove(support, "^2\\.7\\.в\\. .*?/"), pct = n / nrow(fam)) |>
    arrange(pct) |>
    ggplot(aes(pct, fct_reorder(support, pct))) + geom_col(fill = "#2980b9") +
    geom_text(aes(label = paste0(percent(pct, .1), " (N=", n, ")")), hjust = -0.1, size = 2.8) +
    scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0,.2))) +
    labs(title = "Family credit: needed support measures", x = "Share", y = NULL)
  save_chart(c28, "chart_28_family_support_ggplot2.png", 12, 7)

  c29 <- borrowers |>
    group_by(kredit_turi) |>
    summarise(n = n(), npl_rate = mean(is_npl, na.rm = TRUE), .groups = "drop") |>
    arrange(npl_rate) |>
    ggplot(aes(npl_rate, fct_reorder(kredit_turi, npl_rate), fill = npl_rate)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(percent(npl_rate, .1), " (N=", n, ")")), hjust = -0.1, size = 3) +
    scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0,.2))) +
    labs(title = "NPL rate by credit type", x = "NPL rate", y = NULL)
  save_chart(c29, "chart_29_npl_by_credit_type_ggplot2.png", 11, 6)

  c30 <- fam |>
    select(all_of(q27a_cols)) |>
    summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "reason", values_to = "n") |>
    mutate(reason = str_remove(reason, "^2\\.7\\.a .*?/"), pct = n / nrow(fam)) |>
    arrange(pct) |>
    ggplot(aes(pct, fct_reorder(reason, pct))) + geom_col(fill = "#27ae60") +
    geom_text(aes(label = paste0(percent(pct, .1), " (N=", n, ")")), hjust = -0.1, size = 2.8) +
    scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0,.2))) +
    labs(title = "Reasons for taking family credit", x = "Share", y = NULL)
  save_chart(c30, "chart_30_why_family_credit_ggplot2.png", 12, 7)
}

# 33 detailed NPL causes (subset of Q3.7 among NPL)
c33 <- borrowers |>
  filter(is_npl) |>
  select(all_of(q37_cols)) |>
  summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "cause", values_to = "n") |>
  mutate(cause = str_remove(cause, "^3\\.7\\. .*?/"), pct = n / sum(borrowers$is_npl, na.rm = TRUE)) |>
  arrange(pct) |>
  ggplot(aes(pct, fct_reorder(cause, pct))) + geom_col(fill = "#9b59b6") +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Detailed NPL causes (NPL borrowers)", x = "Share", y = NULL)
save_chart(c33, "chart_33_npl_causes_detailed_ggplot2.png", 13, 8)

# 34 decision factors
q210_cols <- names(df)[str_starts(names(df), "2.10.") & str_detect(names(df), fixed("/"))]
c34 <- borrowers |>
  select(all_of(q210_cols)) |>
  summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "factor", values_to = "n") |>
  mutate(factor = str_remove(factor, "^2\\.10\\. .*?/"), pct = n / nrow(borrowers)) |>
  arrange(pct) |>
  ggplot(aes(pct, fct_reorder(factor, pct))) + geom_col(fill = "#34495e") +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Decision factors for borrowing", x = "Share", y = NULL)
save_chart(c34, "chart_34_decision_factors_ggplot2.png", 12, 7)

# 35 credit rating awareness
c35 <- borrowers |>
  select(all_of(c(col_crating, col_impact)), is_npl) |>
  pivot_longer(all_of(c(col_crating, col_impact)), names_to = "question", values_to = "response") |>
  filter(!is.na(response), response != "") |>
  count(question, response, is_npl, name = "n") |>
  group_by(question, is_npl) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  mutate(group = if_else(is_npl, "NPL", "Non-NPL"), question = str_remove(question, "^3\\.(12|13)\\s*")) |>
  ggplot(aes(response, pct, fill = group)) +
  geom_col(position = position_dodge(width = .75), width = .65) +
  facet_wrap(~question, scales = "free_x") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Credit rating awareness and impact knowledge", x = NULL, y = "Share", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_chart(c35, "chart_35_credit_rating_awareness_ggplot2.png", 14, 7)

# 36 purpose x npl
q27_cols <- names(df)[str_starts(names(df), "2.7. Ушбу қарзни") & str_detect(names(df), fixed("/"))]
c36 <- borrowers |>
  select(all_of(q27_cols), is_npl) |>
  pivot_longer(all_of(q27_cols), names_to = "purpose", values_to = "v") |>
  mutate(v = as.numeric(v), purpose = str_remove(purpose, "^2\\.7\\. Ушбу қарзни .*?/")) |>
  group_by(purpose) |>
  summarise(n = sum(v == 1, na.rm = TRUE), npl_rate = mean(is_npl[v == 1], na.rm = TRUE), .groups = "drop") |>
  filter(n > 0) |>
  arrange(npl_rate) |>
  ggplot(aes(npl_rate, fct_reorder(purpose, npl_rate))) +
  geom_col(fill = "#e67e22") +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "NPL rate by credit purpose", x = "NPL rate", y = NULL)
save_chart(c36, "chart_36_purpose_npl_ggplot2.png", 12, 7)

# 37 payment norms x npl
c37 <- borrowers |>
  filter(!is.na(.data[[col_norm]]), .data[[col_norm]] != "") |>
  count(norm = .data[[col_norm]], is_npl, name = "n") |>
  group_by(norm) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(group = if_else(is_npl, "NPL", "Non-NPL")) |>
  ggplot(aes(norm, pct, fill = group)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Payment norms and NPL", x = NULL, y = "Composition", fill = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_chart(c37, "chart_37_payment_norms_ggplot2.png", 12, 6)

# 38 payment priorities
c38 <- borrowers |>
  select(all_of(q39_cols)) |>
  summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "priority", values_to = "n") |>
  mutate(priority = str_remove(priority, "^3\\.9\\. .*?/"), pct = n / nrow(borrowers)) |>
  arrange(pct) |>
  ggplot(aes(pct, fct_reorder(priority, pct))) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = paste0(percent(pct, .1), " (N=", n, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(), expand = expansion(mult = c(0,.2))) +
  labs(title = "Payment priorities (first to repay)", x = "Share", y = NULL)
save_chart(c38, "chart_38_payment_priorities_ggplot2.png", 12, 6)

message("Saved remaining ggplot2 reproduction charts (17-23,25-30,33-38) to: ", out_dir)
