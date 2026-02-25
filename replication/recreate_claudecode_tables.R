library(tidyverse)
library(readr)
library(yaml)

input_file <- "outputs_prep_v2/survey_master_cleaned_v2.csv"
registry_file <- "replication/table_registry.yml"
out_dir <- "outputs/tables/replication"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

registry <- yaml::read_yaml(registry_file)
df <- read_csv(input_file, show_col_types = FALSE)

df <- df |>
  mutate(
    has_loan = as.integer(has_loan),
    holat1_clean = coalesce(holat1, ""),
    repay_group = case_when(
      str_detect(holat1_clean, "NPL") ~ "NPL (3+ months)",
      str_detect(holat1_clean, "1-3 ой") ~ "1-3 month delay",
      str_detect(holat1_clean, "ўз вақтида") ~ "On-time",
      TRUE ~ NA_character_
    ),
    is_npl = as.integer(repay_group == "NPL (3+ months)")
  )

borrowers <- df |> filter(has_loan == 1)
fam <- borrowers |> filter(kredit_turi == "oilaviy")

col_income <- "1.10. Уй хўжалигингиз аъзоларининг жами ойлик даромади?"
col_contract <- "3.4. Қарз (кредит, насия) олишдан аввал тузилган шартнома билан танишганмисиз?"
col_hh <- "1.6 Уй хўжалигингиз жами аъзолари сони нечта?"
col_employed <- "1.7. Уй хўжалигингизда неча киши даромадли меҳнат (иш) билан банд?"
col_children <- "1.8. 18 ёшга етмаган фарзандларингиз сони?"
col_dti <- "2.4.2. Таҳминан оилавий даромадингизнинг қанча қисми (неча фоизи) қарзни қайтариш учун сарфланади?"
col_vol <- "2.4.4. Даромадингиз ойма-ой қай даражада ўзгариб туради?"

q37_cols <- names(df)[str_starts(names(df), "3.7.") & str_detect(names(df), fixed("/"))]
q39_cols <- names(df)[str_starts(names(df), "3.9.") & str_detect(names(df), fixed("/"))]
q27b_cols <- names(df)[str_starts(names(df), "2.7.б.") & str_detect(names(df), fixed("/"))]

required <- c(col_income, col_contract, col_hh, col_employed, col_children, col_dti, col_vol, "kredit_turi", "holat1", "region")
missing <- setdiff(required, names(df))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

style_gt <- function(tbl, title) {
  if (!requireNamespace("gt", quietly = TRUE)) return(NULL)

  num_cols <- names(tbl)[map_lgl(tbl, is.numeric)]
  pct_cols <- names(tbl)[str_detect(names(tbl), "share|pct|rate|accuracy|auc") & names(tbl) %in% num_cols]

  g <- gt::gt(tbl) |>
    gt::cols_label_with(fn = ~ .x |> str_replace_all("_", " ") |> str_to_title()) |>
    gt::tab_header(title = gt::md(paste0("**", title, "**"))) |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.font.names = "Source Sans Pro",
      table.font.size = gt::px(13),
      heading.title.font.size = gt::px(18),
      heading.align = "left",
      column_labels.font.weight = "bold",
      data_row.padding = gt::px(5),
      table.border.top.style = "solid",
      table.border.top.color = "#2F3E46",
      table.border.bottom.color = "#2F3E46"
    ) |>
    gt::tab_source_note(source_note = gt::md("_Source: cleaned v2 pipeline data_"))

  if (length(num_cols) > 0) {
    g <- g |> gt::cols_align(align = "right", columns = all_of(num_cols))
  }
  chr_cols <- names(tbl)[map_lgl(tbl, is.character)]
  if (length(chr_cols) > 0) {
    g <- g |> gt::cols_align(align = "left", columns = all_of(chr_cols))
  }

  non_pct_num <- setdiff(num_cols, pct_cols)
  if (length(non_pct_num) > 0) {
    g <- g |> gt::fmt_number(columns = all_of(non_pct_num), decimals = 2, sep_mark = ",")
  }

  if (length(pct_cols) > 0) {
    for (pc in pct_cols) {
      vmax <- suppressWarnings(max(tbl[[pc]], na.rm = TRUE))
      if (is.finite(vmax) && vmax <= 1.5) {
        g <- g |> gt::fmt_percent(columns = all_of(pc), decimals = 1)
      } else {
        g <- g |> gt::fmt_number(columns = all_of(pc), decimals = 1)
      }
    }

    g <- g |> gt::data_color(
      columns = all_of(pct_cols),
      fn = scales::col_numeric(c("#F1FAEE", "#1D3557"), domain = NULL)
    )
  }

  g
}

save_table <- function(tbl, file_stub, title) {
  csv_path <- file.path(out_dir, paste0(file_stub, ".csv"))
  write_csv(tbl, csv_path)

  g <- style_gt(tbl, title)
  if (!is.null(g)) {
    html_path <- file.path(out_dir, paste0(file_stub, ".html"))
    gt::gtsave(g, html_path, inline_css = TRUE)
  }

  invisible(csv_path)
}

save_table(
  df |>
    filter(!is.na(.data[[col_income]]), .data[[col_income]] != "") |>
    count(income_level = .data[[col_income]], name = "n") |>
    mutate(share = n / sum(n)),
  "table_01_income_levels",
  "Table 01. Respondent income levels"
)

save_table(
  borrowers |>
    select(all_of(q37_cols), is_npl) |>
    pivot_longer(all_of(q37_cols), names_to = "reason", values_to = "v") |>
    mutate(v = as.numeric(v), reason = str_remove(reason, "^3\\.7\\. .*?/")) |>
    group_by(reason) |>
    summarise(
      n_total = sum(v == 1, na.rm = TRUE),
      n_npl = sum(v == 1 & is_npl == 1, na.rm = TRUE),
      pct_npl = n_npl / sum(is_npl == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(category = case_when(
      str_detect(reason, "Даромад|мавсум") ~ "Economic",
      str_detect(reason, "нотўғри|қарзларга") ~ "Behavioral",
      str_detect(reason, "Банк|шарт|рухсат") ~ "Institutional",
      TRUE ~ "Other"
    )) |>
    arrange(desc(n_npl)),
  "table_02_npl_cause_categories",
  "Table 02. Detailed NPL cause categories"
)

save_table(
  borrowers |>
    filter(!is.na(repay_group), !is.na(.data[[col_contract]]), .data[[col_contract]] != "") |>
    count(repay_group, contract_familiarity = .data[[col_contract]], name = "n") |>
    group_by(repay_group) |>
    mutate(share = n / sum(n)) |>
    ungroup(),
  "table_03_contract_familiarity",
  "Table 03. Contract familiarity by repayment group"
)

save_table(
  borrowers |>
    filter(!is.na(repay_group)) |>
    select(repay_group, all_of(q39_cols)) |>
    pivot_longer(-repay_group, names_to = "method", values_to = "v") |>
    mutate(v = as.numeric(v), method = str_remove(method, "^3\\.9\\. .*?/")) |>
    group_by(repay_group, method) |>
    summarise(share = mean(v, na.rm = TRUE), .groups = "drop"),
  "table_04_collection_methods_by_status",
  "Table 04. Collection/payment methods by status"
)

save_table(
  fam |>
    select(all_of(q27b_cols)) |>
    summarise(across(everything(), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "reason", values_to = "n") |>
    mutate(reason = str_remove(reason, "^2\\.7\\.б\\. .*?/"), share = n / nrow(fam)) |>
    arrange(desc(share)),
  "table_05_family_nonpayment_reasons",
  "Table 05. Family entrepreneurship non-payment reasons"
)

save_table(
  tibble(
    metric = c("Total respondents", "Borrowers", "Non-borrowers", "Regions"),
    value = c(nrow(df), sum(df$has_loan == 1, na.rm = TRUE), sum(df$has_loan == 0, na.rm = TRUE), n_distinct(df$region))
  ),
  "table_06_research_parameters",
  "Table 06. Key research parameters"
)

save_table(
  borrowers |>
    filter(!is.na(kredit_turi), kredit_turi != "") |>
    count(kredit_turi, name = "n") |>
    mutate(share = n / sum(n)) |>
    arrange(desc(n)),
  "table_07_credit_type_distribution",
  "Table 07. Credit type distribution"
)

save_table(
  borrowers |>
    filter(!is.na(repay_group)) |>
    count(repay_group, name = "n") |>
    mutate(share = n / sum(n)),
  "table_08_payment_status_distribution",
  "Table 08. Payment status distribution"
)

save_table(
  df |>
    mutate(group = if_else(has_loan == 1, "Borrowers", "Non-borrowers")) |>
    group_by(group) |>
    summarise(
      n = n(),
      mean_age = mean(as.numeric(`1.1. Ёшингиз:`), na.rm = TRUE),
      mean_hh_size = mean(as.numeric(.data[[col_hh]]), na.rm = TRUE),
      mean_income_earners = mean(as.numeric(.data[[col_employed]]), na.rm = TRUE),
      mean_children = mean(as.numeric(.data[[col_children]]), na.rm = TRUE),
      .groups = "drop"
    ),
  "table_09_demographic_profile",
  "Table 09. Demographic profile"
)

model_df <- borrowers |>
  transmute(
    is_npl = is_npl,
    hh_size = as.numeric(.data[[col_hh]]),
    employed_members = as.numeric(.data[[col_employed]]),
    children = as.numeric(.data[[col_children]]),
    dti = as.numeric(factor(.data[[col_dti]])),
    volatility = as.numeric(factor(.data[[col_vol]])),
    read_contract = as.numeric(factor(.data[[col_contract]]))
  ) |>
  drop_na()

fit <- glm(is_npl ~ hh_size + employed_members + children + dti + volatility + read_contract,
           data = model_df, family = binomial())

pred <- as.numeric(predict(fit, type = "response") > 0.5)
acc <- mean(pred == model_df$is_npl)

rank_auc <- function(y, score) {
  pos <- score[y == 1]
  neg <- score[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  mean(outer(pos, neg, ">")) + 0.5 * mean(outer(pos, neg, "=="))
}
auc <- rank_auc(model_df$is_npl, predict(fit, type = "response"))

save_table(
  tibble(
    metric = c("observations", "accuracy", "auc", "n_parameters"),
    value = c(nrow(model_df), acc, auc, length(coef(fit)))
  ),
  "table_10_model_quality",
  "Table 10. Model quality"
)

save_table(
  model_df |>
    summarise(across(everything(), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE)) |>
    pivot_longer(everything(), names_to = "variable_stat", values_to = "value"),
  "table_11_statistical_summary",
  "Table 11. Statistical summary"
)

coef_mat <- summary(fit)$coefficients
coef_tbl <- tibble(
  term = rownames(coef_mat),
  estimate = coef_mat[, "Estimate"],
  std_error = coef_mat[, "Std. Error"],
  z_value = coef_mat[, "z value"],
  p_value = coef_mat[, "Pr(>|z|)"],
  odds_ratio = exp(estimate)
)
save_table(coef_tbl, "table_12_econometric_results", "Table 12. Econometric results")

save_table(
  as_tibble(cor(select(model_df, -is_npl), use = "pairwise.complete.obs"), rownames = "variable"),
  "table_13_correlation_matrix",
  "Table 13. Correlation matrix"
)

save_table(
  tibble(
    check = c("registry_tables", "q37_multi_count", "q39_multi_count", "q27b_multi_count", "model_rows"),
    value = c(length(registry$tables), length(q37_cols), length(q39_cols), length(q27b_cols), nrow(model_df))
  ),
  "table_validation_log",
  "Validation log"
)

message("Saved styled table replication outputs to: ", out_dir)
