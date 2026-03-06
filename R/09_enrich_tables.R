# =============================================================================
# 09_enrich_tables.R  --  Enrichment tables for final report expansion
# =============================================================================
# Input:   data/intermediate/04_analytical.rds
# Output:  outputs/tables/enrichment/H*.csv
#          outputs/tables/enrichment/ENRICHMENT_TABLES.xlsx (optional)
#          logs/09_enrich_tables.log
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(here)
})

# -- 0. Paths -----------------------------------------------------------------
in_rds    <- here("data", "intermediate", "04_analytical.rds")
out_dir   <- here("outputs", "tables", "enrichment")
log_file  <- here("logs", "09_enrich_tables.log")
out_xlsx  <- file.path(out_dir, "ENRICHMENT_TABLES.xlsx")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(in_rds))
writeLines("", log_file)

tee <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

write_tbl <- function(tbl, stem) {
  path <- file.path(out_dir, paste0(stem, ".csv"))
  write_csv(tbl, path, na = "")
  tee(sprintf("  [OK] %s  (%d rows)", basename(path), nrow(tbl)))
  invisible(path)
}

clean_label <- function(x) {
  out <- str_extract(x, "[^/]+$")
  out <- str_replace_all(out, "\\s+", " ")
  str_trim(out)
}

binary_acceptance_table <- function(data, cols, group_var = NULL, item_col = "item") {
  if (length(cols) == 0) return(tibble())

  if (is.null(group_var)) {
    out <- data %>%
      select(all_of(cols)) %>%
      pivot_longer(cols = everything(), names_to = "col_name", values_to = "value") %>%
      mutate(!!item_col := clean_label(col_name)) %>%
      group_by(.data[[item_col]]) %>%
      summarise(
        n_valid = sum(!is.na(value)),
        n_yes   = sum(value == 1L, na.rm = TRUE),
        pct_yes = round(100 * n_yes / pmax(n_valid, 1), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(pct_yes))
    return(out)
  }

  if (!group_var %in% names(data)) return(tibble())

  out <- data %>%
    select(all_of(c(group_var, cols))) %>%
    filter(!is.na(.data[[group_var]])) %>%
    pivot_longer(cols = all_of(cols), names_to = "col_name", values_to = "value") %>%
    mutate(!!item_col := clean_label(col_name)) %>%
    group_by(.data[[group_var]], .data[[item_col]]) %>%
    summarise(
      n_valid = sum(!is.na(value)),
      n_yes   = sum(value == 1L, na.rm = TRUE),
      pct_yes = round(100 * n_yes / pmax(n_valid, 1), 1),
      .groups = "drop"
    ) %>%
    arrange(.data[[item_col]], .data[[group_var]])
  out
}

tee(sprintf("=== 09_enrich_tables.R | %s ===", format(Sys.time(), "%Y-%m-%d %H:%M")))

# -- 1. Load ------------------------------------------------------------------
df <- readRDS(in_rds)
borrowers <- df %>% filter(has_loan == 1L)
tee(sprintf("Loaded 04_analytical.rds: %d rows x %d cols", nrow(df), ncol(df)))
tee(sprintf("Borrowers subset: %d rows", nrow(borrowers)))

tables <- list()

# -- H1. Q2.1: Normative acceptability by repayment status --------------------
tee("\nH1: Q2.1 normative acceptability")
q21_cols <- names(df)[str_detect(names(df), "^\\d+-2\\.1")]

if (length(q21_cols) > 0) {
  h1_overall <- binary_acceptance_table(df, q21_cols, group_var = NULL, item_col = "borrowing_purpose")
  h1_overall <- h1_overall %>% mutate(segment = "Overall") %>% relocate(segment)
  tables[["H1A_q21_acceptability_overall"]] <- h1_overall
  write_tbl(h1_overall, "H1A_q21_acceptability_overall")

  h1_status <- binary_acceptance_table(
    borrowers,
    q21_cols,
    group_var = "repay_group",
    item_col = "borrowing_purpose"
  ) %>%
    rename(repayment_status = repay_group)

  if (nrow(h1_status) > 0) {
    tables[["H1B_q21_acceptability_by_repayment"]] <- h1_status
    write_tbl(h1_status, "H1B_q21_acceptability_by_repayment")
  } else {
    tee("  [SKIP] repay_group not available for H1B")
  }
} else {
  tee("  [SKIP] Q2.1 matrix columns not found")
}

# -- H2. Q2.3: Preferred source by amount bands -------------------------------
tee("\nH2: Q2.3 preferred source by loan-size willingness")
q23_cols <- names(df)[str_detect(names(df), "^\\d+-2\\.3")]

if (length(q23_cols) > 0) {
  h2 <- df %>%
    select(all_of(q23_cols)) %>%
    pivot_longer(cols = everything(), names_to = "col_name", values_to = "amount_mln") %>%
    mutate(source = clean_label(col_name)) %>%
    group_by(source) %>%
    summarise(
      n_valid            = sum(!is.na(amount_mln)),
      pct_willing        = round(100 * mean(amount_mln > 0, na.rm = TRUE), 1),
      mean_mln_all       = round(mean(amount_mln, na.rm = TRUE), 2),
      mean_mln_if_willing = round(mean(amount_mln[amount_mln > 0], na.rm = TRUE), 2),
      median_mln_if_willing = round(median(amount_mln[amount_mln > 0], na.rm = TRUE), 2),
      p75_mln_if_willing = round(quantile(amount_mln[amount_mln > 0], 0.75, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_mln_if_willing))

  tables[["H2_q23_source_amount_summary"]] <- h2
  write_tbl(h2, "H2_q23_source_amount_summary")
} else {
  tee("  [SKIP] Q2.3 matrix columns not found")
}

# -- H3. Loan diversion and NPL ------------------------------------------------
tee("\nH3: Intended use vs diversion and NPL")
if (all(c("loan_used_as_intended", "is_npl") %in% names(borrowers))) {
  h3 <- borrowers %>%
    mutate(
      lui_code = suppressWarnings(as.integer(loan_used_as_intended)),
      use_group = case_when(
        lui_code == 1L ~ "Used as intended",
        lui_code == 3L ~ "Diverted to other purpose",
        TRUE           ~ NA_character_
      )
    ) %>%
    filter(!is.na(use_group)) %>%
    group_by(use_group) %>%
    summarise(
      n        = n(),
      n_npl    = sum(is_npl == 1L, na.rm = TRUE),
      n_non_npl = sum(is_npl == 0L, na.rm = TRUE),
      npl_rate = round(100 * mean(is_npl == 1L, na.rm = TRUE), 1),
      .groups  = "drop"
    ) %>%
    arrange(desc(npl_rate))

  tables[["H3_loan_use_diversion_vs_npl"]] <- h3
  write_tbl(h3, "H3_loan_use_diversion_vs_npl")
} else {
  tee("  [SKIP] Required columns for H3 not found")
}

# -- H4. Q3.3 third-party notification acceptability --------------------------
tee("\nH4: Third-party notification acceptability")
q33_cols <- names(df)[str_detect(names(df), "^\\d+-3\\.3")]

if (length(q33_cols) > 0) {
  h4_overall <- binary_acceptance_table(df, q33_cols, group_var = NULL, item_col = "contact_target") %>%
    mutate(segment = "Overall") %>%
    relocate(segment)
  tables[["H4A_q33_notification_overall"]] <- h4_overall
  write_tbl(h4_overall, "H4A_q33_notification_overall")

  h4_status <- binary_acceptance_table(
    borrowers,
    q33_cols,
    group_var = "repay_group",
    item_col = "contact_target"
  ) %>%
    rename(repayment_status = repay_group)
  if (nrow(h4_status) > 0) {
    tables[["H4B_q33_notification_by_repayment"]] <- h4_status
    write_tbl(h4_status, "H4B_q33_notification_by_repayment")
  } else {
    tee("  [SKIP] repay_group not available for H4B")
  }
} else {
  tee("  [SKIP] Q3.3 matrix columns not found")
}

# -- H5. Straw-borrowing prevalence -------------------------------------------
tee("\nH5: Straw borrowing and name-lending prevalence")
straw_cols <- intersect(c("is_straw_borrower", "is_borrowed_under_other_name"), names(borrowers))

if (length(straw_cols) > 0) {
  h5 <- tibble(
    indicator = c(
      "Borrowed under another person's name",
      "Own name used by another borrower",
      "Any name-mismatch exposure"
    ),
    n = c(
      if ("is_straw_borrower" %in% names(borrowers)) sum(borrowers$is_straw_borrower == 1L, na.rm = TRUE) else NA_integer_,
      if ("is_borrowed_under_other_name" %in% names(borrowers)) sum(borrowers$is_borrowed_under_other_name == 1L, na.rm = TRUE) else NA_integer_,
      if (all(c("is_straw_borrower", "is_borrowed_under_other_name") %in% names(borrowers))) {
        sum((borrowers$is_straw_borrower == 1L) | (borrowers$is_borrowed_under_other_name == 1L), na.rm = TRUE)
      } else {
        NA_integer_
      }
    )
  ) %>%
    mutate(pct_of_borrowers = round(100 * n / nrow(borrowers), 2))

  tables[["H5_straw_borrowing_prevalence"]] <- h5
  write_tbl(h5, "H5_straw_borrowing_prevalence")
} else {
  tee("  [SKIP] Straw-borrowing derived columns not found")
}

# -- H6. Reminder frequency and perceived effectiveness ------------------------
tee("\nH6: Reminder frequency x effectiveness")
eff_cols <- names(borrowers)[str_starts(names(borrowers), "effective_reminder_method__")]

if ("reminders_per_month" %in% names(borrowers) && length(eff_cols) > 0) {
  eff_signal_cols <- setdiff(eff_cols, "effective_reminder_method__no_effect")
  h6 <- borrowers %>%
    mutate(
      any_effective = if (length(eff_signal_cols) > 0) {
        as.integer(rowSums(across(all_of(eff_signal_cols)), na.rm = TRUE) > 0)
      } else {
        NA_integer_
      },
      no_effect = if ("effective_reminder_method__no_effect" %in% names(borrowers)) {
        as.integer(effective_reminder_method__no_effect == 1L)
      } else {
        NA_integer_
      }
    ) %>%
    filter(!is.na(reminders_per_month)) %>%
    group_by(reminders_per_month) %>%
    summarise(
      n                         = n(),
      pct_any_effective_method  = round(100 * mean(any_effective == 1L, na.rm = TRUE), 1),
      pct_no_effect             = round(100 * mean(no_effect == 1L, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(reminders_per_month)

  tables[["H6_reminder_frequency_effectiveness"]] <- h6
  write_tbl(h6, "H6_reminder_frequency_effectiveness")
} else {
  tee("  [SKIP] Required reminder fields not found")
}

# -- H7. Open-ended suggestions thematic summary (deep v2 coding) ------------
tee("\nH7: Open-ended suggestions thematic coding (deep)")
if ("repayment_culture_suggestions" %in% names(df)) {
  txt <- df %>%
    transmute(text = as.character(repayment_culture_suggestions)) %>%
    mutate(
      text_norm = text %>%
        str_to_lower() %>%
        str_replace_all("[‘’ʻʼ´`’]", "'") %>%
        str_replace_all("\\s+", " ") %>%
        str_trim()
    ) %>%
    filter(!is.na(text_norm), text_norm != "", !text_norm %in% c("na", "n/a"))

  non_info_pattern <- regex(
    "^(yo'?q|yuq|й[ўо]?қ|йук|йок|жок|taklif(lar|im)?(\\s+(yo'?q|yuq|mavjud emas))?|mavjud emas|hammasi yaxshi|яхши|bilmayman|bilmadim|билмайман|фикр(им)? й[ўу]қ|muammo mavjud emas|bor|бошка|бошқа|[-.0]+|taklif yuq|takliflar yuq)$",
    ignore_case = TRUE
  )

  theme_rules <- tibble::tribble(
    ~theme,                               ~pattern,
    "interest_cost_reduction",            "foiz|фоиз|ustama|ставка|penya|пеня|kamaytir|pasaytir|камайтир|пасайтир|арзон",
    "affordability_risk_controls",        "daromad|даромад|to'lov qobiliyat|тўлов қобилият|imkoniyat|имкон|hisob|bahola|reja|режа|risk|nazorat|назорат|tekshir|кафил|гаров|mahalla",
    "repayment_discipline",               "vaqti|вақтида|ўз вақтида|uz vaqti|qaytarish|қайтариш|to'lash kerak|тўлаш керак|tulash kerak|масъулият|интизом",
    "financial_literacy_awareness",       "savod|савод|ta'lim|таълим|o'quv|ўқув|trening|тренинг|tushuntir|тушунтир|targ'ib|тарғибот|ijtimoiy tarmoq|video",
    "flexibility_restructure",            "muddat|муддат|uzaytir|узайтир|kanikul|каникул|kredit tatili|кредит таътили|реструктур|grafik",
    "anti_fraud_consumer_protection",     "firibgar|фирибгар|ruxsat|рухсатсиз|online kredit|онлайн кредит|nomimga|номимга|aldan|банк ходим",
    "formal_channel_preference",          "rasmiy bank|расмий банк|qonuniy bank|қонуний банк|bankdan",
    "debt_avoidance",                     "olmaslik|олмаслик|kredit olmas|қарз олмас|zarurat bo'lmasa|kerak emas",
    "reminder_communication",             "sms|eslatma|эслатма|ogoh|огоҳ|xabar|telegram|whatsapp|ilova",
    "income_employment_support",          "ish bilan|иш билан|ish o'rin|иш ўрин|maosh|ойлик|oylik|daromadni osh",
    "legal_enforcement",                  "qonuniy chora|қонуний чора|sud|суд|mib|миб|javobgarlik"
  )

  h7_row <- txt %>%
    mutate(theme = if_else(str_detect(text_norm, non_info_pattern), "non_informative", "other_substantive"))

  for (i in seq_len(nrow(theme_rules))) {
    pat <- regex(theme_rules$pattern[i], ignore_case = TRUE)
    th  <- theme_rules$theme[i]
    h7_row <- h7_row %>%
      mutate(theme = if_else(theme == "other_substantive" & str_detect(text_norm, pat), th, theme))
  }

  h7 <- h7_row %>%
    count(theme, name = "n_responses") %>%
    mutate(pct_of_non_empty = round(100 * n_responses / nrow(h7_row), 1)) %>%
    arrange(desc(n_responses))

  h7_sub <- h7_row %>%
    filter(theme != "non_informative") %>%
    count(theme, name = "n_substantive") %>%
    mutate(pct_of_substantive = round(100 * n_substantive / sum(n_substantive), 1)) %>%
    arrange(desc(n_substantive))

  tables[["H7A_suggestions_theme_counts"]] <- h7
  write_tbl(h7, "H7A_suggestions_theme_counts")
  tables[["H7B_suggestions_theme_counts_substantive"]] <- h7_sub
  write_tbl(h7_sub, "H7B_suggestions_theme_counts_substantive")
} else {
  tee("  [SKIP] repayment_culture_suggestions not found")
}

# -- Optional workbook ---------------------------------------------------------
if (requireNamespace("openxlsx", quietly = TRUE) && length(tables) > 0) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(tables)) {
    sheet <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tables[[nm]])
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  tee(sprintf("\n  [OK] %s", basename(out_xlsx)))
}

tee(sprintf("\nTotal enrichment tables written: %d", length(tables)))
tee("=== 09_enrich_tables.R complete ===")

