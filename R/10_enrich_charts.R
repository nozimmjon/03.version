# =============================================================================
# 10_enrich_charts.R  --  Enrichment charts for final report expansion
# =============================================================================
# Input:   data/intermediate/04_analytical.rds
# Output:  outputs/figures/enrichment/EH*.png
#          logs/10_enrich_charts.log
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(forcats)
  library(scales)
  library(here)
})

# -- 0. Paths -----------------------------------------------------------------
in_rds   <- here("data", "intermediate", "04_analytical.rds")
out_dir  <- here("outputs", "figures", "enrichment")
log_file <- here("logs", "10_enrich_charts.log")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(in_rds))
writeLines("", log_file)

tee <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

save_fig <- function(stem, w = 10, h = 6, dpi = 180) {
  path <- file.path(out_dir, paste0(stem, ".png"))
  ggsave(path, width = w, height = h, dpi = dpi, bg = "white")
  tee(sprintf("  [OK] %s", basename(path)))
}

clean_label <- function(x) {
  out <- str_extract(x, "[^/]+$")
  out <- str_replace_all(out, "\\s+", " ")
  str_trim(out)
}

theme_pub <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey35"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = rel(0.9))
    )
}

tee(sprintf("=== 10_enrich_charts.R | %s ===", format(Sys.time(), "%Y-%m-%d %H:%M")))

# -- 1. Load ------------------------------------------------------------------
df <- readRDS(in_rds)
borrowers <- df %>% filter(has_loan == 1L)
tee(sprintf("Loaded 04_analytical.rds: %d rows x %d cols", nrow(df), ncol(df)))

n_plots <- 0L

# -- EH1. Q2.1 acceptability heatmap by repayment status ----------------------
tee("\nEH1: Q2.1 acceptability by repayment status")
q21_cols <- names(df)[str_detect(names(df), "^\\d+-2\\.1")]

if (length(q21_cols) > 0 && "repay_group" %in% names(borrowers)) {
  pdat <- borrowers %>%
    filter(!is.na(repay_group)) %>%
    select(repay_group, all_of(q21_cols)) %>%
    pivot_longer(cols = all_of(q21_cols), names_to = "purpose_col", values_to = "ok") %>%
    mutate(purpose = clean_label(purpose_col)) %>%
    group_by(repay_group, purpose) %>%
    summarise(
      pct_ok = 100 * mean(ok == 1L, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(pdat, aes(x = repay_group, y = fct_reorder(purpose, pct_ok, .fun = mean), fill = pct_ok)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = sprintf("%.0f", pct_ok)), size = 3, color = "black") +
    scale_fill_gradient(low = "#edf8fb", high = "#006d2c", labels = label_number(suffix = "%")) +
    labs(
      title = "Нима мақсадда қарз (кредит) олиш мақсадга мувофиқ?",
      subtitle = "тўлов ҳолати бўйича мақсадга мувофиқлигини билдирган респондентлар улуши",
      x = NULL, y = NULL, fill = "% Appropriate"
    ) +
    theme_pub()

  save_fig("EH1_q21_acceptability_heatmap", w = 11, h = 7)
  n_plots <- n_plots + 1L
} else {
  tee("  [SKIP] Q2.1 columns or repay_group not available")
}

# -- EH2. Q2.3 preferred source by loan-size willingness ----------------------
tee("\nEH2: Q2.3 source preference by amount")
q23_cols <- names(df)[str_detect(names(df), "^\\d+-2\\.3")]

if (length(q23_cols) > 0) {
  pdat <- df %>%
    select(all_of(q23_cols)) %>%
    pivot_longer(cols = everything(), names_to = "source_col", values_to = "amount_mln") %>%
    mutate(source = clean_label(source_col)) %>%
    group_by(source) %>%
    summarise(
      pct_willing = 100 * mean(amount_mln > 0, na.rm = TRUE),
      mean_if_willing = mean(amount_mln[amount_mln > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(source = fct_reorder(source, mean_if_willing))

  ggplot(pdat, aes(x = mean_if_willing, y = source, fill = pct_willing)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f mln | %.0f%% willing", mean_if_willing, pct_willing)),
              hjust = -0.05, size = 3.2) +
    scale_fill_gradient(low = "#deebf7", high = "#08519c") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
    labs(
      title = "EH2. Preferred loan source by amount capacity",
      subtitle = "Mean maximum amount among willing respondents (million UZS)",
      x = "Mean amount if willing (million UZS)", y = NULL, fill = "% willing"
    ) +
    theme_pub()

  save_fig("EH2_q23_source_amount_preference", w = 11, h = 6.2)
  n_plots <- n_plots + 1L
} else {
  tee("  [SKIP] Q2.3 columns not available")
}

# -- EH3. Intended-use diversion vs NPL ---------------------------------------
tee("\nEH3: Loan diversion vs NPL")
if (all(c("loan_used_as_intended", "is_npl") %in% names(borrowers))) {
  pdat <- borrowers %>%
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
      n = n(),
      npl_rate = 100 * mean(is_npl == 1L, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(pdat, aes(x = use_group, y = npl_rate, fill = use_group)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", npl_rate, n)), vjust = -0.3, size = 3.8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_percent(scale = 1)) +
    scale_fill_manual(values = c("Used as intended" = "#238b45", "Diverted to other purpose" = "#cb181d")) +
    labs(
      title = "EH3. NPL rate by intended loan use",
      subtitle = "Diversion from intended use is associated with higher NPL incidence",
      x = NULL, y = "NPL rate"
    ) +
    theme_pub()

  save_fig("EH3_loan_diversion_vs_npl", w = 8.5, h = 5.8)
  n_plots <- n_plots + 1L
} else {
  tee("  [SKIP] Required columns not available for EH3")
}

# -- EH4. Third-party notification acceptability ------------------------------
tee("\nEH4: Third-party notification acceptability")
q33_cols <- names(df)[str_detect(names(df), "^\\d+-3\\.3")]

if (length(q33_cols) > 0 && "repay_group" %in% names(borrowers)) {
  pdat <- borrowers %>%
    filter(!is.na(repay_group)) %>%
    select(repay_group, all_of(q33_cols)) %>%
    pivot_longer(cols = all_of(q33_cols), names_to = "target_col", values_to = "ok") %>%
    mutate(target = clean_label(target_col)) %>%
    group_by(repay_group, target) %>%
    summarise(pct_ok = 100 * mean(ok == 1L, na.rm = TRUE), .groups = "drop")

  ggplot(pdat, aes(x = pct_ok, y = fct_reorder(target, pct_ok, .fun = mean), fill = repay_group)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "EH4. Acceptability of notifying third parties on payment delay",
      subtitle = "By borrower repayment status",
      x = "% considering acceptable", y = NULL
    ) +
    theme_pub()

  save_fig("EH4_q33_notification_acceptability", w = 11, h = 6.8)
  n_plots <- n_plots + 1L
} else {
  tee("  [SKIP] Q3.3 columns or repay_group not available")
}

# -- EH5. Straw borrowing / name-lending by repayment status ------------------
tee("\nEH5: Straw borrowing exposure by repayment status")
if (all(c("is_straw_borrower", "is_borrowed_under_other_name", "repay_group") %in% names(borrowers))) {
  pdat <- borrowers %>%
    filter(!is.na(repay_group)) %>%
    group_by(repay_group) %>%
    summarise(
      straw_borrower = 100 * mean(is_straw_borrower == 1L, na.rm = TRUE),
      name_lent_out  = 100 * mean(is_borrowed_under_other_name == 1L, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(straw_borrower, name_lent_out),
                 names_to = "indicator", values_to = "pct") %>%
    mutate(indicator = recode(indicator,
                              straw_borrower = "Borrowed in another person's name",
                              name_lent_out  = "Own name used by another borrower"))

  ggplot(pdat, aes(x = repay_group, y = pct, fill = indicator)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              position = position_dodge(width = 0.75), vjust = -0.3, size = 3.2) +
    scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "EH5. Name-mismatch borrowing exposure",
      subtitle = "Two forms: straw borrowing and name-lending",
      x = NULL, y = "% of borrowers"
    ) +
    theme_pub()

  save_fig("EH5_straw_borrowing_by_repayment", w = 10.5, h = 6)
  n_plots <- n_plots + 1L
} else {
  tee("  [SKIP] Required straw-borrowing fields not available")
}

# -- EH6. Reminder frequency and effectiveness ---------------------------------
tee("\nEH6: Reminder frequency x effectiveness")
eff_cols <- names(borrowers)[str_starts(names(borrowers), "effective_reminder_method__")]

if ("reminders_per_month" %in% names(borrowers) && length(eff_cols) > 0) {
  eff_signal_cols <- setdiff(eff_cols, "effective_reminder_method__no_effect")
  pdat <- borrowers %>%
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
      pct_any_effective = 100 * mean(any_effective == 1L, na.rm = TRUE),
      pct_no_effect     = 100 * mean(no_effect == 1L, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(pct_any_effective, pct_no_effect), names_to = "metric", values_to = "pct") %>%
    mutate(metric = recode(metric,
                           pct_any_effective = "At least one effective method",
                           pct_no_effect = "No reminder effect"))

  ggplot(pdat, aes(x = reminders_per_month, y = pct, color = metric, group = metric)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.8) +
    scale_y_continuous(labels = label_number(suffix = "%"), limits = c(0, 100)) +
    scale_color_manual(values = c("At least one effective method" = "#08519c", "No reminder effect" = "#cb181d")) +
    labs(
      title = "EH6. Reminder intensity and perceived effectiveness",
      subtitle = "Effectiveness profile by reminder frequency bucket",
      x = "Reminder frequency per month", y = "% of borrowers"
    ) +
    theme_pub()

  save_fig("EH6_reminder_frequency_effectiveness", w = 10, h = 5.8)
  n_plots <- n_plots + 1L
} else {
  tee("  [SKIP] Reminder fields not available")
}

# -- EH7. Open-ended suggestions themes (deep v2 coding) ---------------------
tee("\nEH7: Open-ended suggestions themes (deep)")
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

  if (nrow(txt) > 0) {
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

    pdat <- txt %>%
      mutate(theme = if_else(str_detect(text_norm, non_info_pattern), "non_informative", "other_substantive"))

    for (i in seq_len(nrow(theme_rules))) {
      pat <- regex(theme_rules$pattern[i], ignore_case = TRUE)
      th  <- theme_rules$theme[i]
      pdat <- pdat %>%
        mutate(theme = if_else(theme == "other_substantive" & str_detect(text_norm, pat), th, theme))
    }

    pplot <- pdat %>%
      filter(theme != "non_informative") %>%
      count(theme, name = "n") %>%
      mutate(theme = fct_reorder(theme, n))

    if (nrow(pplot) > 0) {
      ggplot(pplot, aes(x = n, y = theme, fill = n)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = n), hjust = -0.08, size = 3.5) +
        scale_fill_gradient(low = "#deebf7", high = "#08519c") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
        labs(
          title = "EH7. Themes in open-ended repayment-culture suggestions",
          subtitle = "Substantive responses only (non-informative replies excluded)",
          x = "Responses", y = NULL
        ) +
        theme_pub()

      save_fig("EH7_open_suggestions_themes", w = 10.5, h = 6.2)
      n_plots <- n_plots + 1L
    } else {
      tee("  [SKIP] No substantive responses for EH7")
    }
  } else {
    tee("  [SKIP] No non-empty suggestions")
  }
} else {
  tee("  [SKIP] repayment_culture_suggestions not available")
}

tee(sprintf("\nTotal enrichment charts written: %d", n_plots))
tee("=== 10_enrich_charts.R complete ===")

