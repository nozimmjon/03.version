# ============================================================
# 07_visualise.R  –  Consolidated ggplot2 visualisation
#
# Replicates all 38 charts previously produced by the Python
# claudecodes/ pipeline, using the clean 04_analytical.rds.
#
# Input : data/intermediate/04_analytical.rds
# Output: outputs/figures/*.png
# ============================================================

# ── 0. Setup ─────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(forcats)
})

HAS_BROOM    <- requireNamespace("broom",     quietly = TRUE)
HAS_PROC     <- requireNamespace("pROC",      quietly = TRUE)
HAS_PATCH    <- requireNamespace("patchwork", quietly = TRUE)

if (HAS_BROOM)    library(broom)
if (HAS_PROC)     library(pROC)
if (HAS_PATCH)    library(patchwork)

df <- readRDS("data/intermediate/04_analytical.rds")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

cat(sprintf("Data loaded: %d rows × %d cols\n", nrow(df), ncol(df)))

# ── Colour palette & theme ───────────────────────────────────
COL_ONTIME   <- "#2ca02c"
COL_DELAY    <- "#ff7f0e"
COL_NPL      <- "#d62728"
COL_BORROW   <- "#1f77b4"
COL_NOBORROW <- "#aec7e8"

PALETTE3 <- c(
  "On-time"          = COL_ONTIME,
  "1-3 month delay"  = COL_DELAY,
  "NPL (3+ months)"  = COL_NPL
)

theme_cbuz <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle    = element_text(colour = "grey40", size = rel(0.9)),
      axis.title       = element_text(size = rel(0.9)),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}

save_fig <- function(name, w = 10, h = 6) {
  path <- file.path("outputs/figures", paste0(name, ".png"))
  ggsave(path, width = w, height = h, dpi = 150, bg = "white")
  cat(sprintf("  [OK] %s.png\n", name))
}

# ── Convenience subsets ──────────────────────────────────────
borrowers     <- df %>% filter(has_loan == 1)
non_borrowers <- df %>% filter(has_loan == 0)
npl_only      <- borrowers %>% filter(is_npl == 1)

# ============================================================
# BLOCK 1 – DEMOGRAPHICS (Charts 1–8)
# ============================================================
cat("\n--- Block 1: Demographics (charts 1-8) ---\n")

# ── Chart 1: Regional distribution ──────────────────────────
reg_sum <- df %>%
  group_by(region) %>%
  summarise(
    Қарздорлар     = sum(has_loan),
    `Қарз олмаганлар` = sum(1 - has_loan),
    total          = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Қарздорлар, `Қарз олмаганлар`),
               names_to = "status", values_to = "n") %>%
  mutate(region = fct_reorder(str_wrap(region, 25), total))

ggplot(reg_sum, aes(x = n, y = region, fill = status)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  labs(title = "1-чизма. Минтақалар бўйича тақсимот",
       x = "Респондентлар сони", y = NULL) +
  theme_cbuz()
save_fig("chart_01_regional_distribution")

# ── Chart 2: Age distribution by borrower status ─────────────
age_sum <- df %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group, has_loan) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(has_loan) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(status = ifelse(has_loan == 1, "Қарздорлар", "Қарз олмаганлар"))

ggplot(age_sum, aes(x = age_group, y = pct, fill = status)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  labs(title = "2-чизма. Ёш гуруҳлари бўйича тақсимот (%)",
       x = "Ёш гуруҳи", y = "%") +
  theme_cbuz()
save_fig("chart_02_age_distribution")

# ── Chart 3: Gender distribution ─────────────────────────────
gen_sum <- df %>%
  filter(!is.na(gender)) %>%
  group_by(gender, has_loan) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(has_loan) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(status = ifelse(has_loan == 1, "Қарздорлар", "Қарз олмаганлар"))

ggplot(gen_sum, aes(x = status, y = pct, fill = gender)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "3-чизма. Жинсий тақсимот (%)",
       x = NULL, y = "%") +
  theme_cbuz()
save_fig("chart_03_gender")

# ── Chart 4: Household income by borrower status ──────────────
inc_sum <- df %>%
  filter(!is.na(hh_income_cat)) %>%
  group_by(hh_income_cat, has_loan) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(has_loan) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(status = ifelse(has_loan == 1, "Қарздорлар", "Қарз олмаганлар"))

ggplot(inc_sum, aes(x = hh_income_cat, y = pct, fill = status)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  labs(title = "4-чизма. Уй хўжалиги даромади бўйича тақсимот (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_04_income")

# ── Chart 5: Credit type distribution ────────────────────────
ctype_sum <- borrowers %>%
  filter(!is.na(credit_type_label)) %>%
  count(credit_type_label) %>%
  mutate(
    pct   = 100 * n / sum(n),
    label = fct_reorder(credit_type_label, n)
  )

ggplot(ctype_sum, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d (%.1f%%)", n, pct)),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "5-чизма. Кредит тури бўйича тақсимот",
       x = "Қарздорлар сони", y = NULL) +
  theme_cbuz()
save_fig("chart_05_credit_type")

# ── Chart 6: Repayment status ─────────────────────────────────
rep_sum <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  count(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(rep_sum, aes(x = repay_group, y = n, fill = repay_group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d\n%.1f%%", n, pct)),
            vjust = -0.4, size = 4) +
  scale_fill_manual(values = PALETTE3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "6-чизма. Тўлов ҳолати бўйича тақсимот",
       x = NULL, y = "Сон") +
  theme_cbuz()
save_fig("chart_06_repayment_status")

# ── Chart 7: Income sources by borrower status ────────────────
src_cols <- c(
  "income_sources__salary",
  "income_sources__self_employment",
  "income_sources__remittances",
  "income_sources__pension",
  "income_sources__agriculture",
  "income_sources__seasonal",
  "income_sources__none",
  "income_sources__other"
)
src_labels <- c(
  "Иш ҳақи", "Тадбиркорлик", "Пул ўтказма", "Пенсия",
  "Деҳқончилик", "Мавсумий", "Доимий эмас", "Бошқа"
)
names(src_labels) <- src_cols

src_sum <- df %>%
  group_by(has_loan) %>%
  summarise(across(all_of(src_cols), ~ 100 * mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(-has_loan, names_to = "source", values_to = "pct") %>%
  mutate(
    status    = ifelse(has_loan == 1, "Қарздорлар", "Қарз олмаганлар"),
    src_label = src_labels[source]
  ) %>%
  group_by(src_label) %>%
  mutate(avg_pct = mean(pct)) %>%
  ungroup() %>%
  mutate(src_label = fct_reorder(src_label, avg_pct))

ggplot(src_sum, aes(x = pct, y = src_label, fill = status)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  labs(title = "7-чизма. Даромад манбалари (%)",
       x = "%", y = NULL) +
  theme_cbuz()
save_fig("chart_07_income_sources")

# ── Chart 8: Education level by borrower status ───────────────
edu_sum <- df %>%
  filter(!is.na(education)) %>%
  group_by(education, has_loan) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(has_loan) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(status = ifelse(has_loan == 1, "Қарздорлар", "Қарз олмаганлар"))

ggplot(edu_sum, aes(x = education, y = pct, fill = status)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  labs(title = "8-чизма. Маълумот даражаси бўйича тақсимот (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_08_education")

# ============================================================
# BLOCK 2 – CREDIT BEHAVIOUR (Charts 9–16)
# ============================================================
cat("\n--- Block 2: Credit Behaviour (charts 9-16) ---\n")

# ── Chart 9: Primary credit source preference ────────────────
src_pref <- df %>%
  filter(!is.na(credit_source_primary)) %>%
  count(credit_source_primary) %>%
  mutate(
    pct   = 100 * n / sum(n),
    label = fct_reorder(str_wrap(credit_source_primary, 32), n)
  )

ggplot(src_pref, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "9-чизма. Биринчи навбатда мурожаат қиладиган кредит манбаси",
       x = "Сон", y = NULL) +
  theme_cbuz()
save_fig("chart_09_primary_credit_source")

# ── Chart 10: Why informal borrowing ─────────────────────────
inf_cols   <- c(
  "informal_reason__easy_fast",
  "informal_reason__interest_free",
  "informal_reason__no_formal_income_needed",
  "informal_reason__religious",
  "informal_reason__bad_credit_history",
  "informal_reason__banks_too_expensive",
  "informal_reason__other"
)
inf_labels <- c(
  "Тез ва осон", "Фоизсиз", "Расмий даромад шарт эмас",
  "Диний сабаб", "Кредит тарихи ёмон", "Банклар қимматроқ", "Бошқа"
)
names(inf_labels) <- inf_cols

inf_n <- nrow(df %>% filter(!is.na(credit_source_primary) &
                               str_detect(credit_source_primary,
                                          regex("оила|дўст|нотаниш|кўча|норасмий", ignore_case = TRUE))))
inf_n <- max(inf_n, 1L)

inf_sum <- df %>%
  summarise(across(all_of(inf_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    pct   = 100 * n / nrow(df),
    label = inf_labels[reason],
    label = fct_reorder(label, n)
  )

ggplot(inf_sum, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d", n)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "10-чизма. Норасмий манбага мурожаат қилиш сабаблари",
       x = "Сон", y = NULL) +
  theme_cbuz()
save_fig("chart_10_informal_reasons")

# ── Chart 11: Why commercial banks ───────────────────────────
bank_cols <- c(
  "bank_reason__clear_legal",
  "bank_reason__builds_credit_history",
  "bank_reason__lower_interest",
  "bank_reason__has_contact_in_bank",
  "bank_reason__large_amount_easier",
  "bank_reason__convenient_online",
  "bank_reason__long_term_available",
  "bank_reason__other"
)
bank_labels <- c(
  "Юридик аниқлик", "Кредит тарихи яратиш", "Паст фоиз",
  "Банкда танишлиги бор", "Катта сумма осонроқ",
  "Онлайн қулайлик", "Узоқ муддат мавжуд", "Бошқа"
)
names(bank_labels) <- bank_cols

bank_sum <- df %>%
  summarise(across(all_of(bank_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    label = bank_labels[reason],
    label = fct_reorder(label, n)
  )

ggplot(bank_sum, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d", n)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "11-чизма. Тижорат банкига мурожаат қилиш сабаблари",
       x = "Сон", y = NULL) +
  theme_cbuz()
save_fig("chart_11_bank_reasons")

# ── Chart 12: Why prefer not to borrow ───────────────────────
nocred_cols <- c(
  "no_credit_reason__distrust_institutions",
  "no_credit_reason__fear_debt_trap",
  "no_credit_reason__unstable_income",
  "no_credit_reason__sufficient_savings",
  "no_credit_reason__hard_paperwork",
  "no_credit_reason__bad_experience_around",
  "no_credit_reason__dont_understand_terms",
  "no_credit_reason__religious",
  "no_credit_reason__family_opposition",
  "no_credit_reason__other"
)
nocred_labels <- c(
  "Ишонмаслик", "Қарз тузоғидан қўрқиш", "Барқарор эмас",
  "Етарли жамғарма бор", "Мураккаб ҳужжатлар", "Атрофимдагилар тажрибаси",
  "Шартларни тушунмаслик", "Диний сабаб", "Оила қаршилиги", "Бошқа"
)
names(nocred_labels) <- nocred_cols

nocred_sum <- non_borrowers %>%
  summarise(across(all_of(nocred_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    pct   = 100 * n / nrow(non_borrowers),
    label = nocred_labels[reason],
    label = fct_reorder(label, n)
  )

ggplot(nocred_sum, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d (%.1f%%)", n, pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "12-чизма. Кредит олмаслик сабаблари",
    subtitle = sprintf("Қарз олмаганлар — N = %d", nrow(non_borrowers)),
    x = "Сон", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_12_no_credit_reasons")

# ── Chart 13: Actual credit sources used (borrowers, %) ──────
act_cols <- c(
  "credit_source_actual__commercial_bank",
  "credit_source_actual__formal_installment",
  "credit_source_actual__mfi",
  "credit_source_actual__pawnshop",
  "credit_source_actual__street_lenders",
  "credit_source_actual__family_friends",
  "credit_source_actual__other"
)
act_labels <- c(
  "Тижорат банки", "Расмий насия", "МФТ", "Ломбард",
  "Кўчада фоиз", "Оила/Дўст", "Бошқа"
)
names(act_labels) <- act_cols

act_sum <- borrowers %>%
  summarise(across(all_of(act_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "source", values_to = "pct") %>%
  mutate(
    label = act_labels[source],
    label = fct_reorder(label, pct)
  )

ggplot(act_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "13-чизма. Фактик кредит манбалари (%)",
    subtitle = sprintf("Қарздорлар — N = %d", nrow(borrowers)),
    x = "%", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_13_actual_credit_sources")

# ── Chart 14: Loan purposes (borrowers, %) ───────────────────
purp_cols <- c(
  "loan_purpose__daily_expenses", "loan_purpose__healthcare",
  "loan_purpose__appliances",     "loan_purpose__auto",
  "loan_purpose__education",      "loan_purpose__wedding",
  "loan_purpose__mortgage",       "loan_purpose__renovation",
  "loan_purpose__debt_repayment", "loan_purpose__migration_work",
  "loan_purpose__business",       "loan_purpose__family_entrepreneurship",
  "loan_purpose__help_relatives", "loan_purpose__other"
)
purp_labels <- c(
  "Кунлик харажатлар", "Соғлиқ/Таълим", "Маиший техника", "Автомобил",
  "Таълим", "Тўй/Маросим", "Ипотека", "Таъмирлаш", "Бошқа қарзни тўлаш",
  "Миграция", "Бизнес", "Оилавий тадбиркорлик", "Қариндошларга ёрдам", "Бошқа"
)
names(purp_labels) <- purp_cols

purp_sum <- borrowers %>%
  summarise(across(all_of(purp_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "purpose", values_to = "pct") %>%
  mutate(
    label = purp_labels[purpose],
    label = fct_reorder(label, pct)
  )

ggplot(purp_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "14-чизма. Кредит мақсадлари (%)",
    subtitle = sprintf("Қарздорлар — N = %d", nrow(borrowers)),
    x = "%", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_14_loan_purposes", h = 7)

# ── Chart 15: Loan purpose × repayment status ────────────────
key_purps <- c(
  "loan_purpose__daily_expenses", "loan_purpose__appliances",
  "loan_purpose__wedding",        "loan_purpose__debt_repayment",
  "loan_purpose__business",       "loan_purpose__family_entrepreneurship"
)

purp_by_status <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  group_by(repay_group) %>%
  summarise(across(all_of(key_purps), ~ 100 * mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(-repay_group, names_to = "purpose", values_to = "pct") %>%
  mutate(label = purp_labels[purpose])

ggplot(purp_by_status, aes(x = label, y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(title = "15-чизма. Кредит мақсади × тўлов ҳолати (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_fig("chart_15_purpose_by_status", w = 11)

# ── Chart 16: Loan decision factors ──────────────────────────
dec_cols <- c(
  "loan_decision_factor__necessity",
  "loan_decision_factor__incentive_available",
  "loan_decision_factor__recommendation",
  "loan_decision_factor__government_recommendation",
  "loan_decision_factor__fraud",
  "loan_decision_factor__build_credit_history",
  "loan_decision_factor__inflation_fear",
  "loan_decision_factor__other"
)
dec_labels <- c(
  "Зарурият", "Имтиёз мавжудлиги", "Тавсия", "Давлат тавсияси",
  "Фирибгарлик", "Кредит тарихи яратиш", "Инфляциядан қўрқиш", "Бошқа"
)
names(dec_labels) <- dec_cols

dec_sum <- borrowers %>%
  summarise(across(all_of(dec_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "factor", values_to = "pct") %>%
  mutate(
    label = dec_labels[factor],
    label = fct_reorder(label, pct)
  )

ggplot(dec_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "16-чизма. Кредит олиш қарорига таъсир этган омиллар (%)",
       x = "%", y = NULL) +
  theme_cbuz()
save_fig("chart_16_decision_factors")

# ============================================================
# BLOCK 3 – FINANCIAL METRICS (Charts 17–19)
# ============================================================
cat("\n--- Block 3: Financial Metrics (charts 17-19) ---\n")

# ── Chart 17: DSR by repayment status ────────────────────────
dsr_sum <- borrowers %>%
  filter(!is.na(dsr_cat), !is.na(repay_group)) %>%
  group_by(repay_group, dsr_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(dsr_sum, aes(x = dsr_cat, y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(title = "17-чизма. Қарздорлик-даромад нисбати (DSR) × тўлов ҳолати (%)",
       x = "DSR категорияси", y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_17_dti_by_status")

# ── Chart 18: Income volatility by repayment status ──────────
vol_sum <- borrowers %>%
  filter(
    !is.na(income_variability),
    !is.na(repay_group),
    as.character(income_variability) != "Жавоб бериш қийин"
  ) %>%
  group_by(repay_group, income_variability) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(vol_sum, aes(x = income_variability, y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(title = "18-чизма. Даромад барқарорлиги × тўлов ҳолати (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig("chart_18_volatility_by_status")

# ── Chart 19: NPL reasons (top, horizontal bar) ──────────────
npl_reason_cols <- c(
  "repayment_difficulty_reason__income_decreased",
  "repayment_difficulty_reason__salary_delayed",
  "repayment_difficulty_reason__diverted_to_other_debt",
  "repayment_difficulty_reason__seasonal_income",
  "repayment_difficulty_reason__overestimated_repayment_ability",
  "repayment_difficulty_reason__social_norm_not_paying",
  "repayment_difficulty_reason__no_consequences_belief",
  "repayment_difficulty_reason__pay_when_able_belief",
  "repayment_difficulty_reason__unexpected_expenses",
  "repayment_difficulty_reason__bank_error",
  "repayment_difficulty_reason__hidden_contract_conditions",
  "repayment_difficulty_reason__fraud_registered_in_name",
  "repayment_difficulty_reason__unfair_pricing"
)
npl_reason_labels <- c(
  "Даромад камайди", "Иш ҳақи кечикди", "Бошқа қарзга йўналтирилди",
  "Мавсумий даромад", "Тўлов имкониятини ошиқ баҳолаш",
  "Тўламаслик нормаси", "Оқибатлар йўқ деган фикр",
  "Имкони бўлганда тўлайман", "Кутилмаган харажатлар",
  "Банк хатоси", "Яширин шартлар", "Номимга фирибгарлик",
  "Адолатсиз нархлаш"
)
names(npl_reason_labels) <- npl_reason_cols

N_npl <- nrow(npl_only)

npl_reason_sum <- npl_only %>%
  summarise(across(all_of(npl_reason_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    pct   = 100 * n / max(N_npl, 1),
    label = npl_reason_labels[reason],
    label = fct_reorder(label, n)
  ) %>%
  filter(n > 0)

ggplot(npl_reason_sum, aes(x = n, y = label, fill = pct)) +
  geom_col() +
  geom_text(aes(label = sprintf("n=%d (%.1f%%)", n, pct)),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.32))) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26", guide = "none") +
  labs(
    title    = "19-чизма. Муддати ўтган қарз (NPL) сабаблари",
    subtitle = sprintf("NPL мижозлар — N = %d", N_npl),
    x = "NPL мижозлар сони", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_19_npl_reasons", h = 7)

cat("\n[Charts 1-19 complete]\n")

# ============================================================
# BLOCK 4 – FINANCIAL LITERACY (Charts 20–23)
# ============================================================
cat("\n--- Block 4: Financial Literacy (charts 20-23) ---\n")

# ── Chart 20: Contract reading by repayment status ───────────
contr_sum <- borrowers %>%
  filter(!is.na(contract_read), !is.na(repay_group)) %>%
  group_by(repay_group, contract_read) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(contr_sum, aes(x = contract_read, y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(title = "20-чизма. Шартнома билан танишиш × тўлов ҳолати (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_20_contract_by_status")

# ── Chart 21: Terms understood by repayment status ───────────
terms_sum <- borrowers %>%
  filter(!is.na(terms_understood), !is.na(repay_group)) %>%
  group_by(repay_group, terms_understood) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(terms_sum, aes(x = terms_understood, y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(title = "21-чизма. Кредит шартлари тушунилдими × тўлов ҳолати (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_21_terms_by_status")

# ── Chart 22: Credit regret by repayment status ───────────────
regret_sum <- borrowers %>%
  filter(!is.na(loan_regret), !is.na(repay_group)) %>%
  group_by(repay_group) %>%
  summarise(
    pct_regret = 100 * mean(as.character(loan_regret) == "Ҳа", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(regret_sum, aes(x = repay_group, y = pct_regret, fill = repay_group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", pct_regret, n)),
            vjust = -0.4, size = 4) +
  scale_fill_manual(values = PALETTE3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), limits = c(0, 100)) +
  labs(title = "22-чизма. Кредит олганига афсусланиш × тўлов ҳолати (%)",
       x = NULL, y = "%") +
  theme_cbuz()
save_fig("chart_22_regret_by_status")

# ── Chart 23: Contract reading – Online vs Offline ────────────
online_contr <- borrowers %>%
  filter(!is.na(loan_channel), !is.na(contract_read)) %>%
  group_by(loan_channel, contract_read) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(loan_channel) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(online_contr, aes(x = contract_read, y = pct, fill = loan_channel)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "23-чизма. Шартнома билан танишиш: Онлайн vs Офлайн (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_23_online_vs_offline")

# ============================================================
# BLOCK 5 – LOGISTIC REGRESSION (Charts 24, 31, 32)
# ============================================================
cat("\n--- Block 5: Logistic Regression (charts 24, 31, 32) ---\n")

# ── Prepare model data ────────────────────────────────────────
vol_map <- c(
  "Барқарор"                    = 1,
  "Бироз ўзгариб туради"        = 2,
  "Жуда ўзгарувчан/мавсумий"   = 3
)

model_df <- borrowers %>%
  mutate(
    is_male               = as.integer(gender == "Эркак"),
    is_married            = as.integer(marital_status == "Турмуш қурган"),
    has_higher_edu        = as.integer(as.character(education) == "Олий таълим"),
    income_variability_num = vol_map[as.character(income_variability)]
  ) %>%
  select(
    is_npl, age_w, is_male, is_married, has_higher_edu,
    income_midpoint_mln_uzs_w, dsr_midpoint, financial_buffer_months,
    income_variability_num, fin_lit_score, employed_binary,
    hh_size_w, productive_use_flag
  ) %>%
  drop_na()

cat(sprintf("  Model sample: N = %d  (NPL rate = %.1f%%)\n",
            nrow(model_df), 100 * mean(model_df$is_npl)))

fit <- glm(is_npl ~ ., data = model_df, family = binomial)

acc_val    <- 100 * mean(round(fitted(fit)) == model_df$is_npl)
mcf_r2     <- as.numeric(1 - logLik(fit) /
                logLik(glm(is_npl ~ 1, data = model_df, family = binomial)))

var_rename <- c(
  age_w                     = "Ёш",
  is_male                   = "Эркак жинси",
  is_married                = "Уйланган/турмушга чиққан",
  has_higher_edu            = "Олий маълумот",
  income_midpoint_mln_uzs_w = "Даромад (млн сўм)",
  dsr_midpoint              = "ҚХН (DSR)",
  financial_buffer_months   = "Молиявий буфер (ой)",
  income_variability_num    = "Даромад барқарорлиги",
  fin_lit_score             = "Молиявий саводхонлик",
  employed_binary           = "Расмий банд",
  hh_size_w                 = "Уй хўжалиги катталиги",
  productive_use_flag       = "Самарали фойдаланиш"
)

if (HAS_BROOM) {
  coef_df <- broom::tidy(fit, conf.int = TRUE, exponentiate = FALSE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR        = exp(estimate),
      conf_low  = exp(conf.low),
      conf_high = exp(conf.high),
      direction = ifelse(estimate > 0, "Риск омили", "Ҳимоя омили"),
      var_label = recode(term, !!!var_rename), 
      var_label = reorder(var_label, estimate)
    )
    

  # Chart 24: Logistic coefficients (horizontal bar)
  ggplot(coef_df, aes(x = estimate, y = var_label, fill = direction)) +
    geom_col() +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
    geom_text(
      aes(label    = sprintf("OR=%.2f", OR),
          hjust    = ifelse(estimate > 0, -0.1, 1.1)),
      size = 3.5
    ) +
    scale_fill_manual(values = c("Риск омили" = COL_NPL,
                                 "Ҳимоя омили" = COL_ONTIME)) +
    labs(
      title    = sprintf("24-чизма. Логистик регрессия: коэффициентлар (N=%d)",
                         nrow(model_df)),
      subtitle = sprintf("Аниқлик: %.1f%%  |  McFadden R²: %.3f",
                         acc_val, mcf_r2),
      x = "Коэффициент (logit)", y = NULL
    ) +
    theme_cbuz()
  save_fig("chart_24_logistic_coef", h = 7)

  # Chart 31: Odds ratio forest plot
  ggplot(coef_df, aes(x = OR, y = var_label, colour = direction)) +
    geom_point(size = 4) +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.3) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "black") +
    scale_colour_manual(values = c("Риск омили" = COL_NPL,
                                   "Ҳимоя омили" = COL_ONTIME)) +
    labs(title = "31-чизма. NPL эҳтимолига таъсир: OR Forest Plot (95% CI)",
         x = "Odds Ratio", y = NULL) +
    theme_cbuz()
  save_fig("chart_31_odds_ratios", h = 7)

} else {
  message("  [SKIP charts 24 & 31] Install 'broom' package: install.packages('broom')")
}

# Chart 32: ROC curve
if (HAS_PROC) {
  roc_obj <- pROC::roc(model_df$is_npl, fitted(fit), quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  roc_df  <- data.frame(
    spec = roc_obj$specificities,
    sens = roc_obj$sensitivities
  )
  ggplot(roc_df, aes(x = 1 - spec, y = sens)) +
    geom_line(colour = COL_BORROW, linewidth = 1) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", colour = "grey60") +
    annotate("text", x = 0.72, y = 0.28,
             label = sprintf("AUC = %.3f", auc_val),
             size = 5, colour = COL_BORROW) +
    labs(title = "32-чизма. ROC эгри чизиғи – NPL логистик модели",
         x = "1 − Ўзига хослик (1 − Specificity)",
         y = "Сезгирлик (Sensitivity)") +
    theme_cbuz()
  save_fig("chart_32_roc_curve")
} else {
  message("  [SKIP chart 32] Install 'pROC' package: install.packages('pROC')")
}

# ============================================================
# BLOCK 6 – COLLECTION METHODS (Charts 25–26)
# ============================================================
cat("\n--- Block 6: Collection Methods (charts 25-26) ---\n")

# ── Chart 25: Collection methods × repayment status ──────────
coll_cols <- c(
  "collection_methods_received__sms",
  "collection_methods_received__calls",
  "collection_methods_received__bank_visit",
  "collection_methods_received__mib_officer_visit",
  "collection_methods_received__guarantor_contact",
  "collection_methods_received__official_warning_letter",
  "collection_methods_received__court_proceedings",
  "collection_methods_received__collateral_action"
)
coll_labels <- c(
  "SMS", "Қўнғироқ", "Банк ходими ташрифи", "МИБ ходими ташрифи",
  "Кафил билан боғланиш", "Расмий огоҳлантирма",
  "Суд жараёни", "Гаров чоралари"
)
names(coll_labels) <- coll_cols

coll_sum <- borrowers %>%
  filter(repay_group %in% c("On-time", "NPL (3+ months)")) %>%
  group_by(repay_group) %>%
  summarise(across(all_of(coll_cols), ~ 100 * mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(-repay_group, names_to = "method", values_to = "pct") %>%
  mutate(
    label = coll_labels[method],
    label = fct_reorder(label, pct, .fun = mean)
  )

ggplot(coll_sum, aes(x = pct, y = label, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3[c("On-time", "NPL (3+ months)")]) +
  labs(title = "25-чизма. Инкассо усуллари × тўлов ҳолати (%)",
       x = "%", y = NULL) +
  theme_cbuz()
save_fig("chart_25_collection_by_status")

# ── Chart 26: Effective reminder methods ─────────────────────
eff_cols <- c(
  "effective_reminder_method__sms",
  "effective_reminder_method__calls",
  "effective_reminder_method__app_notification",
  "effective_reminder_method__messenger_telegram_whatsapp",
  "effective_reminder_method__guarantor_call",
  "effective_reminder_method__in_person",
  "effective_reminder_method__other",
  "effective_reminder_method__no_effect"
)
eff_labels <- c(
  "SMS", "Қўнғироқ", "Банк иловаси", "Telegram/WhatsApp",
  "Кафил қўнғироғи", "Шахсий учрашув", "Бошқа", "Самарасиз"
)
names(eff_labels) <- eff_cols

eff_sum <- borrowers %>%
  summarise(across(all_of(eff_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "method", values_to = "pct") %>%
  mutate(
    label = eff_labels[method],
    label = fct_reorder(label, pct)
  )

ggplot(eff_sum, aes(x = pct, y = label, fill = pct)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(title = "26-чизма. Самарали эслатма усуллари (%)",
       x = "%", y = NULL) +
  theme_cbuz()
save_fig("chart_26_effective_reminders")

# ============================================================
# BLOCK 7 – FAMILY ENTREPRENEURSHIP (Charts 27–30)
# ============================================================
cat("\n--- Block 7: Family Entrepreneurship (charts 27-30) ---\n")

fam     <- borrowers %>% filter(kredit_turi == "oilaviy")
N_fam   <- nrow(fam)
cat(sprintf("  Family biz borrowers: N = %d\n", N_fam))

# ── Chart 27: Family credit NPL reasons ──────────────────────
fnpl_cols <- c(
  "family_biz_npl_reason__income_below_expectations",
  "family_biz_npl_reason__business_failed",
  "family_biz_npl_reason__believed_state_loan_forgiven",
  "family_biz_npl_reason__diverted_to_consumption",
  "family_biz_npl_reason__misunderstood_terms",
  "family_biz_npl_reason__other"
)
fnpl_labels <- c(
  "Даромад кутилгандан паст", "Бизнес юрмаган",
  "Давлат кредити кечирилади деган фикр",
  "Истеъмолга йўналтирилди", "Шартлар нотўғри талқин", "Бошқа"
)
names(fnpl_labels) <- fnpl_cols

fnpl_sum <- fam %>%
  summarise(across(all_of(fnpl_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    pct   = 100 * n / max(N_fam, 1),
    label = fnpl_labels[reason],
    label = fct_reorder(label, n)
  )

ggplot(fnpl_sum, aes(x = n, y = label, fill = pct)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d (%.1f%%)", n, pct)),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.32))) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26") +
  labs(
    title    = "27-чизма. Оилавий тадбиркорлик кредити – NPL сабаблари",
    subtitle = sprintf("Оилавий кредит олганлар — N = %d", N_fam),
    x = "Сон", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_27_family_npl_reasons")

# ── Chart 28: Support measures needed ────────────────────────
supp_cols <- c(
  "family_biz_support__business_training",
  "family_biz_support__flexible_schedule",
  "family_biz_support__staged_disbursement",
  "family_biz_support__business_plan_help",
  "family_biz_support__strengthen_oversight",
  "family_biz_support__cash_and_card",
  "family_biz_support__other"
)
supp_labels <- c(
  "Бизнес тренинглар", "Мослашувчан жадвал", "Босқичма-босқич бериш",
  "Бизнес режа ёрдами", "Назоратни кучайтириш", "Нақд ва пластик", "Бошқа"
)
names(supp_labels) <- supp_cols

supp_sum <- fam %>%
  summarise(across(all_of(supp_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "support", values_to = "pct") %>%
  mutate(
    label = supp_labels[support],
    label = fct_reorder(label, pct)
  )

ggplot(supp_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "28-чизма. Оилавий кредит олувчиларга зарур кўмак чоралари (%)",
    subtitle = sprintf("N = %d", N_fam),
    x = "%", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_28_family_support")

# ── Chart 29: NPL rate by credit type ────────────────────────
npl_by_type <- borrowers %>%
  filter(!is.na(credit_type_label)) %>%
  group_by(credit_type_label) %>%
  summarise(n = n(), npl_rate = 100 * mean(is_npl, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n >= 5) %>%
  mutate(
    colour_cat = case_when(
      npl_rate > 50 ~ "Юқори (>50%)",
      npl_rate > 30 ~ "Ўрта (30–50%)",
      TRUE          ~ "Паст (<30%)"
    ),
    credit_type_label = fct_reorder(credit_type_label, npl_rate)
  )

ggplot(npl_by_type,
       aes(x = npl_rate, y = credit_type_label, fill = colour_cat)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%% (n=%d)", npl_rate, n)),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  scale_fill_manual(values = c(
    "Юқори (>50%)" = COL_NPL,
    "Ўрта (30–50%)" = COL_DELAY,
    "Паст (<30%)"   = COL_ONTIME
  )) +
  labs(title = "29-чизма. Кредит тури бўйича NPL даражаси (%)",
       x = "NPL даражаси (%)", y = NULL) +
  theme_cbuz()
save_fig("chart_29_npl_by_credit_type")

# ── Chart 30: Why took family credit ─────────────────────────
fbiz_cols <- c(
  "family_biz_reason__business_idea",
  "family_biz_reason__lower_interest",
  "family_biz_reason__inspired_by_others",
  "family_biz_reason__social_pressure",
  "family_biz_reason__trusted_advice",
  "family_biz_reason__other"
)
fbiz_labels <- c(
  "Бизнес ғояни амалга ошириш", "Паст фоизлар",
  "Атрофимдагилар тажрибаси", "Босим остида",
  "Маслаҳатга ишониш", "Бошқа"
)
names(fbiz_labels) <- fbiz_cols

fbiz_sum <- fam %>%
  summarise(across(all_of(fbiz_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "pct") %>%
  mutate(
    label = fbiz_labels[reason],
    label = fct_reorder(label, pct)
  )

ggplot(fbiz_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "30-чизма. Оилавий кредит олиш сабаблари (%)",
    subtitle = sprintf("N = %d", N_fam),
    x = "%", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_30_why_family_credit")

# ============================================================
# ADDITIONAL CHARTS (33–38)
# ============================================================
cat("\n--- Additional Charts (33-38) ---\n")

# ── Chart 33: Financial literacy score × repayment status ────
finlit_sum <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  group_by(repay_group, fin_lit_score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(finlit_sum,
       aes(x = factor(fin_lit_score), y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(title = "33-чизма. Молиявий саводхонлик балли × тўлов ҳолати (%)",
       x = "МС балли (0–4)", y = "%") +
  theme_cbuz()
save_fig("chart_33_finlit_by_status")

# ── Chart 34: Credit score motivates behaviour ────────────────
crsc_sum <- borrowers %>%
  filter(!is.na(credit_score_motivates)) %>%
  count(credit_score_motivates) %>%
  mutate(
    pct   = 100 * n / sum(n),
    label = fct_reorder(str_wrap(as.character(credit_score_motivates), 32), n)
  )

ggplot(crsc_sum, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(title = "34-чизма. Кредит рейтинги ўз вақтида тўлашга рағбатлантирадими?",
       x = "Сон", y = NULL) +
  theme_cbuz()
save_fig("chart_34_credit_score_motivates")

# ── Chart 35: Knows delay effect × repayment status ──────────
knows_sum <- borrowers %>%
  filter(!is.na(knows_delay_effect), !is.na(repay_group)) %>%
  group_by(repay_group, knows_delay_effect) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(knows_sum,
       aes(x = str_wrap(as.character(knows_delay_effect), 20),
           y = pct, fill = repay_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = PALETTE3) +
  labs(
    title = "35-чизма. Кечикишнинг кредит тарихига таъсирини билиш × ҳолат (%)",
    x = NULL, y = "%"
  ) +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_35_knows_delay_effect")

# ── Chart 36: Repayment priority ─────────────────────────────
prio_cols <- c(
  "repayment_priority__banks",
  "repayment_priority__family_friends",
  "repayment_priority__street_lenders",
  "repayment_priority__formal_installment",
  "repayment_priority__informal_installment",
  "repayment_priority__mfi"
)
prio_labels <- c(
  "Банклар", "Оила/Дўстлар", "Кўчада фоиз",
  "Расмий насия", "Норасмий насия", "МФТ"
)
names(prio_labels) <- prio_cols

prio_sum <- borrowers %>%
  summarise(across(all_of(prio_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "creditor", values_to = "pct") %>%
  mutate(
    label = prio_labels[creditor],
    label = fct_reorder(label, pct)
  )

ggplot(prio_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "36-чизма. Тўлов устуворлиги: кредиторлар бўйича (%)",
       x = "%", y = NULL) +
  theme_cbuz()
save_fig("chart_36_repayment_priority")

# ── Chart 37: Acceptable delay period ────────────────────────
delay_sum <- borrowers %>%
  filter(!is.na(acceptable_delay_days)) %>%
  count(acceptable_delay_days) %>%
  mutate(pct = 100 * n / sum(n))

ggplot(delay_sum,
       aes(x = acceptable_delay_days, y = pct, fill = acceptable_delay_days)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "37-чизма. Мўлжалланган кечикиш муддати (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_37_acceptable_delay")

# ── Chart 38: Peers repayment behaviour perception ────────────
peers_sum <- df %>%
  filter(!is.na(peers_repay_behavior)) %>%
  count(peers_repay_behavior) %>%
  mutate(
    pct   = 100 * n / sum(n),
    label = fct_reorder(str_wrap(as.character(peers_repay_behavior), 38), n)
  )

ggplot(peers_sum, aes(x = n, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "38-чизма. Атрофимдагиларнинг тўлов хулқ-атвори ҳақида фикр",
       x = "Сон", y = NULL) +
  theme_cbuz()
save_fig("chart_38_peers_repay_perception")

# ============================================================
# Done
# ============================================================
n_saved <- length(list.files("outputs/figures", pattern = "\\.png$"))
cat(sprintf("\n=== 07_visualise.R complete ===\n"))
cat(sprintf("Charts saved to outputs/figures/  (%d PNG files)\n", n_saved))
