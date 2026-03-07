local_lib <- file.path(getwd(), "r_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}
# ============================================================
# 16_visualize_report_final_uz.R
#
# Full Uzbek chart pack for publication review.
# Based on the recheck script with:
# - full chart coverage,
# - figure numbering preserved in titles,
# - targeted redesign/polish from the visual audit,
# - AME-first econometric visuals from robust outputs.
#
# Input : data/intermediate/04_analytical.rds
# Output: outputs/figures/final_uz/*.png
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

HAS_BROOM <- requireNamespace("broom", quietly = TRUE)
HAS_PROC  <- requireNamespace("pROC", quietly = TRUE)
HAS_PATCH <- requireNamespace("patchwork", quietly = TRUE)

if (HAS_BROOM) library(broom)
if (HAS_PROC) library(pROC)
if (HAS_PATCH) library(patchwork)

df <- readRDS("data/intermediate/04_analytical.rds") %>%
  mutate(
    repay_group = recode(as.character(repay_group),
      "On-time" = "Ўз вақтида тўловчилар",
      "1-3 month delay" = "1-3 ой кечикувчилар",
      "NPL (3+ months)" = "NPL"
    ),
    repay_group = factor(repay_group, levels = c("Ўз вақтида тўловчилар", "1-3 ой кечикувчилар", "NPL"), ordered = TRUE)
  )
dir.create("outputs/figures/final_uz", recursive = TRUE, showWarnings = FALSE)

cat(sprintf("Маълумот юкланди: %d қатор × %d устун\n", nrow(df), ncol(df)))

# ── Colour palette & theme ───────────────────────────────────
COL_ONTIME   <- "#2ca02c"
COL_DELAY    <- "#ff7f0e"
COL_NPL      <- "#d62728"
COL_BORROW   <- "#1f77b4"
COL_NOBORROW <- "#aec7e8"

PALETTE3 <- c(
  "Ўз вақтида тўловчилар" = COL_ONTIME,
  "1-3 ой кечикувчилар" = COL_DELAY,
  "NPL" = COL_NPL
)

theme_cbuz <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.06), hjust = 0, lineheight = 1.05),
      plot.subtitle = element_text(colour = "grey40", size = rel(0.9)),
      plot.caption = element_text(colour = "grey40", hjust = 0),
      axis.title = element_text(size = rel(0.9)),
      axis.text = element_text(size = rel(0.9)),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 24, 10, 10)
    )
}

wrap_axis <- function(width = 20) {
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width))
}

expand_palette <- function(palette, n) {
  colorRampPalette(RColorBrewer::brewer.pal(
    min(n, RColorBrewer::brewer.pal.info[palette, "maxcolors"]), palette
  ))(n)
}

save_fig <- function(name, w = 10, h = 6) {
  path <- file.path("outputs/figures/final_uz", paste0(name, ".png"))
  ggsave(path, width = w, height = h, dpi = 150, bg = "white")
  cat(sprintf("  [OK] %s.png\n", name))
}

classify_primary_source <- function(x) {
  case_when(
    x %in% c(
      "Банк ташкилотларига",
      "Микромолия ташкилотларига",
      "Расмий насия хизматларига (“Техномарт”, “Узум”, “Ишонч” в.б.)",
      "Ломбардларга"
    ) ~ "Расмий",
    x %in% c("Оила аъзолари, дўстлар ёки танишларга") ~ "Яқинлар",
    x %in% c(
      "Кўчада фоиз эвазига пул (қарз) берувчи норасмий шахсларга",
      "Норасмий насия хизматига (бозорлар, дўконлар в.б.)"
    ) ~ "Норасмий",
    x %in% c("Қарз, кредит, насия олмасдан бошқа ечим қидирар эдим") ~ "Қарз олмаслик",
    TRUE ~ "Бошқа"
  )
}

source_palette <- c(
  "Расмий" = COL_BORROW,
  "Яқинлар" = COL_DELAY,
  "Норасмий" = COL_NPL,
  "Қарз олмаслик" = "#7A7A7A",
  "Бошқа" = "#CFD8DC"
)

borrowers <- df %>% filter(has_loan == 1)
non_borrowers <- df %>% filter(has_loan == 0)
npl_only <- borrowers %>% filter(is_npl == 1)

status_n_text <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  count(repay_group) %>%
  mutate(txt = sprintf("%s: n=%d", repay_group, n)) %>%
  pull(txt) %>%
  paste(collapse = " | ")

econ_dir <- file.path("outputs", "tables", "econometrics")
has_robust_econ <- all(file.exists(
  file.path(econ_dir, c("M4_marginal_effects.csv", "M5_model_fit_comparison.csv"))
))

if (has_robust_econ) {
  ame_tbl <- read.csv(file.path(econ_dir, "M4_marginal_effects.csv"), check.names = FALSE)
  fit_tbl <- read.csv(file.path(econ_dir, "M5_model_fit_comparison.csv"), check.names = FALSE)
}
# ============================================================
# BLOCK 1 – DEMOGRAPHICS (Charts 1–8)
# ============================================================
cat("\n--- Block 1: Demographics (charts 1-8) ---\n")

# ── Chart 1: Regional distribution ──────────────────────────
reg_share <- df %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    n = n(),
    borrower_share = 100 * mean(has_loan == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    low_n = n < 120,
    region = fct_reorder(str_wrap(region, 24), borrower_share)
  )

ggplot(reg_share, aes(x = borrower_share, y = region)) +
  geom_segment(aes(x = 0, xend = borrower_share, yend = region), colour = "grey80", linewidth = 0.8) +
  geom_point(aes(colour = low_n), size = 4.2, alpha = 0.95, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%% | n=%d", borrower_share, n), x = borrower_share + 2.1), hjust = 0, size = 3.2) +
  scale_colour_manual(values = c("TRUE" = COL_NPL, "FALSE" = COL_BORROW)) +
  scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "1-расм. Ҳудудлар бўйича қарздорлар улуши",
    subtitle = sprintf("Барча респондентлар, n=%d. Қизил нуқталар n<120 бўлган ҳудудлар", nrow(df)),
    x = "Қарздор респондентлар улуши",
    y = NULL,
    caption = "Стратификацияланган танланма таркиби кўрсатилган; миллий улуш сифатида талқин қилинмаслиги керак."
  ) +
  theme_cbuz()
save_fig("chart_01_regional_distribution", h = 6.5)

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
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Респондентларнинг ёш гуруҳлари бўйича тақсимот",
       x = "Ёш гуруҳи", y = "") +
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
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_fill(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Респондентларнинг жинс бўйича тақсимоти (%)",
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
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "1-расм. Респондентлар оилавий даромадининг тақсимоти (%)",
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
            hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "5-расм. Кредит тури бўйича тақсимот",
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
            vjust = -0.4, size = 6) +
  scale_fill_manual(values = PALETTE3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Қарзга эга респондентларнинг тўлов ҳолати бўйича тақсимоти",
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
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title = "2-расм. Респондентларнинг даромад манбалари (%)",
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
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c(
    "Қарздорлар"      = COL_BORROW,
    "Қарз олмаганлар" = COL_NOBORROW)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Респондентларнинг маълумот даражаси бўйича тақсимоти (%)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_08_education")

# ── Shared label maps for redesigned blocks ─────────────────
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

key_purps <- c(
  "loan_purpose__daily_expenses", "loan_purpose__appliances",
  "loan_purpose__wedding",        "loan_purpose__debt_repayment",
  "loan_purpose__business",       "loan_purpose__family_entrepreneurship"
)

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
  "Кафил билан боғланиш", "Расмий огоҳлантириш", "Суд жараёни", "Гаров чоралари"
)
names(coll_labels) <- coll_cols

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
  "Кафил қўнғироғи", "Юзма-юз учрашув", "Бошқа", "Самарасиз"
)
names(eff_labels) <- eff_cols

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

# ============================================================
# BLOCK 2 – CREDIT BEHAVIOUR (Charts 9–16)
# ============================================================
cat("\n--- Block 2: Credit Behaviour (charts 9-16) ---\n")

# ── Chart 9: Primary credit source preference ────────────────
source_top <- df %>%
  filter(!is.na(credit_source_primary), credit_source_primary != "Бошқа манбаларга (кўрсатинг)") %>%
  count(credit_source_primary, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n),
    source_type = classify_primary_source(credit_source_primary),
    label = case_when(
      credit_source_primary == "Расмий насия хизматларига (“Техномарт”, “Узум”, “Ишонч” в.б.)" ~ "Расмий насия хизматлари",
      credit_source_primary == "Қарз, кредит, насия олмасдан бошқа ечим қидирар эдим" ~ "Қарз олмасдан бошқа ечим қидирар эдим",
      TRUE ~ credit_source_primary
    ),
    label = str_wrap(label, 28)
  ) %>%
  arrange(desc(n)) %>%
  slice_head(n = 8) %>%
  mutate(label = fct_reorder(label, n))

ggplot(source_top, aes(x = n, y = label, fill = source_type)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d | %.1f%%", n, pct)), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = source_palette) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "3-расм. Зарур бўлганда биринчи мурожаат қилинадиган қарз манбалари",
    subtitle = sprintf("Барча респондентлар, n=%d. Ранглар манба турини англатади", nrow(df)),
    x = "Респондентлар сони",
    y = NULL
  ) +
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

inf_base <- df %>%
  filter(!is.na(informal_reason))

inf_sum <- inf_base %>%
  summarise(across(all_of(inf_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    pct   = 100 * n / nrow(inf_base),
    label = inf_labels[reason],
    label = fct_reorder(label, n)
  )

ggplot(inf_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.30))) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "5-расм. Норасмий манбага мурожаат қилиш сабаблари",
    subtitle = str_wrap("Норасмий манбалар: оила аъзолари (дўстлар, танишлар), норасмий насия ёки кўчадан қарз", 
                        width = 70)
    ,
    x = "Фоиз",
    y = NULL
  ) +
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
  "Таниш бор", "Катта сумма олиш осонроқ",
  "Қулайлик (онлайн)", "Узоқ муддатли", "Бошқа"
)
names(bank_labels) <- bank_cols

bank_base <- df %>%
  filter(!is.na(bank_reason))

bank_sum <- bank_base %>%
  summarise(across(all_of(bank_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "n") %>%
  mutate(
    pct   = 100 * n / nrow(bank_base),
    label = bank_labels[reason],
    label = fct_reorder(label, n)
  )

ggplot(bank_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.30))) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "4-расм. Тижорат банкига мурожаат қилиш сабаблари",
    subtitle = sprintf("Фақат 2.2.б саволи берилган респондентлар, n=%d", nrow(bank_base)),
    x = "Фоиз",
    y = NULL
  ) +
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
  geom_text(aes(label = sprintf("n=%d (%.1f%%)", n, pct)), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "12-расм. Кредит олмаслик сабаблари",
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
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(nrow(act_sum)))+
  labs(
    title    = "13-расм. Амалдаги кредит манбалари (%)",
    subtitle = sprintf("Қарздорлар — N = %d", nrow(borrowers)),
    x = "%", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_13_actual_credit_sources")

# ── Chart 14: Loan purposes (borrowers, %) ───────────────────
purp_top <- borrowers %>%
  summarise(across(all_of(purp_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "purpose", values_to = "n") %>%
  mutate(
    pct = 100 * n / nrow(borrowers),
    label = purp_labels[purpose]
  ) %>%
  arrange(desc(n)) %>%
  slice_head(n = 8) %>%
  mutate(label = fct_reorder(label, n))

ggplot(purp_top, aes(x = n, y = label)) +
  geom_col(fill = "#7A7A7A", width = 0.72) +
  geom_text(aes(label = sprintf("n=%d | %.1f%%", n, pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "8-расм. Олинган қарз мақсадлари: энг кўп учрайдиган йўналишлар",
    subtitle = sprintf("Қарздорлар, n=%d | Топ-8 мақсад", nrow(borrowers)),
    x = "Белгиланган ҳолатлар сони",
    y = NULL,
    caption = "Кўп жавобли савол: улушлар йиғиндиси 100% бўлмаслиги мумкин."
  ) +
  theme_cbuz()
save_fig("chart_14_loan_purposes", h = 6.6)

# ── Chart 15: Loan purpose × repayment status ────────────────
purp_status <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  group_by(repay_group) %>%
  summarise(across(all_of(key_purps), ~ 100 * mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-repay_group, names_to = "purpose", values_to = "pct") %>%
  mutate(label = purp_labels[purpose])

order_tbl <- purp_status %>%
  group_by(label) %>%
  summarise(max_pct = max(pct), .groups = "drop")

purp_status <- purp_status %>%
  left_join(order_tbl, by = "label") %>%
  mutate(label = fct_reorder(label, max_pct))

ggplot(purp_status, aes(x = pct, y = label, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "9-расм. Тўлов ҳолатига кўра асосий қарз мақсадлари",
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_15_purpose_by_status", w = 13.2, h = 6.6)

# ── Chart 16: Loan decision factors ──────────────────────────
dec_sum <- borrowers %>%
  summarise(across(all_of(dec_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "factor", values_to = "pct") %>%
  mutate(
    label = dec_labels[factor],
    label = fct_reorder(label, pct)
  ) %>%
  arrange(desc(pct))

ggplot(dec_sum, aes(x = pct, y = label)) +
  geom_col(fill = COL_BORROW, width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.7) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "11-расм. Қарз олишга таъсир этган омиллар",
    subtitle = sprintf("Қарздорлар, n=%d", nrow(borrowers)),
    x = "Улуш (%)",
    y = NULL
  ) +
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
  mutate(
    pct = 100 * n / sum(n),
    dsr_label = forcats::fct_rev(factor(as.character(dsr_cat), levels = levels(borrowers$dsr_cat)))
  ) %>%
  ungroup() %>% 
  filter(dsr_cat != "Жавоб бериш қийин (ўқилмасин)", 
         dsr_cat != "0% (тўлов қилмайман)")
  

ggplot(dsr_sum, aes(x = pct, y = dsr_label, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = str_wrap("19-расм. Тўлов ҳолатига кўра қарз тўловларининг оилавий даромадга нисбати", width = 45),
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_17_dti_by_status", w = 12.8, h = 6.4)

# ── Chart 18: Income volatility by repayment status ──────────
vol_sum <- borrowers %>%
  filter(
    !is.na(income_variability),
    !is.na(repay_group),
    as.character(income_variability) != "Жавоб бериш қийин (ўқилмасин)"
  ) %>%
  group_by(repay_group, income_variability) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(
    pct = 100 * n / sum(n),
    income_variability = forcats::fct_rev(factor(as.character(income_variability), levels = unique(as.character(na.omit(borrowers$income_variability)))))
  ) %>%
  ungroup()

ggplot(vol_sum, aes(x = pct, y = income_variability, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "17-расм. Тўлов ҳолатига кўра даромад барқарорлиги",
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_18_volatility_by_status", w = 12.8, h = 6.2)

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
            hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.32))) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26", guide = "none") +
  labs(
    title = str_wrap("16-расм. Кредитни муддати ўтган қарздорликка айланиш сабаблари",
                        width = 45), 
    subtitle = sprintf("NPL қарздорлар сони N= %d", N_npl),
    x = NULL, y = NULL
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
  mutate(
    pct = 100 * n / sum(n),
    contract_read = forcats::fct_rev(factor(as.character(contract_read), levels = unique(as.character(na.omit(borrowers$contract_read)))))
  ) %>%
  ungroup()

ggplot(contr_sum, aes(x = pct, y = contract_read, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = str_wrap("21-расм. Тўлов ҳолатига кўра шартнома билан танишиш даражаси", width = 45),
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_20_contract_by_status", w = 12.8, h = 6.4)


# ── Chart 21: Terms understood by repayment status ───────────
terms_sum <- borrowers %>%
  filter(!is.na(terms_understood), !is.na(repay_group)) %>%
  group_by(repay_group, terms_understood) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(
    pct = 100 * n / sum(n),
    terms_understood = forcats::fct_rev(factor(as.character(terms_understood), levels = unique(as.character(na.omit(borrowers$terms_understood)))))
  ) %>%
  ungroup()

ggplot(terms_sum, aes(x = pct, y = terms_understood, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = str_wrap("23-расм. Тўлов ҳолатига кўра кредит шартларини тушуниш даражаси", width = 45),
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))+

save_fig("chart_21_terms_by_status", w = 12.8, h = 6.4)

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
  labs(title = "10-расм. Қарздорлар кесимида кредит олганидан афсусланиш ҳолати",
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
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "22-расм. Шартнома билан танишиш ҳолати (Онлайн vs Офлайн)",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("chart_23_online_vs_offline")

# ============================================================
# BLOCK 5 – ROBUST ECONOMETRICS (Charts 24, 31, 32)
# ============================================================
cat("\n--- Block 5: Robust Econometrics (charts 24, 31, 32) ---\n")

if (has_robust_econ) {
  term_map <- c(
    age = "Ёш",
    dsr_midpoint = "Қарз юки (DSR)",
    employed_binary = "Расмий банд",
    fin_lit_score = "Молиявий саводхонлик",
    financial_buffer_months = "Молиявий буфер (ой)",
    has_higher_edu = "Олий маълумот",
    hh_size = "Уй хўжалиги катталиги",
    income_midpoint_mln_uzs = "Даромад (млн сўм)",
    income_variability_num = "Даромад беқарорлиги",
    is_male = "Эркак",
    is_married = "Турмуш қурган",
    productive_use_flag = "Мақсадли фойдаланиш"
  )

  ame_region <- ame_tbl %>%
    filter(model == "region_fe_logit") %>%
    mutate(
      var_label = recode(term, !!!term_map),
      direction = ifelse(marginal_effect >= 0, "Риск омили", "Ҳимоя омили"),
      var_label = fct_reorder(var_label, marginal_effect)
    )

  ggplot(ame_region, aes(x = marginal_effect, y = var_label, colour = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0.2, linewidth = 0.8, orientation = "y") +
    geom_point(size = 3.8) +
    geom_text(aes(label = sprintf("%.3f", marginal_effect), hjust = ifelse(marginal_effect >= 0, -0.2, 1.2)), size = 3.1, show.legend = FALSE) +
    scale_colour_manual(values = c("Риск омили" = COL_NPL, "Ҳимоя омили" = COL_ONTIME)) +
    scale_x_continuous(expand = expansion(mult = c(0.16, 0.18))) +
    labs(
      title = "24-расм. NPL эҳтимолига таъсир этувчи омиллар: ўртача чекли таъсир (AME)",
      subtitle = "Ҳудудий fixed effects logit модели, 95% ишонч оралиғи",
      x = "AME",
      y = NULL
    ) +
    theme_cbuz()
  save_fig("chart_24_logistic_coef", h = 7)

  key_ame <- ame_region %>%
    filter(term %in% c("dsr_midpoint", "financial_buffer_months", "income_variability_num", "fin_lit_score")) %>%
    mutate(
      step = case_when(
        term == "dsr_midpoint" ~ "+10 фоиз пункт",
        term == "financial_buffer_months" ~ "+1 ой",
        term == "income_variability_num" ~ "+1 поғона",
        term == "fin_lit_score" ~ "+1 балл",
        TRUE ~ "+1"
      ),
      display_effect = ifelse(term == "dsr_midpoint", marginal_effect * 10, marginal_effect),
      display_low = ifelse(term == "dsr_midpoint", conf_low * 10, conf_low),
      display_high = ifelse(term == "dsr_midpoint", conf_high * 10, conf_high),
      policy_label = case_when(
        term == "dsr_midpoint" ~ "Қарз юки (DSR)",
        term == "financial_buffer_months" ~ "Молиявий буфер",
        term == "income_variability_num" ~ "Даромад беқарорлиги",
        term == "fin_lit_score" ~ "Молиявий саводхонлик",
        TRUE ~ var_label
      ),
      policy_label = paste0(policy_label, " | ", step),
      direction = ifelse(display_effect >= 0, "Риск омили", "Ҳимоя омили"),
      policy_label = fct_reorder(policy_label, display_effect)
    )

  ggplot(key_ame, aes(x = display_effect, y = policy_label, fill = direction)) +
    geom_col(width = 0.68, show.legend = FALSE) +
    geom_errorbar(aes(xmin = display_low, xmax = display_high), width = 0.18, linewidth = 0.8, orientation = "y") +
    geom_text(aes(label = sprintf("%.1f п.п.", 100 * display_effect), hjust = ifelse(display_effect >= 0, -0.1, 1.1)), size = 3.4) +
    scale_fill_manual(values = c("Риск омили" = COL_NPL, "Ҳимоя омили" = COL_ONTIME)) +
    scale_x_continuous(labels = label_number(accuracy = 0.01), expand = expansion(mult = c(0.18, 0.2))) +
    labs(
      title = "31-расм. Асосий омилларнинг амалий таъсири",
      subtitle = "Қийматлар AME асосида: NPL эҳтимолидаги ўзгариш",
      x = "Эҳтимол ўзгариши",
      y = NULL,
      caption = "DSR учун таъсир +10 фоиз пункт ўзгаришга қайта ҳисобланган; қолганлари +1 бирлик қадамида."
    ) +
    theme_cbuz()
  save_fig("chart_31_odds_ratios", h = 6.6)

  fit_vis <- fit_tbl %>%
    mutate(
      model_label = recode(
        model,
        base_logit = "Базавий logit",
        region_fe_logit = "Ҳудуд FE logit",
        nonlinear_logit = "Ночизиқли logit",
        penalized_lasso = "LASSO",
        penalized_ridge = "Ridge"
      ),
      model_label = fct_reorder(model_label, auc)
    )

  ggplot(fit_vis, aes(x = auc, y = model_label)) +
    geom_segment(aes(x = 0.5, xend = auc, yend = model_label), colour = "grey80", linewidth = 0.8) +
    geom_point(size = 4, colour = COL_BORROW) +
    geom_text(aes(label = sprintf("AUC=%.3f | Accuracy=%.1f%%", auc, accuracy), x = auc + 0.006), hjust = 0, size = 3.2) +
    scale_x_continuous(limits = c(0.5, 0.78), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "32-расм. Робаст моделлар таққосланиши",
      subtitle = "AUC бўйича сараланган; ёрлиқларда accuracy ҳам берилган",
      x = "AUC",
      y = NULL
    ) +
    theme_cbuz()
  save_fig("chart_32_roc_curve", h = 5.8)
} else {
  message("  [SKIP charts 24, 31, 32] Robust econometrics tables not found in outputs/tables/econometrics")
}

# ============================================================
# BLOCK 6 – COLLECTION METHODS (Charts 25–26)
# ============================================================
cat("\n--- Block 6: Collection Methods (charts 25-26) ---\n")

# ── Chart 25: Collection methods × repayment status ──────────
coll_sum <- borrowers %>%
  filter(repay_group %in% c("Ўз вақтида тўловчилар", "NPL")) %>%
  group_by(repay_group) %>%
  summarise(across(all_of(coll_cols), ~ 100 * mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-repay_group, names_to = "method", values_to = "pct") %>%
  mutate(label = coll_labels[method])

coll_order <- coll_sum %>%
  group_by(label) %>%
  summarise(max_pct = max(pct), .groups = "drop")

coll_sum <- coll_sum %>%
  left_join(coll_order, by = "label") %>%
  mutate(label = fct_reorder(label, max_pct))

ggplot(coll_sum, aes(x = pct, y = label, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3[c("Ўз вақтида тўловчи", "NPL")]) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "28-расм. Тўлов ҳолатига кўра ундириш чоралари",
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_25_collection_by_status", w = 12.8, h = 6.4)

# ── Chart 26: Effective reminder methods ─────────────────────
eff_sum <- borrowers %>%
  summarise(across(all_of(eff_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "method", values_to = "pct") %>%
  mutate(
    label = eff_labels[method],
    label = fct_reorder(label, pct)
  )

ggplot(eff_sum, aes(x = pct, y = label)) +
  geom_col(fill = COL_BORROW, width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "27-расм. Қайси эслатма усуллари самарали деб баҳоланади",
    subtitle = sprintf("Қарздорлар, n=%d", nrow(borrowers)),
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz()
save_fig("chart_26_effective_reminders")

# ============================================================
# BLOCK 7 – FAMILY ENTREPRENEURSHIP (Charts 27–30)
# ============================================================
cat("\n--- Block 7: Family Entrepreneurship (charts 27-30) ---\n")

fam     <- borrowers %>% filter(!is.na(family_biz_admin) & family_biz_admin == 1L)
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
  "Истеъмолга йўналтирилди", "Шартлар нотўғри талқини", "Бошқа"
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

ggplot(fnpl_sum, aes(x = pct, y = label, fill = pct)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.32))) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26") +
  labs(
    title    = str_wrap("30-расм. Оилавий тадбиркорлик кредитида NPL сабаблари (Респондентлар фикрлари)",
                        width = 45),
    x = "Фоиз", y = NULL
  ) +
  theme_cbuz()
save_fig("chart_27_family_npl_reasons")

# ── Chart 28: Support measures needed ────────────────────────
supp_sum <- fam %>%
  summarise(across(all_of(supp_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "support", values_to = "pct") %>%
  mutate(
    label = supp_labels[support],
    label = fct_reorder(label, pct)
  )

ggplot(supp_sum, aes(x = pct, y = label)) +
  geom_col(fill = COL_BORROW, width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "31-расм. Оилавий кредит олувчиларга зарур кўмак чоралари",
    subtitle = sprintf("Оилавий тадбиркорлик кредити олганлар, n=%d", N_fam),
    x = "Улуш (%)",
    y = NULL
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
            hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  scale_fill_manual(values = c(
    "Юқори (>50%)" = COL_NPL,
    "Ўрта (30–50%)" = COL_DELAY,
    "Паст (<30%)"   = COL_ONTIME
  )) +
  labs(title = "29-расм. Кредит тури бўйича NPL даражаси (%)",
       x = "NPL даражаси (%)", y = NULL) +
  theme_cbuz()
save_fig("chart_29_npl_by_credit_type")

# ── Chart 30: Why took family credit ─────────────────────────
fbiz_sum <- fam %>%
  summarise(across(all_of(fbiz_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "pct") %>%
  mutate(
    label = fbiz_labels[reason],
    label = fct_reorder(label, pct)
  )

ggplot(fbiz_sum, aes(x = pct, y = label)) +
  geom_col(fill = "#7A7A7A", width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "29-расм. Нега оилавий кредит олинган",
    subtitle = sprintf("Оилавий тадбиркорлик кредити олганлар, n=%d", N_fam),
    x = "Улуш (%)",
    y = NULL
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
  mutate(
    pct = 100 * n / sum(n),
    fin_lit_score = forcats::fct_rev(factor(fin_lit_score, levels = sort(unique(fin_lit_score))))
  ) %>%
  ungroup()

ggplot(finlit_sum, aes(x = pct, y = fin_lit_score, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "26-расм. Тўлов ҳолатига кўра молиявий саводхонлик баллари",
    subtitle = status_n_text,
    caption = str_wrap("Изоҳ: Мазкур индекс 4 компонент йиғиндисидан тузилган (0-4).Булар: шартнома билан танишиш, шартларни тушуниш, кредит рейтинги ҳақида хабардорлик ва тўлов кечикиши кредит тарихига таъсир қилишини билиш.
                       4 энг юқори балл, 0 энг паст бал.",
                       width = 100),
    x = "Улуш (%)",
    y = "Молиявий саводхонлик бали"
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_33_finlit_by_status", w = 12.8, h = 6.2)

# ── Chart 34: Credit score motivates behaviour ────────────────
crsc_sum <- borrowers %>%
  filter(!is.na(credit_score_motivates)) %>%
  count(credit_score_motivates) %>%
  mutate(
    pct   = 100 * n / sum(n),
    label = fct_reorder(str_wrap(as.character(credit_score_motivates), 32), n)
  ) %>% 
  filter(credit_score_motivates != "Иккиланаман")

ggplot(crsc_sum, aes(x = pct, y = label, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(title = "25-расм. Кредит рейтинги ўз вақтида тўлашга рағбатлантирадими?",
       x = "Фоиз", y = NULL) +
  theme_cbuz()
save_fig("chart_34_credit_score_motivates")

# ── Chart 35: Knows delay effect × repayment status ──────────
knows_sum <- borrowers %>%
  filter(!is.na(knows_delay_effect), !is.na(repay_group)) %>%
  group_by(repay_group, knows_delay_effect) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(repay_group) %>%
  mutate(
    pct = 100 * n / sum(n),
    knows_delay_effect = forcats::fct_rev(factor(as.character(knows_delay_effect), levels = unique(as.character(na.omit(borrowers$knows_delay_effect)))))
  ) %>%
  ungroup() %>%
  filter(as.character(knows_delay_effect) != "Жавоб бериш қийин (ўқилмасин)")

ggplot(knows_sum, aes(x = pct, y = knows_delay_effect, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = PALETTE3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = str_wrap("24-расм. Тўлов ҳолатига кўра кечикишнинг кредит тарихига таъсири ҳақидаги билим", width = 45),
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_cbuz() +
  theme(strip.text = element_text(face = "bold"))
save_fig("chart_35_knows_delay_effect", w = 12.8, h = 6.4)

# ── Chart 36: Repayment priority ─────────────────────────────
prio_sum <- borrowers %>%
  summarise(across(all_of(prio_cols), ~ 100 * mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "creditor", values_to = "pct") %>%
  mutate(
    label = prio_labels[creditor],
    label = fct_reorder(label, pct)
  )

ggplot(prio_sum, aes(x = pct, y = label)) +
  geom_col(fill = COL_BORROW, width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "15-расм. Қарзлар бўйича тўлов устуворлиги",
    subtitle = sprintf("Қарздорлар, n=%d", nrow(borrowers)),
    x = "Улуш (%)",
    y = NULL
  ) +
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
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "14-расм. Респондентларнинг кредитларни кечиктириш ҳолатлари бўйича фикри",
       x = NULL, y = "%") +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  wrap_axis()

save_fig("chart_37_acceptable_delay")

# ── Chart 38: Peers repayment behaviour perception ────────────
peers_sum <- df %>%
  filter(!is.na(peers_repay_behavior)) %>%
  count(peers_repay_behavior, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n),
    label = str_wrap(as.character(peers_repay_behavior), 34),
    label = fct_reorder(label, n)
  )

ggplot(peers_sum, aes(x = n, y = label)) +
  geom_col(fill = "#7A7A7A", width = 0.72) +
  geom_text(aes(label = sprintf("n=%d | %.1f%%", n, pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "13-расм. Атрофдагиларнинг тўлов интизоми ҳақидаги қарашлар",
    subtitle = sprintf("Барча респондентлар, n=%d", nrow(df)),
    x = "Респондентлар сони",
    y = NULL
  ) +
  theme_cbuz()
save_fig("chart_38_peers_repay_perception")

# ============================================================
# ENRICHMENT CHARTS (39–42) — for team's Доклад_катта-32
# ============================================================
cat("\n--- Enrichment Charts (39-42) ---\n")

# ── Chart 39: Informal credit preference by region ───────────
# "Informal" = family/friends, street lenders, pawnshops,
#              informal installment (anything non-bank/MFI/formal)
informal_sources <- c(
  "Оила аъзолари, дўстлар ёки танишларга",
  "Кўчада фоиз эвазига пул (қарз) берувчи норасмий шахсларга",
  "Норасмий насия хизматига (бозорлар, дўконлар в.б.)",
  "Ломбардларга"
)

reg_inf <- df %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    n            = n(),
    pct_informal = 100 * mean(!is.na(informal_reason)),
    .groups = "drop"
  ) %>%
  mutate(
    region_lbl = str_wrap(region, 22),
    region_lbl = fct_reorder(region_lbl, pct_informal),
    tier = case_when(
      pct_informal >= 60 ~ "Юқори  (≥60%)",
      pct_informal >= 40 ~ "Ўрта  (40–60%)",
      TRUE               ~ "Паст  (<40%)"
    )
  )

ggplot(reg_inf, aes(x = pct_informal, y = region_lbl, fill = tier)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_informal)),
    hjust = -0.1, 
    size = 3.7
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.28)),
    limits = c(0, 100)
  ) +
  scale_fill_manual(
    values  = c("Юқори  (≥60%)" = COL_NPL,
                "Ўрта  (40–60%)" = COL_DELAY,
                "Паст  (<40%)"   = COL_ONTIME),
    breaks  = c("Юқори  (≥60%)", "Ўрта  (40–60%)", "Паст  (<40%)")
  ) +
  labs(
    title    = "7-расм. Ҳудудлар кесимида норасмий қарз олиш устворлиги",
    subtitle = "Норасмий манбаларга мурожаат қилишини билдирганлар улуши",
    x        = "(%)",
    y        = NULL,
    fill     = NULL
  ) +
  theme_cbuz()
save_fig("chart_39_informal_by_region", h = 6.5)

# ── Chart 40: Q2.3 — Max willingness to borrow by source ─────
q23_cols <- grep("^[0-9]+-2\\.3", names(df), value = TRUE)

q23_src_labels <- c(
  "Банк",
  "Расмий насия\n(техно-дўкон платформалари)",
  "Норасмий насия\n(бозор ва дўконлар)",
  "Кўчада фоиз\nберувчи шахс",
  "Микромолия\nташкилоти",
  "Ломбард",
  "Оила аъзолари /\nДўстлар",
  "Бошқа манба"
)
q23_band_map <- c(
  "2.5" = "5 млн сўмгача",
  "12.5" = "5-20 млн сўм",
  "35" = "20-50 млн сўм",
  "75" = "50-100 млн сўм",
  "150" = "100+ млн сўм"
)
q23_band_levels <- unname(q23_band_map[c("2.5", "12.5", "35", "75", "150")])

if (length(q23_cols) >= 8) {
  q23_stats <- df %>%
    summarise(across(all_of(q23_cols[1:8]), list(
      pct = ~ 100 * mean(.x > 0, na.rm = TRUE),
      median_code = ~ median(.x[.x > 0], na.rm = TRUE)
    ))) %>%
    pivot_longer(everything(), names_to = c("col", ".value"), names_pattern = "(.+)_(pct|median_code)") %>%
    mutate(
      source = q23_src_labels[seq_len(n())],
      median_band = factor(unname(q23_band_map[as.character(median_code)]), levels = q23_band_levels),
      median_order = median_code,
      source_type = case_when(
        str_detect(source, "Банк|Микромолия|Расмий насия") ~ "Расмий",
        str_detect(source, "Оила") ~ "Яқинлар",
        TRUE ~ "Норасмий"
      )
    ) %>%
    filter(source != "Бошқа манба") %>%
    arrange(desc(median_order), desc(pct)) %>%
    mutate(source = factor(source, levels = source))

  ggplot(q23_stats, aes(x = median_order, y = source, fill = source_type)) +
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%s | %.0f%%", as.character(median_band), pct)), hjust = -0.1, size = 3.6) +
    scale_fill_manual(values = c("Расмий" = COL_BORROW, "Яқинлар" = COL_DELAY, "Норасмий" = COL_NPL)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
    labs(
      title = "6-расм. Манбалар кесимида олинадиган медиан қарз миқдори",
      subtitle = "Қийматлар фақат 0 дан катта жавоблар бўйича медиан midpoint кодга асосланган",
      x = "Медиан миқдор (кодланган midpoint, млн сўм)",
      y = NULL
    ) +
    theme_cbuz()
  save_fig("chart_40_q23_loan_size_by_source", h = 6.1)
} else {
  message("  [SKIP chart 40] Expected 8 Q2.3 columns, found: ", length(q23_cols))
}

# ── Chart 41: Financial buffer distribution ───────────────────
# Ordered: no buffer → 1-2 months → 3-4 → 5-6 → 6+ months
buf_levels <- levels(df$financial_buffer)   # already ordered in the data

buf_sum <- df %>%
  filter(!is.na(financial_buffer)) %>%
  count(financial_buffer) %>%
  mutate(
    pct     = 100 * n / sum(n),
    is_zero = (as.integer(financial_buffer) == 1L),   # first level = no buffer
    lbl     = str_wrap(as.character(financial_buffer), 18)
  )

zero_pct <- buf_sum$pct[buf_sum$is_zero][1]

ggplot(buf_sum,
       aes(x = financial_buffer, y = pct, fill = is_zero)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%\n(n=%d)", pct, n)),
    vjust = -0.35, size = 4
  ) +
  scale_fill_manual(values = c("TRUE" = COL_NPL, "FALSE" = COL_BORROW)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  labs(
    title    = "18-расм. Респондентларнинг молиявий буфер (жамғарма захираси) бўйича ҳолат",
    subtitle = "Даромад тўхтаб қолса, неча ой давомида кредит (қарз) тўловларини амалга ошира оласиз?",
    x = NULL, y = "%"
  ) +
  theme_cbuz() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_fig("chart_41_financial_buffer")

# ── Chart 42: NPL rate — productive vs. non-productive use ───
prod_npl <- borrowers %>%
  group_by(productive_use_flag) %>%
  summarise(
    n        = n(),
    npl_rate = 100 * mean(is_npl, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    label = ifelse(productive_use_flag == 1,
                   "Мақсадли фойдаланиш",
                   "Мақсадсиз фойдаланиш"),
    fill_col = ifelse(npl_rate > 52, COL_NPL, COL_DELAY)
  )

ggplot(prod_npl, aes(x = label, y = npl_rate, fill = label)) +
  geom_col(show.legend = FALSE, width = 0.5) +
  geom_text(
    aes(label = sprintf("%.1f%%\n(n=%d)", npl_rate, n)),
    vjust = -0.4, size = 5
  ) +
  scale_fill_manual(values = setNames(prod_npl$fill_col, prod_npl$label)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2)),
    limits = c(0, 80)
  ) +
  labs(
    title    = "42-расм. NPL даражаси: кредит мақсадли ишлатилганми?",
    subtitle = "Диққат: мақсадли фойдаланиш NPL даражасини камайтирмайди — кредит йўналтирилиши мураккаброқ",
    x = NULL, y = "NPL даражаси (%)"
  ) +
  theme_cbuz()
save_fig("chart_42_npl_by_productive_use")

# ============================================================
# Done
# ============================================================
n_saved <- length(list.files("outputs/figures/final_uz", pattern = "\\.png$"))
cat(sprintf("\n=== 16_visualize_report_final_uz.R complete ===\n"))
cat(sprintf("Чизмалар outputs/figures/final_uz/ га сақланди  (%d PNG файл)\n", n_saved))
cat(sprintf("  Чизмалар 01–38 : асосий репорт блоклари\n"))
cat(sprintf("  Чизмалар 39–42 : қўшимча текширув чизмалари\n"))













