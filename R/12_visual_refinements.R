local_lib <- file.path(getwd(), "r_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}
# =============================================================================
# 12_visual_refinements.R
# Focused chart pack applying stricter data visualization principles
# =============================================================================
# Inputs:
#   data/intermediate/04_analytical.rds
#   outputs/tables/econometrics/M4_marginal_effects.csv
#   outputs/tables/enrichment/H7B_suggestions_theme_counts_substantive.csv
# Outputs:
#   outputs/figures/refined/*.png
#   logs/12_visual_refinements.log
# =============================================================================

has_ragg <- requireNamespace('ragg', quietly = TRUE)

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

in_rds <- here("data", "intermediate", "04_analytical.rds")
ame_csv <- here("outputs", "tables", "econometrics", "M4_marginal_effects.csv")
themes_csv <- here("outputs", "tables", "enrichment", "H7B_suggestions_theme_counts_substantive.csv")
out_dir <- here("outputs", "figures", "refined")
log_file <- here("logs", "12_visual_refinements.log")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(here("logs"), recursive = TRUE, showWarnings = FALSE)
writeLines("", log_file)

tee <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

save_fig <- function(stem, w = 10, h = 6, dpi = 180) {
  path <- file.path(out_dir, paste0(stem, ".png"))
  if (has_ragg) {
    ggsave(path, width = w, height = h, dpi = dpi, bg = "white", device = ragg::agg_png)
  } else {
    ggsave(path, width = w, height = h, dpi = dpi, bg = "white")
  }
  tee(sprintf("  [OK] %s", basename(path)))
}

theme_refined <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey35"),
      axis.title = element_text(size = rel(0.92)),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.caption = element_text(color = "grey35", hjust = 0)
    )
}

col_risk <- "#B23A48"
col_protect <- "#2E6F57"
col_neutral <- "#8A817C"
col_structure <- "#3C6E71"
col_low_n <- "#C44536"

stopifnot(file.exists(in_rds))
stopifnot(file.exists(ame_csv))
stopifnot(file.exists(themes_csv))

df <- readRDS(in_rds)
ame <- read_csv(ame_csv, show_col_types = FALSE)
themes <- read_csv(themes_csv, show_col_types = FALSE)

tee(sprintf("=== 12_visual_refinements.R | %s ===", format(Sys.time(), "%Y-%m-%d %H:%M")))
tee(sprintf("Loaded analytical data: %d rows x %d cols", nrow(df), ncol(df)))

# -----------------------------------------------------------------------------
# V1. Policy dashboard figure: four policy-relevant effects
# -----------------------------------------------------------------------------
tee("\nV1: policy dashboard effect bars")

dashboard_tbl <- ame %>%
  filter(model == "region_fe_logit") %>%
  filter(term %in% c("dsr_midpoint", "financial_buffer_months", "income_variability_num", "fin_lit_score")) %>%
  mutate(
    increment = case_when(
      term == "dsr_midpoint" ~ 0.10,
      TRUE ~ 1
    ),
    term_label = recode(
      term,
      dsr_midpoint = "DSR (+10 p.p.)",
      financial_buffer_months = "Financial buffer (+1 month)",
      income_variability_num = "Income variability (+1 level)",
      fin_lit_score = "Financial literacy (+1 point)"
    ),
    effect_pp = 100 * marginal_effect * increment,
    conf_low_pp = 100 * conf_low * increment,
    conf_high_pp = 100 * conf_high * increment,
    direction = if_else(effect_pp >= 0, "Risk", "Protective"),
    label = sprintf("%+.2f pp", effect_pp)
  ) %>%
  arrange(effect_pp) %>%
  mutate(term_label = factor(term_label, levels = term_label))

p1 <- ggplot(dashboard_tbl, aes(x = effect_pp, y = term_label, fill = direction)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_errorbar(aes(xmin = conf_low_pp, xmax = conf_high_pp), orientation = "y", width = 0.14, linewidth = 0.7) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.5) +
  geom_text(
    aes(label = label, x = if_else(effect_pp >= 0, effect_pp + 0.45, effect_pp - 0.45)),
    hjust = ifelse(dashboard_tbl$effect_pp >= 0, 0, 1),
    size = 3.6
  ) +
  scale_fill_manual(values = c("Risk" = col_risk, "Protective" = col_protect)) +
  scale_x_continuous(labels = label_number(suffix = " pp"), expand = expansion(mult = c(0.08, 0.18))) +
  labs(
    title = "Policy dashboard: practical change in NPL probability",
    subtitle = "Region fixed-effects model; bars show average marginal effect for realistic increments",
    x = "Approximate change in NPL probability (percentage points)",
    y = NULL,
    caption = "Positive values increase NPL risk; negative values reduce it."
  ) +
  theme_refined()

save_fig("RV1_policy_dashboard_effects", w = 11, h = 5.8)

# -----------------------------------------------------------------------------
# V2. Clean AME plot with uncertainty
# -----------------------------------------------------------------------------
tee("\nV2: AME plot with uncertainty")

ame_plot_tbl <- ame %>%
  filter(model == "region_fe_logit") %>%
  filter(term %in% c(
    "age", "dsr_midpoint", "financial_buffer_months", "income_variability_num",
    "fin_lit_score", "income_midpoint_mln_uzs", "has_higher_edu"
  )) %>%
  mutate(
    term_label = recode(
      term,
      age = "Age",
      dsr_midpoint = "DSR midpoint",
      financial_buffer_months = "Financial buffer (months)",
      income_variability_num = "Income variability (1-3)",
      fin_lit_score = "Financial literacy score",
      income_midpoint_mln_uzs = "Income midpoint (mln UZS)",
      has_higher_edu = "Higher education"
    ),
    direction = if_else(marginal_effect >= 0, "Risk", "Protective")
  ) %>%
  arrange(marginal_effect) %>%
  mutate(term_label = factor(term_label, levels = term_label))

p2 <- ggplot(ame_plot_tbl, aes(x = marginal_effect, y = term_label, color = direction)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), orientation = "y", width = 0.14, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(
    aes(
      label = sprintf("%+.3f", marginal_effect),
      x = if_else(marginal_effect >= 0, conf_high + 0.008, conf_low - 0.008)
    ),
    hjust = ifelse(ame_plot_tbl$marginal_effect >= 0, 0, 1),
    size = 3.3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Risk" = col_risk, "Protective" = col_protect)) +
  scale_x_continuous(labels = label_number(accuracy = 0.01), expand = expansion(mult = c(0.1, 0.18))) +
  labs(
    title = "Average marginal effects with uncertainty",
    subtitle = "Region fixed-effects model; points are AMEs and whiskers are 95% confidence intervals",
    x = "Average marginal effect on NPL probability",
    y = NULL
  ) +
  theme_refined()

save_fig("RV2_ame_uncertainty_region_fe", w = 10.8, h = 5.8)

# -----------------------------------------------------------------------------
# V3. Denominator-aware regional chart with low-n flagging
# -----------------------------------------------------------------------------
tee("\nV3: denominator-aware regional chart")

informal_sources <- c(
  "Оила аъзолари, дўстлар ёки танишларга",
  "Кўчада фоиз эвазига пул (қарз) берувчи норасмий шахсларга",
  "Норасмий насия хизматига (бозорлар, дўконлар в.б.)",
  "Ломбардларга"
)

region_tbl <- df %>%
  filter(!is.na(credit_source_primary), !is.na(region)) %>%
  mutate(is_informal_first = credit_source_primary %in% informal_sources) %>%
  group_by(region) %>%
  summarise(
    n = n(),
    pct_informal = 100 * mean(is_informal_first, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    low_n = n < 120,
    n_flag = if_else(low_n, "Low n", "Adequate n"),
    region_label = str_wrap(region, 24),
    region_label = fct_reorder(region_label, pct_informal)
  )

p3 <- ggplot(region_tbl, aes(x = pct_informal, y = region_label)) +
  geom_segment(aes(x = 0, xend = pct_informal, yend = region_label), color = "grey80", linewidth = 0.8) +
  geom_point(aes(size = n, color = n_flag), alpha = 0.95) +
  geom_text(aes(label = sprintf("%.1f%%  |  n=%d", pct_informal, n), x = pct_informal + 2.2), hjust = 0, size = 3.2) +
  scale_color_manual(values = c("Adequate n" = col_structure, "Low n" = col_low_n)) +
  scale_size_continuous(range = c(3, 8)) +
  scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Informal first-choice borrowing by region",
    subtitle = "Ordered regional comparison; low-n regions are flagged in red",
    x = "% naming an informal first source",
    y = NULL,
    caption = "Threshold used here: low n < 120 respondents with non-missing region and primary source."
  ) +
  theme_refined()

save_fig("RV3_region_informal_low_n_flagged", w = 11, h = 6.6)

# -----------------------------------------------------------------------------
# V4. Open-ended themes with counts and shares together
# -----------------------------------------------------------------------------
tee("\nV4: open-ended themes with counts and shares")

themes_tbl <- themes %>%
  filter(theme != "other_substantive") %>%
  slice_max(order_by = n_substantive, n = 10, with_ties = FALSE) %>%
  mutate(
    theme_label = recode(
      theme,
      interest_cost_reduction = "Interest / cost reduction",
      affordability_risk_controls = "Affordability and risk controls",
      repayment_discipline = "Repayment discipline",
      flexibility_restructure = "Flexibility / restructuring",
      financial_literacy_awareness = "Financial literacy / awareness",
      debt_avoidance = "Debt avoidance",
      anti_fraud_consumer_protection = "Fraud / consumer protection",
      income_employment_support = "Income / employment support",
      reminder_communication = "Reminder communication",
      legal_enforcement = "Legal enforcement",
      formal_channel_preference = "Formal channel preference",
      .default = str_replace_all(theme, "_", " ")
    ),
    label = sprintf("n=%d | %.1f%%", n_substantive, pct_of_substantive),
    theme_label = fct_reorder(theme_label, n_substantive)
  )

p4 <- ggplot(themes_tbl, aes(x = n_substantive, y = theme_label)) +
  geom_col(fill = col_neutral, width = 0.65) +
  geom_text(aes(label = label, x = n_substantive + 8), hjust = 0, size = 3.3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Open-ended policy themes",
    subtitle = "Top substantive themes; labels show both count and share of substantive responses",
    x = "Count of substantive responses",
    y = NULL
  ) +
  theme_refined()

save_fig("RV4_open_themes_count_and_share", w = 10.6, h = 6.2)

tee("\n=== 12_visual_refinements.R complete ===")




