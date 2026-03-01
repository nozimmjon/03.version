# =============================================================================
# 06_analyse.R  —  Analysis: descriptive statistics, cross-tabs, logistic regression
# =============================================================================
# Input:   data/intermediate/04_analytical.rds
# Outputs: outputs/tables/analysis/  (CSV tables + console log)
#          logs/06_analyse.log
#
# Sections:
#   A. Sample composition
#   B. Demographics by borrower status
#   C. Credit profile (borrowers only)
#   D. Financial literacy by repayment status
#   E. Cross-tabulations: key variables × NPL status
#   F. Logistic regression (NPL among borrowers) — GLM, odds ratios, AUC
#   G. Family entrepreneurship sub-analysis
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(stringr)
  library(here)
})

# Optional packages with graceful fallback
has_broom <- requireNamespace("broom", quietly = TRUE)
has_proc  <- requireNamespace("pROC",  quietly = TRUE)

# ── 0. Paths ──────────────────────────────────────────────────────────────────
in_rds    <- here("data", "intermediate", "04_analytical.rds")
out_dir   <- here("outputs", "tables", "analysis")
log_file  <- here("logs", "06_analyse.log")

dir.create(out_dir,         showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"),    showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(in_rds))

# ── Helpers ───────────────────────────────────────────────────────────────────

# Write a CSV table + print a header to console
save_table <- function(tbl, name, caption = NULL) {
  path <- file.path(out_dir, paste0(name, ".csv"))
  write_csv(tbl, path)
  if (!is.null(caption)) cat(sprintf("\n=== %s ===\n", toupper(caption)))
  print(tbl, n = Inf)
  invisible(tbl)
}

# Frequency + percent table for one variable, optionally by group
freq_pct <- function(data, var, group = NULL, label_col = NULL) {
  var  <- rlang::sym(var)
  if (is.null(group)) {
    out <- data %>%
      count(!!var) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
  } else {
    group <- rlang::sym(group)
    out <- data %>%
      count(!!group, !!var) %>%
      group_by(!!group) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()
  }
  if (!is.null(label_col)) out <- rename(out, category = !!rlang::sym(deparse(var)))
  out
}

# Tee: write to log AND console
tee <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

# ── 1. Load data ──────────────────────────────────────────────────────────────
cat(sprintf("\n%-60s %s\n", "=== 06_analyse.R", format(Sys.time(), "%Y-%m-%d %H:%M")))
writeLines("", log_file)  # reset log

df <- readRDS(in_rds)
tee(sprintf("Loaded 04_analytical.rds : %d rows × %d cols", nrow(df), ncol(df)))

# Analytical subsets
borrowers  <- df %>% filter(has_loan == 1L)
non_borrow <- df %>% filter(has_loan == 0L)

# Family-biz: use admin flag (consistent with 04_construct.R); fall back to kredit_turi
if ("family_biz_admin" %in% names(df)) {
  family_biz <- borrowers %>% filter(!is.na(family_biz_admin) & family_biz_admin == 1L)
  tee("  family_biz_admin flag used for family_biz subset")
} else {
  family_biz <- borrowers %>% filter(kredit_turi == "oilaviy")
  tee("  kredit_turi == 'oilaviy' used (family_biz_admin not found)")
}

tee(sprintf("Borrowers    : %d", nrow(borrowers)))
tee(sprintf("Non-borrowers: %d", nrow(non_borrow)))
tee(sprintf("Family biz   : %d (oilaviy kredit)", nrow(family_biz)))

# NPL / status groups (borrowers only)
npl_grp     <- borrowers %>% filter(is_npl == 1L)
delayed_grp <- borrowers %>% filter(is_delayed == 1L, is_npl == 0L)
ontime_grp  <- borrowers %>% filter(is_npl == 0L, is_delayed == 0L)

tee(sprintf("\nRepayment groups among borrowers:"))
tee(sprintf("  NPL (3+ months) : %d  (%.1f%%)", nrow(npl_grp),
            nrow(npl_grp) / nrow(borrowers) * 100))
tee(sprintf("  1-3 month delay : %d  (%.1f%%)", nrow(delayed_grp),
            nrow(delayed_grp) / nrow(borrowers) * 100))
tee(sprintf("  On-time         : %d  (%.1f%%)", nrow(ontime_grp),
            nrow(ontime_grp) / nrow(borrowers) * 100))

# ── A. SAMPLE COMPOSITION ──────────────────────────────────────────────────────
tee("\n\n====== A. SAMPLE COMPOSITION ======")

# A1: Region distribution
a1 <- df %>%
  count(region) %>%
  mutate(
    pct_total    = round(n / sum(n) * 100, 1),
    n_borrower   = vapply(region,
                          function(r) sum(borrowers$region == r, na.rm = TRUE),
                          integer(1)),
    n_nonborrow  = vapply(region,
                          function(r) sum(non_borrow$region == r, na.rm = TRUE),
                          integer(1)),
    pct_borrow   = round(n_borrower / n * 100, 1)
  ) %>%
  arrange(desc(n))

save_table(a1, "A1_region_distribution", "A1. Regional distribution")

# A2: Borrower status totals  (built AFTER family_biz is defined)
a2 <- tibble(
  group     = c("All respondents", "Borrowers", "Non-borrowers", "Family biz (admin flag)"),
  n         = c(nrow(df), nrow(borrowers), nrow(non_borrow), nrow(family_biz)),
  pct_of_all = round(c(nrow(df), nrow(borrowers), nrow(non_borrow), nrow(family_biz)) /
                       nrow(df) * 100, 1)
)
save_table(a2, "A2_sample_totals", "A2. Sample totals")

# ── B. DEMOGRAPHICS BY BORROWER STATUS ────────────────────────────────────────
tee("\n\n====== B. DEMOGRAPHICS ======")

# B1: Age summary
b1 <- df %>%
  mutate(group = if_else(has_loan == 1L, "Borrowers", "Non-borrowers")) %>%
  group_by(group) %>%
  summarise(
    n     = n(),
    mean  = round(mean(age, na.rm = TRUE), 1),
    sd    = round(sd(age,  na.rm = TRUE), 1),
    p25   = round(quantile(age, 0.25, na.rm = TRUE), 0),
    med   = round(median(age, na.rm = TRUE), 0),
    p75   = round(quantile(age, 0.75, na.rm = TRUE), 0),
    .groups = "drop"
  )
save_table(b1, "B1_age_summary", "B1. Age by borrower status")

# B2: Gender
b2 <- df %>%
  mutate(group = if_else(has_loan == 1L, "Borrowers", "Non-borrowers")) %>%
  count(group, gender) %>%
  group_by(group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(group, desc(n))
save_table(b2, "B2_gender", "B2. Gender by borrower status")

# B3: Education
b3 <- df %>%
  mutate(group = if_else(has_loan == 1L, "Borrowers", "Non-borrowers")) %>%
  count(group, education) %>%
  group_by(group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()
save_table(b3, "B3_education", "B3. Education by borrower status")

# B4: Employment
b4 <- df %>%
  mutate(group = if_else(has_loan == 1L, "Borrowers", "Non-borrowers")) %>%
  count(group, employment) %>%
  group_by(group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(group, desc(n))
save_table(b4, "B4_employment", "B4. Employment by borrower status")

# B5: Income category
b5 <- df %>%
  mutate(group = if_else(has_loan == 1L, "Borrowers", "Non-borrowers")) %>%
  count(group, hh_income_cat) %>%
  group_by(group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()
save_table(b5, "B5_income_category", "B5. Income category by borrower status")

# B6: Household size
b6 <- df %>%
  mutate(group = if_else(has_loan == 1L, "Borrowers", "Non-borrowers")) %>%
  group_by(group) %>%
  summarise(
    n    = n(),
    mean = round(mean(hh_size,   na.rm = TRUE), 2),
    sd   = round(sd(hh_size,     na.rm = TRUE), 2),
    med  = round(median(hh_size, na.rm = TRUE), 0),
    .groups = "drop"
  )
save_table(b6, "B6_household_size", "B6. Household size by borrower status")

# ── C. CREDIT PROFILE (BORROWERS ONLY) ────────────────────────────────────────
tee("\n\n====== C. CREDIT PROFILE (BORROWERS) ======")

# C1: Credit type distribution
c1 <- borrowers %>%
  count(kredit_turi) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))
save_table(c1, "C1_credit_type", "C1. Credit type distribution (borrowers)")

# C2: Repayment status
c2 <- borrowers %>%
  filter(!is.na(holat1)) %>%
  count(holat1) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))
save_table(c2, "C2_repayment_status", "C2. Repayment status (borrowers)")

# C3: DSR category
if ("dsr_cat" %in% names(borrowers)) {
  c3 <- borrowers %>%
    filter(!is.na(dsr_cat)) %>%
    count(dsr_cat) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
  save_table(c3, "C3_dsr_category", "C3. DSR category (borrowers)")
}

# C4: Financial buffer
if ("financial_buffer" %in% names(borrowers)) {
  c4 <- borrowers %>%
    filter(!is.na(financial_buffer)) %>%
    count(financial_buffer) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
  save_table(c4, "C4_financial_buffer", "C4. Financial buffer (borrowers)")
}

# C5: Income variability
if ("income_variability" %in% names(borrowers)) {
  c5 <- borrowers %>%
    filter(!is.na(income_variability)) %>%
    count(income_variability) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
  save_table(c5, "C5_income_variability", "C5. Income variability (borrowers)")
}

# C6: Straw borrowing / name-lending prevalence
c6 <- tibble(
  indicator = c("is_straw_borrower (borrowed under another's name)",
                "is_borrowed_under_other_name (name lent to another)",
                "productive_use_flag (business / migration / family biz)"),
  n_among_borrowers = c(
    if ("is_straw_borrower"          %in% names(borrowers)) sum(borrowers$is_straw_borrower,          na.rm = TRUE) else NA_integer_,
    if ("is_borrowed_under_other_name" %in% names(borrowers)) sum(borrowers$is_borrowed_under_other_name, na.rm = TRUE) else NA_integer_,
    if ("productive_use_flag"         %in% names(borrowers)) sum(borrowers$productive_use_flag,         na.rm = TRUE) else NA_integer_
  ),
  pct = round(n_among_borrowers / nrow(borrowers) * 100, 1)
)
save_table(c6, "C6_special_flags", "C6. Special flags among borrowers")

# ── D. FINANCIAL LITERACY ──────────────────────────────────────────────────────
tee("\n\n====== D. FINANCIAL LITERACY ======")

# D1: fin_lit_score distribution (all vs borrowers)
if ("fin_lit_score" %in% names(df)) {
  d1 <- bind_rows(
    df         %>% mutate(group = "All")          %>% group_by(group) %>%
      summarise(mean_score = round(mean(fin_lit_score, na.rm=TRUE), 2),
                sd         = round(sd(fin_lit_score,   na.rm=TRUE), 2),
                n          = n(), .groups="drop"),
    borrowers  %>% mutate(group = "Borrowers")    %>% group_by(group) %>%
      summarise(mean_score = round(mean(fin_lit_score, na.rm=TRUE), 2),
                sd         = round(sd(fin_lit_score,   na.rm=TRUE), 2),
                n          = n(), .groups="drop"),
    non_borrow %>% mutate(group = "Non-borrowers") %>% group_by(group) %>%
      summarise(mean_score = round(mean(fin_lit_score, na.rm=TRUE), 2),
                sd         = round(sd(fin_lit_score,   na.rm=TRUE), 2),
                n          = n(), .groups="drop")
  )
  save_table(d1, "D1_fin_lit_summary", "D1. Financial literacy score summary")

  # D2: fin_lit_score by repayment status (borrowers only)
  d2 <- borrowers %>%
    filter(!is.na(repay_group)) %>%
    group_by(repay_group) %>%
    summarise(
      n          = n(),
      mean_score = round(mean(fin_lit_score, na.rm=TRUE), 2),
      sd         = round(sd(fin_lit_score,   na.rm=TRUE), 2),
      .groups = "drop"
    )
  save_table(d2, "D2_fin_lit_by_status", "D2. Financial literacy by repayment status")
}

# D3: Contract reading by repayment status
if ("contract_read" %in% names(borrowers)) {
  d3 <- borrowers %>%
    filter(!is.na(repay_group), !is.na(contract_read)) %>%
    count(repay_group, contract_read) %>%
    group_by(repay_group) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  save_table(d3, "D3_contract_read_by_status", "D3. Contract reading by repayment status")
}

# D4: Terms understood by repayment status
if ("terms_understood" %in% names(borrowers)) {
  d4 <- borrowers %>%
    filter(!is.na(repay_group), !is.na(terms_understood)) %>%
    count(repay_group, terms_understood) %>%
    group_by(repay_group) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  save_table(d4, "D4_terms_understood_by_status", "D4. Terms understood by repayment status")
}

# D5: Delay tolerance by repayment status
if ("delay_tolerance_score" %in% names(borrowers)) {
  d5 <- borrowers %>%
    filter(!is.na(repay_group)) %>%
    group_by(repay_group) %>%
    summarise(
      n              = n(),
      mean_tolerance = round(mean(delay_tolerance_score, na.rm=TRUE), 2),
      pct_score0     = round(mean(delay_tolerance_score == 0L, na.rm=TRUE)*100, 1),
      pct_score4     = round(mean(delay_tolerance_score == 4L, na.rm=TRUE)*100, 1),
      .groups = "drop"
    )
  save_table(d5, "D5_delay_tolerance_by_status", "D5. Delay tolerance by repayment status")
}

# ── E. CROSS-TABULATIONS: KEY VARS × NPL STATUS ────────────────────────────────
tee("\n\n====== E. CROSS-TABULATIONS ======")

# Helper: cross-tab of a factor var vs is_npl (among borrowers)
crosstab_npl <- function(var_name) {
  if (!var_name %in% names(borrowers)) {
    cat(sprintf("  [SKIP] %s not in dataset\n", var_name)); return(invisible(NULL))
  }
  var <- rlang::sym(var_name)
  borrowers %>%
    filter(!is.na(!!var)) %>%
    group_by(!!var) %>%
    summarise(
      n_total    = n(),
      n_npl      = sum(is_npl, na.rm = TRUE),
      npl_rate   = round(mean(is_npl, na.rm = TRUE) * 100, 1),
      n_ontime   = sum(is_npl == 0L & is_delayed == 0L, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(var = var_name) %>%
    rename(category = !!var) %>%
    mutate(category = as.character(category)) %>%
    select(var, category, n_total, n_npl, npl_rate, n_ontime)
}

e_list <- lapply(
  c("hh_income_cat", "dsr_cat", "income_variability", "financial_buffer",
    "education", "gender", "kredit_turi", "employed_binary"),
  crosstab_npl
)
e1 <- bind_rows(e_list)
save_table(e1, "E1_crosstabs_npl", "E1. Cross-tabs: key variables × NPL rate (borrowers)")

# E2: Numeric means by NPL status
num_vars <- c("age", "hh_size", "income_midpoint_mln_uzs", "dsr_midpoint",
              "financial_buffer_months", "fin_lit_score", "delay_tolerance_score")
num_vars <- intersect(num_vars, names(borrowers))

e2 <- borrowers %>%
  filter(!is.na(is_npl)) %>%
  group_by(is_npl) %>%
  summarise(across(all_of(num_vars),
                   list(mean = ~ round(mean(.x, na.rm=TRUE), 2),
                        sd   = ~ round(sd(.x,   na.rm=TRUE), 2)),
                   .names = "{.col}__{.fn}"),
            n = n(),
            .groups = "drop") %>%
  mutate(status = if_else(is_npl == 1L, "NPL", "Non-NPL")) %>%
  select(status, n, everything(), -is_npl)
save_table(e2, "E2_numeric_means_by_npl", "E2. Numeric variable means by NPL status")

# ── F. LOGISTIC REGRESSION (NPL AMONG BORROWERS) ──────────────────────────────
tee("\n\n====== F. LOGISTIC REGRESSION ======")

# Build model dataset
# Encode categorical predictors as numeric where needed
if ("income_variability" %in% names(borrowers)) {
  borrowers <- borrowers %>%
    mutate(income_variability_num = case_when(
      str_detect(as.character(income_variability), "Барқарор")  ~ 1L,
      str_detect(as.character(income_variability), "Бироз")     ~ 2L,
      str_detect(as.character(income_variability), "Жуда")      ~ 3L,
      TRUE                                                       ~ NA_integer_
    ))
} else {
  borrowers <- borrowers %>% mutate(income_variability_num = NA_integer_)
}

# Encode gender and education as binary
borrowers <- borrowers %>%
  mutate(
    is_male       = as.integer(!is.na(gender)    & str_detect(as.character(gender),    "Эркак")),
    is_married    = as.integer(!is.na(marital_status) &
                                 str_detect(as.character(marital_status), "Турмуш")),
    has_higher_edu = as.integer(!is.na(education) & str_detect(as.character(education), "Олий"))
  )

# Predictor set
model_predictors <- c(
  "age", "is_male", "is_married", "has_higher_edu",
  "income_midpoint_mln_uzs", "dsr_midpoint", "financial_buffer_months",
  "income_variability_num", "fin_lit_score", "employed_binary",
  "hh_size", "productive_use_flag"
)
model_predictors <- intersect(model_predictors, names(borrowers))

model_df <- borrowers %>%
  select(is_npl, all_of(model_predictors)) %>%
  filter(!is.na(is_npl)) %>%
  na.omit()

tee(sprintf("Model sample: N=%d (after listwise deletion; N=%d before)",
            nrow(model_df), nrow(borrowers)))
tee(sprintf("NPL rate in model sample: %.1f%%",
            mean(model_df$is_npl) * 100))
tee(sprintf("Predictors used: %s", paste(model_predictors, collapse = ", ")))

# Fit model
frm <- as.formula(paste("is_npl ~", paste(model_predictors, collapse = " + ")))
fit <- glm(frm, data = model_df, family = binomial(link = "logit"))

tee("\n--- GLM Summary ---")
print(summary(fit))

# Tidy coefficient table with odds ratios and 95% CI
coef_raw <- summary(fit)$coefficients
coef_ci  <- suppressMessages(confint(fit))  # profile-likelihood CI

f1 <- tibble(
  term      = rownames(coef_raw),
  estimate  = coef_raw[, "Estimate"],
  std_err   = coef_raw[, "Std. Error"],
  z_value   = coef_raw[, "z value"],
  p_value   = coef_raw[, "Pr(>|z|)"],
  OR        = exp(estimate),
  CI_lower  = exp(coef_ci[rownames(coef_raw), 1]),
  CI_upper  = exp(coef_ci[rownames(coef_raw), 2])
) %>%
  mutate(
    across(c(estimate, std_err, z_value, OR, CI_lower, CI_upper),
           ~ round(.x, 4)),
    p_value    = round(p_value, 4),
    signif     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE             ~ ""
    )
  ) %>%
  arrange(p_value)

save_table(f1, "F1_logistic_coefficients",
           "F1. Logistic regression: coefficients and odds ratios")

# Model fit statistics
null_deviance <- fit$null.deviance
resid_dev     <- fit$deviance
df_null       <- fit$df.null
df_resid      <- fit$df.residual
pseudo_r2     <- round(1 - resid_dev / null_deviance, 4)   # McFadden R²
aic           <- round(AIC(fit), 2)

tee(sprintf("\n--- Model fit ---"))
tee(sprintf("  Null deviance  : %.2f  (df = %d)", null_deviance, df_null))
tee(sprintf("  Resid deviance : %.2f  (df = %d)", resid_dev,     df_resid))
tee(sprintf("  McFadden R²    : %.4f", pseudo_r2))
tee(sprintf("  AIC            : %.2f", aic))

# Hosmer-Lemeshow goodness of fit (simple decile split)
model_df$pred_prob <- predict(fit, type = "response")
model_df$decile    <- cut(model_df$pred_prob,
                          breaks = quantile(model_df$pred_prob,
                                            probs = seq(0, 1, 0.1), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
hl_tbl <- model_df %>%
  group_by(decile) %>%
  summarise(observed  = sum(is_npl),
            expected  = round(sum(pred_prob), 1),
            n         = n(),
            .groups = "drop")
tee("\n--- Hosmer-Lemeshow (decile calibration) ---")
print(hl_tbl)

# AUC (if pROC installed)
auc_val <- NA_real_
if (has_proc) {
  roc_obj <- pROC::roc(model_df$is_npl, model_df$pred_prob, quiet = TRUE)
  auc_val <- round(as.numeric(pROC::auc(roc_obj)), 4)
  tee(sprintf("  AUC (ROC)      : %.4f", auc_val))
} else {
  tee("  AUC            : pROC not installed — skipped")
}

# Model summary table
f2 <- tibble(
  metric  = c("N (model sample)", "N (full borrowers)", "NPL rate (%)",
               "Predictors", "McFadden R²", "AIC", "AUC"),
  value   = c(nrow(model_df), nrow(borrowers),
               round(mean(model_df$is_npl) * 100, 1),
               length(model_predictors),
               pseudo_r2, aic, auc_val)
)
save_table(f2, "F2_model_summary", "F2. Logistic model summary statistics")

# ── G. FAMILY ENTREPRENEURSHIP ANALYSIS ───────────────────────────────────────
tee("\n\n====== G. FAMILY ENTREPRENEURSHIP ======")

# Other borrowers for comparison
other_borrow <- borrowers %>% filter(kredit_turi != "oilaviy")

tee(sprintf("Family biz borrowers: N=%d", nrow(family_biz)))
tee(sprintf("Other borrowers     : N=%d", nrow(other_borrow)))

# G1: Demographic comparison
g1_vars <- c("age", "income_midpoint_mln_uzs", "fin_lit_score",
             "dsr_midpoint", "financial_buffer_months")
g1_vars <- intersect(g1_vars, names(df))

g1 <- bind_rows(
  family_biz  %>% mutate(group = "Family biz"),
  other_borrow %>% mutate(group = "Other borrowers")
) %>%
  group_by(group) %>%
  summarise(
    n          = n(),
    npl_rate   = round(mean(is_npl, na.rm=TRUE)*100, 1),
    across(all_of(g1_vars),
           ~ round(mean(.x, na.rm=TRUE), 2),
           .names = "mean_{.col}"),
    pct_male         = round(mean(str_detect(as.character(gender), "Эркак"), na.rm=TRUE)*100, 1),
    pct_higher_edu   = round(mean(str_detect(as.character(education), "Олий"), na.rm=TRUE)*100, 1),
    .groups = "drop"
  )
save_table(g1, "G1_family_vs_other_borrowers", "G1. Family biz vs other borrowers")

# G2: NPL rate by credit type (sorted)
g2 <- borrowers %>%
  filter(!is.na(kredit_turi), !is.na(is_npl)) %>%
  group_by(kredit_turi) %>%
  summarise(
    n        = n(),
    n_npl    = sum(is_npl),
    npl_rate = round(mean(is_npl) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(npl_rate))
save_table(g2, "G2_npl_by_credit_type", "G2. NPL rate by credit type")

# G3: NPL rate by credit type and income category
g3 <- borrowers %>%
  filter(!is.na(kredit_turi), !is.na(hh_income_cat), !is.na(is_npl)) %>%
  group_by(kredit_turi, hh_income_cat) %>%
  summarise(
    n        = n(),
    npl_rate = round(mean(is_npl) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%   # only cells with sufficient n
  arrange(kredit_turi, hh_income_cat)
save_table(g3, "G3_npl_credit_type_income", "G3. NPL rate by credit type × income")

# ── FINAL SUMMARY ──────────────────────────────────────────────────────────────
tee("\n\n====== ANALYSIS COMPLETE ======")

# List all tables written
tables_written <- list.files(out_dir, pattern = "\\.csv$", full.names = FALSE)
tee(sprintf("Tables written to: %s", out_dir))
for (t in sort(tables_written)) tee(sprintf("  %s", t))

tee(sprintf("\n06_analyse.R finished: %s", format(Sys.time(), "%Y-%m-%d %H:%M")))
