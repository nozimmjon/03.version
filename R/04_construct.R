# =============================================================================
# 04_construct.R  —  Analytical dataset construction
# =============================================================================
# Pipeline:  data/intermediate/03_clean.rds  →  data/intermediate/04_analytical.rds
#                                             →  data/analytical/borrowers.rds
#                                             →  data/analytical/non_borrowers.rds
#                                             →  data/analytical/family_biz.rds
#            Column manifest written to:         logs/04_column_manifest.csv
#            Execution record appended to:       logs/04_construct.log
#
# What this script adds on top of 03_clean.rds:
#   A. Ordered factors   — ordinal single-choice vars get correct level ordering
#                          so table() / ggplot2 respect questionnaire ordering
#   B. Labelled flags    — employed_binary, repay_group, multiple_sources_flag
#   C. Score winsorizing — cap extreme outliers in numeric vars before analysis
#   D. Analytical splits — borrowers, non_borrowers, family_biz subsets
#   E. Export            — RDS (each subset) + CSV for the analytical folder
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(here)
})

# ── 0. Paths ──────────────────────────────────────────────────────────────────
in_rds        <- here("data", "intermediate", "03_clean.rds")
out_rds       <- here("data", "intermediate", "04_analytical.rds")
analytical_dir <- here("data", "analytical")
log_file      <- here("logs", "04_construct.log")
manifest_csv  <- here("logs", "04_column_manifest.csv")

dir.create(analytical_dir,                  showWarnings = FALSE, recursive = TRUE)
dir.create(here("data", "intermediate"),    showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"),                    showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(in_rds))

# ── 1. Load ───────────────────────────────────────────────────────────────────
df <- readRDS(in_rds)
cat(sprintf("Loaded 03_clean.rds : %d rows x %d cols\n\n", nrow(df), ncol(df)))

# ── A. Ordered factors ────────────────────────────────────────────────────────
# Apply questionnaire-aligned level ordering so that all downstream tables and
# ggplot2 charts display categories in the intended sequence without any
# manual reordering at the analysis stage.

cat("=== A. ORDERED FACTORS ===\n")

ordinal_specs <- list(
  # Income bands (hh_income_cat) — ascending
  hh_income_cat = c(
    "5 млн сўмгача",
    "5-10 млн сўм",
    "10 - 20 млн сўм",
    "20-50 млн сўм",
    "50 млн сўмдан юқори",
    "Жавоб беришга қийналаман (ўқиб берилмасин)"
  ),

  # Debt-service ratio (dsr_cat) — ascending
  dsr_cat = c(
    "0% (тўлов қилмайман)",
    "10% дан – кам",
    "10% дан – 20% гача",
    "20% дан – 35% гача",
    "35% дан – 50%гача",
    "50%дан кўп",
    "Жавоб бериш қийин (ўқилмасин)"
  ),

  # Financial buffer months (financial_buffer) — ascending resilience
  financial_buffer = c(
    "умуман эга эмасман",
    "1 - 2 ой",
    "3 - 4 ой",
    "5 - 6 ой",
    "6 ойдан кўпроқ",
    "Жавоб бериш қийин (ўқилмасин)"
  ),

  # Education — ascending
  education = c(
    "Ўрта таълим (мактаб, лицей)",
    "Ўрта махсус, касб-ҳунар таълими (техникум, коллеж)",
    "Олий таълим (бакалавр, магистр в.б.)"
  ),

  # Contract reading — ascending literacy
  contract_read = c(
    "Танишмаганман",
    "Ҳа, қисман танишганман",
    "Ҳа, тўлиқ танишганман"
  ),

  # Loan terms understanding — ascending
  terms_understood = c(
    "Тушунмаганман",
    "Тушунганман деб ўйладим, аммо кўпроқ тўлашга тўғри келди",
    "Ҳа"
  ),

  # Bank explanation quality — ascending
  bank_explained_terms = c(
    "Йўқ, умуман маълумот беришмаган",
    "Қисман маълумот беришган",
    "Ҳа, тўлиқ маълумот беришган"
  ),

  # Acceptable delay tolerance — ascending moral hazard
  acceptable_delay_days = c(
    "Ҳеч қандай кечикиш мумкин (нормал) эмас",
    "1-7 кун",
    "8-30 кун",
    "30 кундан ортиқ",
    "Тўламаса ҳам бўлади"
  ),

  # Payment history status — ascending severity
  holat1 = c(
    "Қарздорликни ўз вақтида тўлаётган мижозлар",
    "Қарздорликни 1-3 ой кечикиш билан тўлаётган мижозлар",
    "Қарздорликни 3 ой ва ундан кўп муддат (NPL) тўламаётган мижозлар"
  ),

  # Reminders per month — ascending frequency
  reminders_per_month = c(
    "Юборилмаган",
    "1-2 марта",
    "3-5 марта",
    "5 мартадан ортиқ"
  ),

  # Credit score awareness — descending unawareness
  credit_score_motivates = c(
    "Ҳа, кучли рағбатлантиради",
    "Ҳа, қисман рағбатлантиради",
    "Иккиланаман",
    "Йўқ",
    "Кредит рейтинги ҳақида маълумотга эга эмасман"
  ),

  # Knowledge of delay effect on credit history — ascending knowledge
  knows_delay_effect = c(
    "Йўқ, билмайман",
    "Жавоб бериш қийин (ўқилмасин)",
    "Қисман биламан",
    "Ҳа, аниқ биламан"
  ),

  # Peers repayment perception
  peers_repay_behavior = c(
    "Вақтида тўлашда жиддий муаммолар мавжуд",
    "Озгина кечикиш ҳолатлари бор",
    "Вақтида тўлашяпти",
    "Жавоб беришга қийналаман (ўқилмасин)"
  ),

  # Income variability — ascending volatility
  income_variability = c(
    "Барқарор, деярли ўзгармайди",
    "Бироз ўзгариб туради",
    "Жуда ўзгарувчан/мавсумий",
    "Жавоб бериш қийин (ўқилмасин)"
  ),

  # Loan channel — in-person vs digital
  loan_channel = c(
    "Офлайн (Банкга борган ҳолда)",
    "Онлайн"
  ),

  # Borrowing motivation
  borrow_motivation = c(
    "Вазиятдан чиқиш учун",
    "Ҳаёт тарзимни янада яхшилаш учун"
  ),

  # Loan regret
  loan_regret = c(
    "Йўқ",
    "Ҳа"
  ),

  # Loan used as intended
  loan_used_as_intended = c(
    "Ҳа",
    "Жавоб беришга қийналаман (ўқилмасин)",
    "Йўқ, бошқа мақсадда ишлатилди"
  ),

  # Straw borrower usage
  used_straw_borrower = c(
    "Йўқ",
    "Жавоб бериш қийин / Эслолмайман (ўқилмасин)",
    "Ҳа"
  )
)

n_factored <- 0L
for (var in names(ordinal_specs)) {
  if (!var %in% names(df)) next
  lvls <- ordinal_specs[[var]]
  # Include any unexpected values as extra unordered trailing levels
  extra <- setdiff(unique(df[[var]][!is.na(df[[var]])]), lvls)
  if (length(extra) > 0) {
    cat(sprintf("  NOTE: %s — unexpected values added as trailing levels: %s\n",
                var, paste(extra, collapse = ", ")))
    lvls <- c(lvls, extra)
  }
  df[[var]] <- factor(df[[var]], levels = lvls, ordered = TRUE)
  n_factored <- n_factored + 1L
  cat(sprintf("  [OK] %-30s  %d levels\n", var, length(lvls)))
}
cat(sprintf("\nOrdered factors applied: %d vars\n\n", n_factored))

# ── B. Additional analytical variables ────────────────────────────────────────
cat("=== B. ADDITIONAL ANALYTICAL VARIABLES ===\n")

# B.1  employed_binary: 1 if formally or self-employed
#   Actual Uzbek values in employment:
#     "Давлат ташкилотида ишлайман"                                          → government
#     "Хусусий секторда ёлланиб ишлайман (корхона, фирма, ташкилот в.б.)"  → private sector
#     "Ўз-ўзини банд қилган / мустақил тадбиркор"                           → self-employed
if ("employment" %in% names(df)) {
  df <- df %>%
    mutate(employed_binary = as.integer(
      !is.na(employment) & employment %in% c(
        "Давлат ташкилотида ишлайман",
        "Хусусий секторда ёлланиб ишлайман (корхона, фирма, ташкилот в.б.)",
        "Ўз-ўзини банд қилган / мустақил тадбиркор"
      )
    ))
  cat("  employed_binary       : 1 if employment in {government, private, self_employed}\n")
}

# B.2  repay_group: human-readable repayment status label (for charts/tables)
#   Derived from holat1 (admin pre-filled Uzbek labels):
#     "Қарздорликни ўз вақтида тўлаётган мижозлар"                        → On-time
#     "Қарздорликни 1-3 ой кечикиш билан тўлаётган мижозлар"             → 1-3 month delay
#     "Қарздорликни 3 ой ва ундан кўп муддат (NPL) тўламаётган мижозлар" → NPL (3+ months)
if ("holat1" %in% names(df)) {
  df <- df %>%
    mutate(repay_group = case_when(
      str_detect(as.character(holat1), fixed("NPL"))          ~ "NPL (3+ months)",
      str_detect(as.character(holat1), "1-3 ой кечикиш")     ~ "1-3 month delay",
      str_detect(as.character(holat1), "ўз вақтида тўлаётган") ~ "On-time",
      TRUE                                                     ~ NA_character_
    ))
  df$repay_group <- factor(df$repay_group,
                            levels = c("On-time", "1-3 month delay", "NPL (3+ months)"),
                            ordered = TRUE)
  cat("  repay_group           : labeled repayment status from holat1\n")
}

# B.3  has_multiple_sources: 1 if respondent selected 2+ items in credit_source_primary
cs_prim_cols <- names(df)[str_starts(names(df), "credit_source_primary__")]
if (length(cs_prim_cols) > 0) {
  df <- df %>%
    mutate(has_multiple_sources = as.integer(
      rowSums(across(all_of(cs_prim_cols)), na.rm = TRUE) >= 2
    ))
  cat("  has_multiple_sources  : 1 if 2+ sources selected in credit_source_primary\n")
}

# B.4  family_biz_flag: admin flag cross-checked with self-reported loan_purpose
if ("oilaviy_kredit" %in% names(df)) {
  df <- df %>% rename(family_biz_admin = oilaviy_kredit)
  cat("  family_biz_admin      : renamed from oilaviy_kredit (admin flag)\n")
}

# B.5  credit_type_label: human-readable version of kredit_turi (admin field)
#   Actual values in kredit_turi (Latin/transliterated from CAPI admin):
#     ipoteka, oilaviy, mikroqarz, istemol_kredit, avtokredit, kredit_karta, talim_krediti
if ("kredit_turi" %in% names(df)) {
  df <- df %>%
    mutate(credit_type_label = dplyr::recode(
      kredit_turi,
      ipoteka        = "Ипотека (mortgage)",
      oilaviy        = "Оилавий кредит (family business)",
      mikroqarz      = "Микроқарз",
      istemol_kredit = "Истеъмол кредити (consumer)",
      avtokredit     = "Автокредит",
      kredit_karta   = "Кредит карта",
      talim_krediti  = "Таълим кредити",
      .default       = kredit_turi
    ))
  cat("  credit_type_label     : human-readable kredit_turi\n")
}

# B.6  age_group: 5-band age grouping for demographic charts
if ("age" %in% names(df)) {
  df <- df %>%
    mutate(age_group = cut(
      age,
      breaks = c(17, 25, 35, 45, 55, Inf),
      labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
      right  = TRUE
    ))
  cat("  age_group             : 5-band cut from age (18-25, 26-35, 36-45, 46-55, 56+)\n")
}

# B.7  dependency_ratio: (hh_size - hh_workers) / hh_size — household dependency burden
if (all(c("hh_size", "hh_workers") %in% names(df))) {
  df <- df %>%
    mutate(dependency_ratio = if_else(
      !is.na(hh_size) & hh_size > 0 & !is.na(hh_workers),
      (hh_size - hh_workers) / hh_size,
      NA_real_
    ))
  cat("  dependency_ratio      : (hh_size - hh_workers) / hh_size\n")
}

# B.8  moral_hazard_flag: strong non-payment attitude signal
#   1 if any of: no_need_to_pay (delay_tolerance) OR not_necessary_to_repay (justified)
dts_col  <- "delay_tolerance_score"
jnp_cols <- names(df)[str_starts(names(df), "justified_nonpayment__")]

moral_sources <- character(0)
if (dts_col %in% names(df)) moral_sources <- c(moral_sources, dts_col)
if (length(jnp_cols) > 0)   moral_sources <- c(moral_sources, jnp_cols)

if (dts_col %in% names(df)) {
  jnp_not_nec <- intersect("justified_nonpayment__not_necessary_to_repay", names(df))
  if (length(jnp_not_nec) > 0) {
    df <- df %>%
      mutate(moral_hazard_flag = as.integer(
        (!is.na(delay_tolerance_score) & as.integer(delay_tolerance_score) == 4L) |
          (.data[[jnp_not_nec[1]]] == 1L)
      ))
  } else {
    df <- df %>%
      mutate(moral_hazard_flag = as.integer(
        !is.na(delay_tolerance_score) & as.integer(delay_tolerance_score) == 4L
      ))
  }
  cat("  moral_hazard_flag     : delay_tolerance == no_need_to_pay OR not_necessary_to_repay\n")
}

# B.9  compound_vulnerability_flag: high DSR + highly variable income
#   income_variability actual value for "highly variable": "Жуда ўзгарувчан/мавсумий"
if (all(c("dsr_midpoint", "income_variability") %in% names(df))) {
  df <- df %>%
    mutate(compound_vulnerability = as.integer(
      (!is.na(dsr_midpoint) & dsr_midpoint >= 0.35) &
        (!is.na(income_variability) &
           as.character(income_variability) == "Жуда ўзгарувчан/мавсумий")
    ))
  cat("  compound_vulnerability: dsr_midpoint >= 0.35 AND income_variability == highly_variable\n")
}

cat("\n")

# ── C. Numeric winsorizing ────────────────────────────────────────────────────
# Cap extreme outliers at [1%, 99%] for continuous numeric vars used in models.
# Winsorized columns get a _w suffix; originals are preserved.
cat("=== C. NUMERIC WINSORIZING ===\n")

winsorise <- function(x, lo = 0.01, hi = 0.99) {
  qs <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmax(pmin(x, qs[2]), qs[1])
}

winsor_vars <- intersect(
  c("age", "hh_size", "hh_workers", "children_under18", "n_loans",
    "dsr_midpoint", "income_midpoint_mln_uzs", "financial_buffer_months",
    "dependency_ratio"),
  names(df)
)

for (v in winsor_vars) {
  if (!is.numeric(df[[v]])) next
  n_changed <- sum(!is.na(df[[v]]) & df[[v]] != winsorise(df[[v]]))
  if (n_changed > 0) {
    df[[paste0(v, "_w")]] <- winsorise(df[[v]])
    cat(sprintf("  %-35s  %d values capped\n", paste0(v, " → ", v, "_w"), n_changed))
  }
}
cat("\n")

# ── D. Analytical splits ──────────────────────────────────────────────────────
cat("=== D. ANALYTICAL SPLITS ===\n")

stopifnot("has_loan" %in% names(df))

borrowers     <- df %>% filter(has_loan == 1L)
non_borrowers <- df %>% filter(has_loan == 0L)

# Family-business credit sub-sample (admin flag preferred; self-report fallback)
if ("family_biz_admin" %in% names(df)) {
  family_biz <- borrowers %>% filter(!is.na(family_biz_admin) & family_biz_admin == 1L)
} else if ("loan_purpose__family_entrepreneurship" %in% names(df)) {
  family_biz <- borrowers %>%
    filter(!is.na(loan_purpose__family_entrepreneurship) &
             loan_purpose__family_entrepreneurship == 1L)
} else {
  family_biz <- borrowers[0, ]   # empty with same schema
  cat("  NOTE: family_biz subset empty — no family biz flag or loan_purpose dummy found\n")
}

cat(sprintf("  Full dataset  : %d rows\n",  nrow(df)))
cat(sprintf("  Borrowers     : %d rows\n",  nrow(borrowers)))
cat(sprintf("  Non-borrowers : %d rows\n",  nrow(non_borrowers)))
cat(sprintf("  Family biz    : %d rows\n",  nrow(family_biz)))
cat(sprintf("  Unclassified  : %d rows\n\n",
            nrow(df) - nrow(borrowers) - nrow(non_borrowers)))

# ── E. Column manifest ────────────────────────────────────────────────────────
cat("=== E. COLUMN MANIFEST ===\n")

manifest <- tibble(
  col_name    = names(df),
  col_class   = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
  is_ordered  = vapply(df, function(x) is.ordered(x), logical(1)),
  n_levels    = vapply(df, function(x) if (is.factor(x)) nlevels(x) else NA_integer_, integer(1)),
  n_non_na    = vapply(df, function(x) sum(!is.na(x)), integer(1)),
  pct_missing = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
  n_unique    = vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1))
) %>%
  arrange(pct_missing)

cat(sprintf("Column manifest: %d columns\n", nrow(manifest)))
cat(sprintf("  ordered factors : %d\n",  sum(manifest$is_ordered,     na.rm = TRUE)))
cat(sprintf("  unordered factor: %d\n",
            sum(!manifest$is_ordered & !is.na(manifest$n_levels), na.rm = TRUE)))
cat(sprintf("  numeric/double  : %d\n",
            sum(str_detect(manifest$col_class, "numeric|double"))))
cat(sprintf("  integer         : %d\n",  sum(manifest$col_class == "integer")))
cat(sprintf("  character       : %d\n",  sum(manifest$col_class == "character")))
cat("\n")

# ── F. Write outputs ───────────────────────────────────────────────────────────
cat("=== F. WRITE OUTPUTS ===\n")

# Main analytical RDS
saveRDS(df, out_rds)
cat(sprintf("✓ %s  [%d x %d]\n", out_rds, nrow(df), ncol(df)))

# Analytical subsets (RDS)
subsets <- list(
  borrowers     = borrowers,
  non_borrowers = non_borrowers,
  family_biz    = family_biz
)
for (nm in names(subsets)) {
  path <- here("data", "analytical", paste0(nm, ".rds"))
  saveRDS(subsets[[nm]], path)
  cat(sprintf("✓ %s  [%d x %d]\n", path, nrow(subsets[[nm]]), ncol(subsets[[nm]])))
}

# Column manifest CSV
write_csv(manifest, manifest_csv)
cat(sprintf("✓ %s\n", manifest_csv))

# Append to execution log
log_line <- sprintf(
  "%s  04_construct  rows=%d  cols=%d  borrowers=%d  non_borrowers=%d  family_biz=%d",
  format(Sys.time()),
  nrow(df), ncol(df),
  nrow(borrowers), nrow(non_borrowers), nrow(family_biz)
)
write(log_line, file = log_file, append = TRUE)
cat(sprintf("✓ Log appended: %s\n", log_file))

cat("\n=== 04_construct COMPLETE ===\n")
