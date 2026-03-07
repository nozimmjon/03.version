# =============================================================================
# 03_clean.R  —  Type casting, multi-select expansion, derived variables
# =============================================================================
# Pipeline:  data/intermediate/01_raw.rds   →  data/intermediate/03_clean.rds
#            Rename log written to:            logs/03_multiselect_rename_log.csv
#            Execution record appended to:     logs/03_clean.log
#
# Processing steps:
#   1.  Load inputs (01_raw.rds + codebook)
#   2.  Helpers: norm_text(), to01(), slug()
#   3.  Parse codebook by q_type
#   4.  Normalize open-text fields (trim, collapse whitespace, blank → NA)
#   5.  Type-cast numeric columns → as.numeric()
#   6.  Type-cast binary admin columns → 0L / 1L
#   7.  Multi-select dummy expansion:
#         · detect dummy columns by raw_col_name stem + "/" prefix
#         · convert raw values → 0/1 integer (handles "Ҳа"/"Йўқ" and "1"/"0")
#         · rename: {var_name}__{option_code}  via positional mapping from codebook;
#           fall back to slugified Uzbek label when count mismatches
#   8.  Matrix block processing (Q2.1, Q2.3, Q3.3): convert to 0/1 integer
#   9.  Derived variable construction (all DERIVED rows in codebook):
#         has_loan, is_straw_borrower, is_borrowed_under_other_name,
#         is_npl, is_delayed, dsr_midpoint, income_midpoint_mln_uzs,
#         financial_buffer_months, multi_debt_flag, productive_use_flag,
#         over_indebted_flag, delay_tolerance_score, fin_lit_score,
#         credit_formality
#  10.  Final column inventory + remaining "/" audit
#  11.  Write outputs
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

if (!requireNamespace("stringi", quietly = TRUE))
  stop("Package 'stringi' required.  Install with: install.packages('stringi')")

# Unicode NFC normalization helper (fixes encoding mismatches between
# codebook CSV and raw Excel column names from CAPI)
nfc <- function(x) stringi::stri_trans_nfc(str_trim(x))

# ── 0. Paths ──────────────────────────────────────────────────────────────────
raw_rds          <- here("data", "intermediate", "01_raw.rds")
cb_csv           <- here("codebook", "codebook.csv")
out_rds          <- here("data", "intermediate", "03_clean.rds")
log_file         <- here("logs", "03_clean.log")
rename_log_csv   <- here("logs", "03_multiselect_rename_log.csv")

dir.create(here("data", "intermediate"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"),                 showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(raw_rds), file.exists(cb_csv))

# ── 1. Load ───────────────────────────────────────────────────────────────────
df <- readRDS(raw_rds)
cb <- read_csv(cb_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat(sprintf("Loaded 01_raw.rds : %d rows x %d cols\n", nrow(df), ncol(df)))
cat(sprintf("Codebook rows     : %d\n\n", nrow(cb)))

# ── 2. Helpers ────────────────────────────────────────────────────────────────

# Trim + collapse whitespace; blank / null-like strings → NA_character_
norm_text <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", " ")
  x[x %in% c("nan", "NaN", "None", "NULL", "")] <- NA_character_
  x
}

# Convert any truthy representation to strict 0L / 1L
to01 <- function(x) {
  if (is.na(x))        return(0L)
  if (is.logical(x))   return(ifelse(x, 1L, 0L))
  if (is.numeric(x))   return(ifelse(x != 0, 1L, 0L))
  s <- str_to_lower(str_trim(as.character(x)))
  if (s %in% c("1", "true",  "ҳa", "ha",    "yes", "y"))   return(1L)
  if (s %in% c("0", "false", "йўқ", "yo'q", "yo'q",
               "yoq", "no",  "n"))                           return(0L)
  suppressWarnings(v <- as.numeric(s))
  if (!is.na(v)) return(ifelse(v != 0, 1L, 0L))
  0L
}

# Make a safe, lowercase R identifier from arbitrary text
slug <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_to_lower(x)
  x <- str_replace_all(x, "\\s+", "_")
  x <- str_replace_all(x, "[^0-9a-z_]", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x[x == ""] <- "unknown"
  x
}

# Convert Q2.1 / Q3.3 matrix cell values → 0L / 1L / NA_integer_.
# Both blocks use the same binary Uzbek response scale:
#   "Мақсадга мувофиқ"     → 1L  (appropriate / yes)
#   "Мақсадга мувофиқ эмас"→ 0L  (not appropriate / no)
# NA / blank cells → NA_integer_ (non-response ≠ "no").
# NOTE: Q2.3 uses ordinal loan-amount ranges and is handled separately by q23_to_mln().
matrix_to01 <- function(x) {
  if (is.na(x))  return(NA_integer_)
  s <- str_trim(as.character(x))
  if (s == "")   return(NA_integer_)
  if (s == "Мақсадга мувофиқ")      return(1L)
  if (s == "Мақсадга мувофиқ эмас") return(0L)
  suppressWarnings(v <- as.numeric(s))
  if (!is.na(v)) return(ifelse(v != 0, 1L, 0L))
  0L  # unrecognised string → 0 (conservative)
}

# Convert Q2.3 credit-preference matrix cells → numeric midpoint (million UZS).
# Q2.3: "Depending on loan amount, from which sources would you prefer to borrow?"
# The respondent selects the MAXIMUM AMOUNT they would take from each source:
#   "0 (олмайман)"  →   0.0 M  (would not borrow from this source at all)
#   "5 млн сўмгача" →   2.5 M  (up to 5 M — midpoint)
#   "5-20 млн"      →  12.5 M  (5–20 M midpoint)
#   "20-50 млн"     →  35.0 M  (20–50 M midpoint)
#   "50-100 млн"    →  75.0 M  (50–100 M midpoint)
#   "100+ млн"      → 150.0 M  (open-ended upper band; 150 M by convention)
# Result is double (million UZS). 0 = unwilling; positive = willing up to that amount.
q23_to_mln <- function(x) {
  if (is.na(x)) return(NA_real_)
  s <- str_trim(as.character(x))
  switch(s,
    "0 (олмайман)"  =   0.0,
    "5 млн сўмгача" =   2.5,
    "5-20 млн"      =  12.5,
    "20-50 млн"     =  35.0,
    "50-100 млн"    =  75.0,
    "100+ млн"      = 150.0,
    NA_real_          # unrecognised value → NA
  )
}

# ── 3. Parse codebook ─────────────────────────────────────────────────────────
cb_work <- cb %>%
  filter(section != "derived", var_name %in% names(df))

cb_numeric <- cb_work %>% filter(q_type == "numeric")
cb_binary  <- cb_work %>% filter(q_type == "binary")
cb_open    <- cb_work %>% filter(q_type == "open_text")
cb_date    <- cb_work %>% filter(q_type == "date")

# Multi-select rows: raw_col_name is the CAPI question stem (without "/" + option)
cb_ms <- cb %>%
  filter(
    section  != "derived",
    q_type   == "multi_select",
    !is.na(raw_col_name),
    !str_starts(raw_col_name, "NOT IN RAW"),
    !str_starts(raw_col_name, "DERIVED"),
    !str_detect(raw_col_name, fixed("(matrix"))
  ) %>%
  select(var_name, raw_col_name, response_options)

cat(sprintf("Codebook vars present in raw  : %d\n", nrow(cb_work)))
cat(sprintf("  numeric                     : %d\n", nrow(cb_numeric)))
cat(sprintf("  binary (non-derived)        : %d\n", nrow(cb_binary)))
cat(sprintf("  open_text                   : %d\n", nrow(cb_open)))
cat(sprintf("  multi_select (to expand)    : %d\n", nrow(cb_ms)))
cat("\n")

# ── 4. Normalize open-text fields ─────────────────────────────────────────────
cat("=== STEP 4: OPEN-TEXT NORMALIZATION ===\n")
open_present <- intersect(cb_open$var_name, names(df))
cat(sprintf("Open-text vars: %d expected, %d present in raw\n",
            length(cb_open$var_name), length(open_present)))

df <- df %>%
  mutate(across(all_of(open_present), norm_text))

cat("Done.\n\n")

# ── 5. Type-cast: numeric ─────────────────────────────────────────────────────
cat("=== STEP 5: NUMERIC TYPE CASTING ===\n")
num_present <- intersect(cb_numeric$var_name, names(df))
cat(sprintf("Numeric vars: %d expected, %d present\n",
            nrow(cb_numeric), length(num_present)))

df <- df %>%
  mutate(across(all_of(num_present),
                ~ suppressWarnings(as.numeric(.x))))

# Report any that failed (all NA after conversion)
for (v in num_present) {
  n_na  <- sum(is.na(df[[v]]))
  n_tot <- nrow(df)
  if (n_na == n_tot)
    cat(sprintf("  WARNING: %s — all values are NA after as.numeric()\n", v))
}
cat("Done.\n\n")

# ── 6. Type-cast: binary admin columns ────────────────────────────────────────
cat("=== STEP 6: BINARY ADMIN TYPE CASTING ===\n")
bin_present <- intersect(cb_binary$var_name, names(df))
if (length(bin_present) > 0) {
  df <- df %>%
    mutate(across(all_of(bin_present),
                  ~ vapply(.x, to01, integer(1))))
  cat(sprintf("Binary vars converted to 0/1 : %s\n",
              paste(bin_present, collapse = ", ")))
} else {
  cat("No non-derived binary vars found in raw.\n")
}
cat("\n")

# ── 7. Multi-select dummy expansion ───────────────────────────────────────────
cat("=== STEP 7: MULTI-SELECT DUMMY EXPANSION ===\n")

# All columns with "/" in their name (CAPI dummy columns)
all_slash_cols <- names(df)[str_detect(names(df), fixed("/"))]
cat(sprintf("Total '/' columns in raw      : %d\n", length(all_slash_cols)))

# Detect matrix columns to exclude from generic dummy processing.
# Matrix blocks follow the CAPI pattern: "N-{q_number} <question text>/<option>"
# where N is a numeric item counter. We protect Q2.1, Q2.3, and Q3.3.
matrix_stem_patterns <- paste(
  c("^\\d+-2\\.1[\\. ]", "^\\d+-2\\.3[\\. ]", "^\\d+-3\\.3[\\. ]"),
  collapse = "|"
)
matrix_slash_cols <- all_slash_cols[str_detect(all_slash_cols, matrix_stem_patterns)]
cat(sprintf("Matrix '/' columns (protected): %d\n", length(matrix_slash_cols)))

non_matrix_slash <- setdiff(all_slash_cols, matrix_slash_cols)
cat(sprintf("Non-matrix '/' columns        : %d\n\n", length(non_matrix_slash)))

# Pre-compute NFC-normalized full column names for prefix-match detection.
# We match each column to its codebook stem using str_starts(col, stem + "/")
# instead of the old LAST-slash equality approach.  This correctly handles:
#   (a) option labels that contain "/" (e.g. "рибо/фоиздан", "уйга/ишга",
#       "Telegram/WhatsApp") — LAST-slash split produced a wrong "stem" and
#       orphaned these 10 real data columns;
#   (b) question stems that themselves contain "/" (e.g. Q3.11.1
#       "Кредит/қарз/насияни...") — prefix matching still finds all 9 options.
non_matrix_slash_nfc <- nfc(non_matrix_slash)

# Rename log accumulator
rename_log <- tibble(
  var_name    = character(),
  old_col     = character(),
  new_col     = character(),
  option_label = character(),
  option_code = character(),
  method      = character()
)

n_found_total <- 0L

for (i in seq_len(nrow(cb_ms))) {
  vn       <- cb_ms$var_name[i]
  stem     <- cb_ms$raw_col_name[i]
  opts_str <- cb_ms$response_options[i]

  if (is.na(stem) || is.na(opts_str)) next

  # Match: column NFC name must start with "<stem_nfc>/"
  # Using startsWith() (base R, fixed string) — NOT regex — to avoid
  # misinterpretation of "?", "(", ")" in Uzbek question-stem text.
  stem_nfc     <- nfc(stem)
  this_dummies <- non_matrix_slash[startsWith(non_matrix_slash_nfc, paste0(stem_nfc, "/"))]

  if (length(this_dummies) == 0) {
    cat(sprintf("  [NOT FOUND] %s\n    stem: '%s'\n",
                vn, substr(stem, 1, 80)))
    next
  }

  n_found_total <- n_found_total + length(this_dummies)
  opts          <- str_split(opts_str, fixed("|"))[[1]]
  option_labels <- str_extract(this_dummies, "[^/]+$")

  if (length(this_dummies) == length(opts)) {
    # Happy path: count matches → positional rename
    new_names <- paste0(vn, "__", opts)
    method    <- "positional"
  } else {
    # Count mismatch → slugify the Uzbek option label
    new_names <- paste0(vn, "__", slug(option_labels))
    method    <- "slug"
    cat(sprintf(
      "  [MISMATCH] %s: found %d dummies, codebook has %d options — slug fallback\n",
      vn, length(this_dummies), length(opts)
    ))
  }

  # Ensure uniqueness within this block (edge case)
  if (anyDuplicated(new_names)) {
    new_names <- make.unique(new_names, sep = "_")
  }

  # Convert to 0/1 integer
  df <- df %>%
    mutate(across(all_of(this_dummies), ~ vapply(.x, to01, integer(1))))

  # Rename columns
  rename_vec <- setNames(this_dummies, new_names)
  df <- df %>% rename(all_of(rename_vec))

  # Log
  n <- length(this_dummies)
  rename_log <- bind_rows(rename_log, tibble(
    var_name     = vn,
    old_col      = this_dummies,
    new_col      = new_names,
    option_label = option_labels,
    option_code  = if (method == "positional") opts else rep(NA_character_, n),
    method       = method
  ))

  cat(sprintf("  [OK] %-35s %d dummies  [%s]\n", vn, n, method))
}

cat(sprintf(
  "\nMulti-select expansion done:\n  %d vars processed | %d dummies renamed\n  positional: %d | slug: %d\n\n",
  nrow(rename_log %>% distinct(var_name)), nrow(rename_log),
  sum(rename_log$method == "positional"), sum(rename_log$method == "slug")
))

# ── 8. Matrix block processing ────────────────────────────────────────────────
cat("=== STEP 8: MATRIX BLOCK PROCESSING ===\n")

# Re-detect matrix slash cols in df (names may have changed after step 7)
current_slash <- names(df)[str_detect(names(df), fixed("/"))]
matrix_now    <- current_slash[str_detect(current_slash, matrix_stem_patterns)]

cat(sprintf("Matrix dummy columns to convert: %d\n", length(matrix_now)))

if (length(matrix_now) > 0) {
  # Separate by question block — each block uses a different response scale
  q21_cols <- matrix_now[str_detect(matrix_now, "^\\d+-2\\.1")]
  q23_cols <- matrix_now[str_detect(matrix_now, "^\\d+-2\\.3")]
  q33_cols <- matrix_now[str_detect(matrix_now, "^\\d+-3\\.3")]

  cat(sprintf("  Q2.1 credit_purpose_matrix : %d cols  [0/1 binary]\n",        length(q21_cols)))
  cat(sprintf("  Q2.3 credit_pref_matrix    : %d cols  [numeric midpoint, M UZS]\n", length(q23_cols)))
  cat(sprintf("  Q3.3 notify_matrix         : %d cols  [0/1 binary]\n",        length(q33_cols)))

  # ── Q2.1 + Q3.3 → binary 0L / 1L  ("Мақсадга мувофиқ" / "Мақсадга мувофиқ эмас")
  binary_cols <- c(q21_cols, q33_cols)
  if (length(binary_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(binary_cols), ~ vapply(.x, matrix_to01, integer(1))))
    cat("\n  Q2.1 / Q3.3 → 0/1 integer (1 = appropriate):\n")
    for (mc in binary_cols) {
      n1  <- sum(df[[mc]] == 1L, na.rm = TRUE)
      n0  <- sum(df[[mc]] == 0L, na.rm = TRUE)
      nna <- sum(is.na(df[[mc]]))
      label <- substr(str_extract(mc, "[^/]+$"), 1, 50)
      cat(sprintf("    %-52s  n1=%4d  n0=%4d  na=%d\n", label, n1, n0, nna))
    }
  }

  # ── Q2.3 → double (million UZS midpoint; 0 = would not borrow from source)
  if (length(q23_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(q23_cols), ~ vapply(.x, q23_to_mln, double(1))))
    cat("\n  Q2.3 → numeric midpoint (M UZS; 0 = would not borrow):\n")
    for (mc in q23_cols) {
      vals <- df[[mc]]
      n_pos <- sum(vals  > 0,  na.rm = TRUE)
      n_zero <- sum(vals == 0,  na.rm = TRUE)
      nna   <- sum(is.na(vals))
      mean_pos <- if (n_pos > 0) round(mean(vals[vals > 0], na.rm = TRUE), 1) else NA
      label <- substr(str_extract(mc, "[^/]+$"), 1, 50)
      cat(sprintf("    %-52s  willing=%4d  not=%4d  mean_if_willing=%.1f M\n",
                  label, n_pos, n_zero, mean_pos))
    }
  }
}
cat("\n")

# ── 9. Derived variables ───────────────────────────────────────────────────────
cat("=== STEP 9: DERIVED VARIABLES ===\n")

# 9.1  has_loan: 1 if has_loan_type is not the Uzbek "No" answer and not NA
#   has_loan_type raw Uzbek values:
#     "Йўқ"                                                            → 0 (not a borrower)
#     "Ҳа (ўзимнинг номимга расмийлаштириб, ўзим учун олинган)"      → 1 (own name, for self)
#     "Ҳа (менинг номимга расмийлаштирилган, лекин бошқа биров учун)" → 1 (name lent out)
#     "Ҳа (бошқа бировнинг номига расмийлаштириб, ўзим учун олганман)"→ 1 (straw borrower)
if ("has_loan_type" %in% names(df)) {
  df <- df %>%
    mutate(has_loan = as.integer(!is.na(has_loan_type) & has_loan_type != "Йўқ"))
  cat("  has_loan                    : 1 if has_loan_type != 'Йўқ'\n")
} else {
  df <- df %>% mutate(has_loan = NA_integer_)
  cat("  has_loan                    : has_loan_type missing — set to NA\n")
}

# 9.2  is_straw_borrower: borrowed under ANOTHER person's name for self
#   "Ҳа (бошқа бировнинг номига расмийлаштириб, ўзим учун олганман)"
if ("has_loan_type" %in% names(df)) {
  df <- df %>%
    mutate(is_straw_borrower =
             as.integer(!is.na(has_loan_type) &
                          str_detect(has_loan_type, fixed("бошқа бировнинг номига"))))
  cat("  is_straw_borrower           : borrowed under another's name for self\n")
}

# 9.3  is_borrowed_under_other_name: MY name used, but loan was for ANOTHER person
#   "Ҳа (менинг номимга расмийлаштирилган, лекин бошқа биров учун олинган)"
if ("has_loan_type" %in% names(df)) {
  df <- df %>%
    mutate(is_borrowed_under_other_name =
             as.integer(!is.na(has_loan_type) &
                          str_detect(has_loan_type, fixed("бошқа биров учун олинган"))))
  cat("  is_borrowed_under_other_name: name lent to another borrower\n")
}

# 9.4  is_npl / is_delayed (from admin holat1 — Uzbek labels)
#   "Қарздорликни ўз вақтида тўлаётган мижозлар"                          → on_time
#   "Қарздорликни 1-3 ой кечикиш билан тўлаётган мижозлар"               → delayed 1-3m
#   "Қарздорликни 3 ой ва ундан кўп муддат (NPL) тўламаётган мижозлар"   → NPL
if ("holat1" %in% names(df)) {
  df <- df %>%
    mutate(
      is_npl     = as.integer(!is.na(holat1) & str_detect(holat1, fixed("NPL"))),
      is_delayed = as.integer(!is.na(holat1) & (
        str_detect(holat1, fixed("NPL")) |
          str_detect(holat1, "1-3 ой кечикиш")
      ))
    )
  cat("  is_npl                      : holat1 contains 'NPL'\n")
  cat("  is_delayed                  : holat1 contains NPL or 1-3 month delay\n")
} else {
  cat("  is_npl / is_delayed         : holat1 not found — skipped\n")
}

# 9.5  dsr_midpoint (midpoints per CBU over-indebtedness classification)
#   Actual Uzbek values in dsr_cat:
#     "0% (тўлов қилмайман)"          → 0.000  (pays nothing / no debt service)
#     "10% дан – кам"                 → 0.050  (< 10%)
#     "10% дан – 20% гача"            → 0.150  (10–20%)
#     "20% дан – 35% гача"            → 0.275  (20–35%)
#     "35% дан – 50%гача"             → 0.425  (35–50%)
#     "50%дан кўп"                    → 0.625  (> 50%)
#     "Жавоб бериш қийин (ўқилмасин)" → NA
if ("dsr_cat" %in% names(df)) {
  df <- df %>%
    mutate(dsr_midpoint = case_when(
      dsr_cat == "0% (тўлов қилмайман)"         ~ 0.000,
      dsr_cat == "10% дан – кам"                ~ 0.050,
      dsr_cat == "10% дан – 20% гача"           ~ 0.150,
      dsr_cat == "20% дан – 35% гача"           ~ 0.275,
      dsr_cat == "35% дан – 50%гача"            ~ 0.425,
      dsr_cat == "50%дан кўп"                   ~ 0.625,
      TRUE                                       ~ NA_real_
    ))
  cat("  dsr_midpoint                : from dsr_cat midpoints (Uzbek labels)\n")
}

# 9.6  income_midpoint_mln_uzs (million UZS midpoints)
#   Actual Uzbek values in hh_income_cat:
#     "5 млн сўмгача"                                     → 2.5
#     "5-10 млн сўм"                                      → 7.5
#     "10 - 20 млн сўм"                                   → 15.0
#     "20-50 млн сўм"                                     → 35.0
#     "50 млн сўмдан юқори"                               → 75.0
#     "Жавоб беришга қийналаман (ўқиб берилмасин)"        → NA
if ("hh_income_cat" %in% names(df)) {
  df <- df %>%
    mutate(income_midpoint_mln_uzs = case_when(
      hh_income_cat == "5 млн сўмгача"        ~  2.5,
      hh_income_cat == "5-10 млн сўм"         ~  7.5,
      hh_income_cat == "10 - 20 млн сўм"      ~ 15.0,
      hh_income_cat == "20-50 млн сўм"        ~ 35.0,
      hh_income_cat == "50 млн сўмдан юқори"  ~ 75.0,
      TRUE                                     ~ NA_real_
    ))
  cat("  income_midpoint_mln_uzs     : from hh_income_cat midpoints (million UZS)\n")
}

# 9.7  financial_buffer_months
#   Actual Uzbek values in financial_buffer:
#     "умуман эга эмасман"               → 0.0  (no buffer at all)
#     "1 - 2 ой"                         → 1.5
#     "3 - 4 ой"                         → 3.5
#     "5 - 6 ой"                         → 5.5
#     "6 ойдан кўпроқ"                   → 8.0
#     "Жавоб бериш қийин (ўқилмасин)"    → NA
if ("financial_buffer" %in% names(df)) {
  df <- df %>%
    mutate(financial_buffer_months = case_when(
      financial_buffer == "умуман эга эмасман"              ~ 0.0,
      financial_buffer == "1 - 2 ой"                        ~ 1.5,
      financial_buffer == "3 - 4 ой"                        ~ 3.5,
      financial_buffer == "5 - 6 ой"                        ~ 5.5,
      financial_buffer == "6 ойдан кўпроқ"                  ~ 8.0,
      TRUE                                                   ~ NA_real_
    ))
  cat("  financial_buffer_months     : from financial_buffer midpoints\n")
}

# 9.8  multi_debt_flag: 1 if n_loans > 1
if ("n_loans" %in% names(df)) {
  df <- df %>%
    mutate(multi_debt_flag = as.integer(!is.na(n_loans) & n_loans > 1))
  cat("  multi_debt_flag             : 1 if n_loans > 1\n")
}

# 9.9  productive_use_flag (business | migration_work | family_entrepreneurship)
# After the prefix-matching fix in Step 7, loan_purpose gets all 14 options (count
# matches codebook → positional rename).  The three productive-use options are:
#   migration_work         (option 10) "Ишлаш учун четга кетиш харажатларини қоплаш"
#   business               (option 11) "Бизнесни бошлаш ёки кенгайтириш"
#   family_entrepreneurship(option 12) "Оилавий тадбиркорликни юритиш"
lp_prod_needed <- paste0("loan_purpose__", c("migration_work", "business", "family_entrepreneurship"))
lp_prod_found  <- intersect(lp_prod_needed, names(df))
if (length(lp_prod_found) > 0) {
  df <- df %>%
    mutate(productive_use_flag =
             as.integer(rowSums(across(all_of(lp_prod_found)), na.rm = TRUE) > 0))
  cat(sprintf("  productive_use_flag         : from [%s]\n",
              paste(lp_prod_found, collapse = ", ")))
} else {
  cat("  productive_use_flag         : loan_purpose__ dummies not found — skipped\n")
}

# 9.10  over_indebted_flag: dsr_midpoint >= 0.5 OR n_loans >= 3
# NA for non-borrowers; 0 if borrower with neither condition met
have_dsr  <- "dsr_midpoint" %in% names(df)
have_nlns <- "n_loans"      %in% names(df)

if (have_dsr || have_nlns) {
  df <- df %>%
    mutate(over_indebted_flag = case_when(
      has_loan == 0L ~ NA_integer_,
      (have_dsr  & !is.na(dsr_midpoint) & dsr_midpoint >= 0.5) |
        (have_nlns & !is.na(n_loans)    & n_loans >= 3)        ~ 1L,
      TRUE                                                      ~ 0L
    ))
  cat("  over_indebted_flag          : dsr_midpoint >= 0.5 OR n_loans >= 3\n")
}

# 9.11  delay_tolerance_score (0–4 ordinal; higher = more tolerance / moral hazard)
#   Actual Uzbek values in acceptable_delay_days:
#     "Ҳеч қандай кечикиш мумкин (нормал) эмас" → 0  (no delay acceptable)
#     "1-7 кун"                                  → 1
#     "8-30 кун"                                 → 2
#     "30 кундан ортиқ"                          → 3
#     "Тўламаса ҳам бўлади"                      → 4  (no need to repay at all)
if ("acceptable_delay_days" %in% names(df)) {
  df <- df %>%
    mutate(delay_tolerance_score = case_when(
      acceptable_delay_days == "Ҳеч қандай кечикиш мумкин (нормал) эмас" ~ 0L,
      acceptable_delay_days == "1-7 кун"                                  ~ 1L,
      acceptable_delay_days == "8-30 кун"                                 ~ 2L,
      acceptable_delay_days == "30 кундан ортиқ"                          ~ 3L,
      acceptable_delay_days == "Тўламаса ҳам бўлади"                      ~ 4L,
      TRUE                                                                 ~ NA_integer_
    ))
  cat("  delay_tolerance_score       : from acceptable_delay_days (0 = none, 4 = no_need)\n")
}

# 9.12  fin_lit_score: additive 0–4 from four financial-literacy components
#   contract_read:
#     "Ҳа, тўлиқ танишганман"                           = 1  (read fully)
#     "Ҳа, қисман танишганман"                         = 1  (read partially)
#     "Танишмаганман"                                  = 0
#   terms_understood:
#     "Ҳа"                                              = 1  (understood)
#     "Тушунганман деб ўйладим..." / "Тушунмаганман"    = 0
#   credit_score_motivates:
#     "Кредит рейтинги ҳақида маълумотга эга эмасман"   = 0  (unaware)
#     all other responses                                = 1
#   knows_delay_effect:
#     "Ҳа, аниқ биламан" / "Қисман биламан"            = 1
#     "Йўқ, билмайман" / "Жавоб бериш қийин"           = 0
fl_comp_cols <- c("fl_contract_read", "fl_terms_understood",
                  "fl_credit_score",  "fl_knows_delay")

if ("contract_read" %in% names(df)) {
  df <- df %>%
    mutate(fl_contract_read = as.integer(
      !is.na(contract_read) & contract_read %in% c("Ҳа, тўлиқ танишганман", "Ҳа, қисман танишганман")
    ))
}
if ("terms_understood" %in% names(df)) {
  df <- df %>%
    mutate(fl_terms_understood = as.integer(
      !is.na(terms_understood) & terms_understood == "Ҳа"
    ))
}
if ("credit_score_motivates" %in% names(df)) {
  df <- df %>%
    mutate(fl_credit_score = as.integer(
      !is.na(credit_score_motivates) &
        credit_score_motivates != "Кредит рейтинги ҳақида маълумотга эга эмасман"
    ))
}
if ("knows_delay_effect" %in% names(df)) {
  df <- df %>%
    mutate(fl_knows_delay = as.integer(
      !is.na(knows_delay_effect) &
        knows_delay_effect %in% c("Ҳа, аниқ биламан", "Қисман биламан")
    ))
}

fl_present <- intersect(fl_comp_cols, names(df))
if (length(fl_present) == 4) {
  df <- df %>%
    mutate(
      fin_lit_score = if_else(
        rowSums(is.na(across(all_of(fl_present)))) > 0L,
        NA_integer_,
        as.integer(rowSums(across(all_of(fl_present)), na.rm = TRUE))
      )
    ) %>%
    select(-all_of(fl_comp_cols))        # drop intermediate helper columns
  cat("  fin_lit_score               : 4-component additive (0–4)\n")
} else {
  # Drop any partial helper columns
  if (length(fl_present) > 0) df <- df %>% select(-any_of(fl_comp_cols))
  cat(sprintf("  fin_lit_score               : %d / 4 components found — skipped\n",
              length(fl_present)))
}

# 9.13  credit_formality: formal_bank | formal_other | informal | mixed
cs_fb  <- intersect("credit_source_actual__commercial_bank",     names(df))
cs_fo  <- intersect(c("credit_source_actual__formal_installment",
                       "credit_source_actual__mfi",
                       "credit_source_actual__pawnshop"),         names(df))
cs_inf <- intersect(c("credit_source_actual__street_lenders",
                       "credit_source_actual__family_friends"),   names(df))

if (length(cs_fb) > 0 || length(cs_fo) > 0 || length(cs_inf) > 0) {
  df <- df %>%
    mutate(
      .fb  = if (length(cs_fb)  > 0) rowSums(across(all_of(cs_fb)),  na.rm = TRUE) else 0L,
      .fo  = if (length(cs_fo)  > 0) rowSums(across(all_of(cs_fo)),  na.rm = TRUE) else 0L,
      .inf = if (length(cs_inf) > 0) rowSums(across(all_of(cs_inf)), na.rm = TRUE) else 0L,
      credit_formality = case_when(
        has_loan != 1L                                    ~ NA_character_,
        .fb > 0 & .fo == 0 & .inf == 0                   ~ "formal_bank",
        .fb == 0 & .fo  > 0 & .inf == 0                  ~ "formal_other",
        .fb == 0 & .fo == 0 & .inf  > 0                  ~ "informal",
        (.fb + .fo + .inf) > 0                            ~ "mixed",
        TRUE                                              ~ NA_character_
      )
    ) %>%
    select(-.fb, -.fo, -.inf)
  cat("  credit_formality            : formal_bank | formal_other | informal | mixed\n")
} else {
  cat("  credit_formality            : credit_source_actual__ dummies not found — skipped\n")
}

cat("\n")

# ── 10. Final column inventory + remaining "/" audit ───────────────────────────
cat("=== STEP 10: FINAL COLUMN INVENTORY ===\n")

n_numeric  <- sum(vapply(df, is.double,    logical(1)))
n_integer  <- sum(vapply(df, is.integer,   logical(1)))
n_char     <- sum(vapply(df, is.character, logical(1)))
n_logical  <- sum(vapply(df, is.logical,   logical(1)))

cat(sprintf("Rows            : %d\n",  nrow(df)))
cat(sprintf("Columns total   : %d\n",  ncol(df)))
cat(sprintf("  double        : %d\n",  n_numeric))
cat(sprintf("  integer       : %d\n",  n_integer))
cat(sprintf("  character     : %d\n",  n_char))
cat(sprintf("  logical       : %d\n",  n_logical))

# Audit unprocessed "/" columns
remaining_slash <- names(df)[str_detect(names(df), fixed("/"))]
if (length(remaining_slash) > 0) {
  cat(sprintf(
    "\nWARNING: %d unprocessed '/' columns remain after Steps 7–8.\n",
    length(remaining_slash)
  ))
  cat("These are either unmapped multi-select blocks or additional matrix columns.\n")
  for (v in head(remaining_slash, 30))
    cat(sprintf("  %s\n", substr(v, 1, 110)))
  if (length(remaining_slash) > 30)
    cat(sprintf("  ... and %d more\n", length(remaining_slash) - 30))
} else {
  cat("\nAll '/' columns processed. [OK]\n")
}

# Derived variable presence check
derived_expected <- c(
  "has_loan", "is_straw_borrower", "is_borrowed_under_other_name",
  "is_npl", "is_delayed", "dsr_midpoint", "income_midpoint_mln_uzs",
  "financial_buffer_months", "multi_debt_flag", "productive_use_flag",
  "over_indebted_flag", "delay_tolerance_score", "fin_lit_score",
  "credit_formality"
)
cat(sprintf("\nDerived variables: %d / %d present\n",
            sum(derived_expected %in% names(df)), length(derived_expected)))
for (v in derived_expected) {
  present <- v %in% names(df)
  n_non_na <- if (present) sum(!is.na(df[[v]])) else 0L
  cat(sprintf("  [%s] %-35s  non-NA: %d\n",
              if (present) "OK" else "MISS", v, n_non_na))
}

cat("\n")

# ── 11. Write outputs ──────────────────────────────────────────────────────────
saveRDS(df, out_rds)
cat(sprintf("✓ Saved: %s\n", out_rds))
cat(sprintf("  Dimensions: %d rows x %d cols\n", nrow(df), ncol(df)))

write_csv(rename_log, rename_log_csv)
cat(sprintf("✓ Multi-select rename log: %s  (%d rows)\n",
            rename_log_csv, nrow(rename_log)))

log_line <- sprintf(
  "%s  03_clean  rows=%d  cols=%d  ms_renames=%d  remaining_slash=%d",
  format(Sys.time()), nrow(df), ncol(df), nrow(rename_log),
  length(remaining_slash)
)
write(log_line, file = log_file, append = TRUE)
cat(sprintf("✓ Log appended: %s\n", log_file))

cat("\n=== 03_clean COMPLETE ===\n")
