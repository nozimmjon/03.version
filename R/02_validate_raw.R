# =============================================================================
# 02_validate_raw.R  —  Raw data quality checks (pre-clean)
# =============================================================================
# Input:   data/intermediate/01_raw.rds       (2 131 x 245, all-text columns)
# Outputs: logs/02_validate_raw_report.html   (pointblank interactive report)
#          logs/02_validate_raw.log           (timestamp + pass/fail counts)
#
# Validation layers:
#   A. Coverage      -- all expected codebook var_names present in raw
#   B. Completeness  -- always-asked vars must not contain NA
#   C. Set-check     -- single_choice (non-NA) values within codebook set
#   D. Range-check   -- numeric vars bounded by valid_min / valid_max
#   E. Cross-column  -- hh_workers <= hh_size; children_under18 < hh_size
#   F. Gate          -- consent == 'yes'; has_loan_type routes block-2/3 vars
#   G. Skip-logic    -- employment_other, income_sources_other consistency
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

if (!requireNamespace("pointblank", quietly = TRUE)) {
  stop(
    "Package 'pointblank' not found. ",
    "Install with:  install.packages('pointblank')"
  )
}
library(pointblank)

# -- 0. Paths ------------------------------------------------------------------
raw_rds  <- here("data", "intermediate", "01_raw.rds")
cb_csv   <- here("codebook", "codebook.csv")
rpt_html <- here("logs", "02_validate_raw_report.html")
log_file <- here("logs", "02_validate_raw.log")

dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(raw_rds), file.exists(cb_csv))

# -- 1. Load ------------------------------------------------------------------
raw <- readRDS(raw_rds)
cb  <- read_csv(
  cb_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

cat(sprintf("Raw data  : %d rows x %d cols\n", nrow(raw), ncol(raw)))
cat(sprintf("Codebook  : %d rows\n\n", nrow(cb)))

# -- 2. Parse codebook for validation specs -----------------------------------
cb_work <- cb %>%
  filter(
    section != "derived",
    var_name %in% names(raw)
  )

# -- A. Coverage check --------------------------------------------------------
cat("=== A. COVERAGE ===\n")

# PII vars were separated to pii_table.rds in 01_ingest.R — not in raw
pii_vars <- c("resp_name_main", "resp_name_add", "phone_number", "date_of_birth")

cb_expected <- cb %>%
  filter(
    section != "derived",
    !is.na(raw_col_name),
    !str_starts(raw_col_name, "NOT IN RAW"),
    !str_starts(raw_col_name, "DERIVED"),
    !str_detect(raw_col_name, fixed("(matrix")),
    !var_name %in% pii_vars
  ) %>%
  pull(var_name)

cb_missing <- setdiff(cb_expected, names(raw))

cat(sprintf("Expected codebook vars : %d\n", length(cb_expected)))
cat(sprintf("Present in raw         : %d\n", length(cb_expected) - length(cb_missing)))

if (length(cb_missing) > 0) {
  cat("MISSING from raw:\n")
  for (v in cb_missing) cat("  x", v, "\n")
} else {
  cat("All expected vars present [OK]\n")
}
cat("\n")

# -- B. Always-asked vars (completeness targets) ------------------------------
always_vars <- cb_work %>%
  filter(str_trim(tolower(skip_condition)) == "always") %>%
  pull(var_name) %>%
  intersect(names(raw))

cat(sprintf("Always-asked vars found in raw: %d\n", length(always_vars)))
cat(paste0("  ", paste(always_vars, collapse = ", ")), "\n\n")

# -- C. Single-choice vars with parseable response_options --------------------
sc_df <- cb_work %>%
  filter(
    q_type %in% c("single_choice", "binary"),
    !is.na(response_options),
    substr(response_options, 1, 1) != "(",   # exclude (dynamic...), (list...), (matrix...)
    !str_starts(response_options, "rows:")   # exclude matrix row descriptions
  ) %>%
  select(var_name, skip_condition, response_options)

cat(sprintf("Single-choice/binary vars with parseable options: %d\n\n", nrow(sc_df)))

# -- D. Numeric vars with defined ranges -------------------------------------
num_df <- cb_work %>%
  filter(
    q_type == "numeric",
    !is.na(valid_min),
    !is.na(valid_max)
  ) %>%
  transmute(
    var_name,
    lo = as.numeric(valid_min),
    hi = as.numeric(valid_max)
  ) %>%
  filter(var_name %in% names(raw))

cat(sprintf("Numeric vars with range bounds: %d\n\n", nrow(num_df)))

# -- 3. Pre-type for range checks ---------------------------------------------
raw_typed <- raw %>%
  mutate(across(
    all_of(num_df$var_name),
    ~ suppressWarnings(as.numeric(.x))
  ))

# -- 4. Build pointblank agent ------------------------------------------------
agent <- create_agent(
  tbl   = raw_typed,
  label = paste0(
    "02_validate_raw | dataset17022026 | ",
    format(Sys.time(), "%Y-%m-%d %H:%M")
  )
)

# -- 4B. Completeness: always-asked vars must be non-NA -----------------------
for (v in always_vars) {
  agent <- agent %>%
    col_vals_not_null(
      columns = vars(!!sym(v)),
      label   = paste0("completeness: ", v)
    )
}

# -- 4C. Set membership: single_choice / binary vars -------------------------
for (i in seq_len(nrow(sc_df))) {
  v    <- sc_df$var_name[i]
  opts <- str_split(sc_df$response_options[i], fixed("|"))[[1]]

  agent <- agent %>%
    col_vals_in_set(
      columns = vars(!!sym(v)),
      set     = c(opts, NA_character_),
      label   = paste0("set [", paste(opts, collapse = "|"), "]: ", v)
    )
}

# -- 4D. Numeric ranges -------------------------------------------------------
for (i in seq_len(nrow(num_df))) {
  v  <- num_df$var_name[i]
  lo <- num_df$lo[i]
  hi <- num_df$hi[i]

  agent <- agent %>%
    col_vals_between(
      columns = vars(!!sym(v)),
      left    = lo,
      right   = hi,
      na_pass = TRUE,
      label   = paste0("range [", lo, "-", hi, "]: ", v)
    )
}

# -- 4E. Cross-column checks --------------------------------------------------
if (all(c("hh_workers", "hh_size") %in% names(raw_typed))) {
  agent <- agent %>%
    col_vals_expr(
      expr  = expr(is.na(hh_workers) | is.na(hh_size) | hh_workers <= hh_size),
      label = "cross_col: hh_workers <= hh_size"
    )
}

if (all(c("children_under18", "hh_size") %in% names(raw_typed))) {
  agent <- agent %>%
    col_vals_expr(
      expr  = expr(is.na(children_under18) | is.na(hh_size) | children_under18 < hh_size),
      label = "cross_col: children_under18 < hh_size"
    )
}

# -- 4F. Consent gate ---------------------------------------------------------
if ("consent" %in% names(raw_typed)) {
  agent <- agent %>%
    col_vals_in_set(
      columns = vars(consent),
      set     = c("yes"),
      label   = "gate: consent == 'yes' (no refusals in analytic dataset)"
    )
}

# -- 4G. Row-id uniqueness ----------------------------------------------------
agent <- agent %>%
  rows_distinct(
    columns = vars(row_id),
    label   = "uniqueness: row_id distinct"
  )

# -- 5. Interrogate -----------------------------------------------------------
agent <- agent %>% interrogate()

# -- 6. Export HTML report ----------------------------------------------------
tryCatch(
  {
    export_report(agent, filename = rpt_html)
    cat(sprintf("Pointblank HTML report saved: %s\n\n", rpt_html))
  },
  error = function(e) {
    cat("WARNING: Could not save HTML report:", conditionMessage(e), "\n\n")
  }
)

# -- 7. Manual gate-routing check (has_loan_type == 'no') ---------------------
cat("=== F. GATE ROUTING (has_loan_type) ===\n")

borrower_vars <- unique(c(
  "n_loans", "dsr_cat", "financial_buffer", "income_variability",
  "credit_source_actual", "borrow_motivation", "loan_purpose",
  "loan_channel", "loan_used_as_intended", "loan_decision_factor",
  "contract_read", "terms_understood", "bank_explained_terms",
  "credit_score_motivates", "knows_delay_effect", "loan_regret",
  "used_straw_borrower", "reminders_per_month", "acceptable_delay_days",
  "repayment_difficulty_reason", "collection_methods_received",
  "effective_reminder_method"
))
bv_present <- intersect(borrower_vars, names(raw))

if ("has_loan_type" %in% names(raw)) {
  no_loan <- raw %>% filter(has_loan_type == "no")
  cat(sprintf("Rows where has_loan_type == 'no': %d\n", nrow(no_loan)))

  gate_issues <- 0L
  for (v in bv_present) {
    nn     <- sum(!is.na(no_loan[[v]]))
    status <- if (nn == 0) "OK     " else { gate_issues <- gate_issues + 1L; "WARNING" }
    cat(sprintf("  [%s] %-42s -- %d unexpected non-NA\n", status, v, nn))
  }
  cat(sprintf("\nGate issues: %d / %d vars checked\n\n", gate_issues, length(bv_present)))
} else {
  cat("has_loan_type not found -- gate check skipped\n\n")
}

# -- 8. Manual consistency checks (dplyr) -------------------------------------
cat("=== G. SKIP-LOGIC CONSISTENCY ===\n")

# 8a. hh_workers <= hh_size
if (all(c("hh_workers", "hh_size") %in% names(raw))) {
  viol_hw <- raw %>%
    mutate(
      hs = suppressWarnings(as.numeric(hh_size)),
      hw = suppressWarnings(as.numeric(hh_workers))
    ) %>%
    filter(!is.na(hs), !is.na(hw), hw > hs)
  cat(sprintf("  hh_workers > hh_size violations        : %d rows\n", nrow(viol_hw)))
  if (nrow(viol_hw) > 0) {
    viol_hw %>%
      select(row_id, hh_size, hh_workers) %>%
      head(10) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
  }
}

# 8b. children_under18 should be < hh_size
if (all(c("children_under18", "hh_size") %in% names(raw))) {
  viol_ch <- raw %>%
    mutate(
      hs = suppressWarnings(as.numeric(hh_size)),
      ch = suppressWarnings(as.numeric(children_under18))
    ) %>%
    filter(!is.na(hs), !is.na(ch), ch >= hs)
  cat(sprintf("  children_under18 >= hh_size            : %d rows\n", nrow(viol_ch)))
}

# 8c. employment_other only when employment == 'other'
if (all(c("employment", "employment_other") %in% names(raw))) {
  viol_eo <- raw %>%
    filter(
      (is.na(employment) | employment != "other"),
      !is.na(employment_other)
    )
  cat(sprintf("  employment_other non-NA (emp != 'other'): %d rows\n", nrow(viol_eo)))
}

# 8d. income_sources_other: check if dummy col is accessible yet
if ("income_sources_other" %in% names(raw)) {
  cat("  income_sources_other present in raw [OK]\n")
}

# 8e. Age out-of-range
if ("age" %in% names(raw)) {
  age_vals  <- suppressWarnings(as.numeric(raw$age))
  n_age_na  <- sum(is.na(age_vals))
  n_age_bad <- sum(!is.na(age_vals) & (age_vals < 18 | age_vals > 80))
  cat(sprintf("  age out-of-range [18-80]               : %d rows\n",  n_age_bad))
  cat(sprintf("  age missing (NA)                       : %d rows\n",  n_age_na))
}

cat("\n")

# -- 9. Pointblank summary ----------------------------------------------------
val_set <- tryCatch(agent$validation_set, error = function(e) NULL)

cat("=== POINTBLANK SUMMARY ===\n")
if (!is.null(val_set)) {
  n_total <- nrow(val_set)
  n_pass  <- sum(val_set$all_passed, na.rm = TRUE)
  n_fail  <- n_total - n_pass

  cat(sprintf("Total checks : %d\n", n_total))
  cat(sprintf("Passed       : %d\n", n_pass))
  cat(sprintf("Failed       : %d\n", n_fail))

  if (n_fail > 0) {
    cat("\nFailed checks:\n")
    val_set %>%
      filter(!all_passed) %>%
      select(i, label, n_failed, f_failed) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
  } else {
    cat("All pointblank checks passed [OK]\n")
  }

  log_line <- sprintf(
    "%s  02_validate_raw  total=%d  pass=%d  fail=%d",
    format(Sys.time()), n_total, n_pass, n_fail
  )
  write(log_line, file = log_file, append = TRUE)
  cat(sprintf("\nLog appended  : %s\n", log_file))
} else {
  cat("Could not extract validation_set from agent object.\n")
}

cat("\n=== VALIDATE RAW COMPLETE ===\n")
