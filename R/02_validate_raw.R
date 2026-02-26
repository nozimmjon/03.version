#!/usr/bin/env Rscript

# ============================================================
# Phase 02: Raw-data validation (before cleaning)
# ------------------------------------------------------------
# PURPOSE
# - Validate raw survey data against the machine-readable codebook.
# - Produce QA artifacts (HTML + CSV logs) for field/data review.
# - Do NOT clean data here; this script only checks and reports.
#
# INPUTS (configurable via environment variables)
# - codebook/codebook.csv
# - data/intermediate/01_raw.rds (preferred, if available)
# - data/raw/dataset17022026.xlsx (fallback; read as text)
#
# OUTPUTS
# - outputs/validation_reports/02_validate_raw_pointblank.html
# - outputs/validation_reports/02_validate_raw_failures.csv
# - outputs/validation_reports/02_validate_raw_skip_failures.csv
# - outputs/validation_reports/codebook_column_presence.csv
# - outputs/validation_reports/missing_codebook_vars.csv
# - outputs/validation_reports/02_validate_raw_summary.csv
# - outputs/validation_reports/02_validate_raw.log
# ============================================================

suppressPackageStartupMessages({
  # Core data manipulation
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)

  # Validation engine
  library(pointblank)

  # Used to normalize codebook column names to snake_case
  library(janitor)

  # Used only when XLSX fallback is required
  library(readxl)
})

# ------------------------------------------------------------
# 1) Runtime configuration and file paths
# ------------------------------------------------------------
# We keep paths configurable so the script can run across machines/servers.
# If env vars are not set, defaults are used.
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
codebook_path <- Sys.getenv("CODEBOOK_PATH", unset = file.path("codebook", "codebook.csv"))
validation_dir <- Sys.getenv("VALIDATION_DIR", unset = file.path("outputs", "validation_reports"))
input_rds <- Sys.getenv("RAW_RDS_PATH", unset = file.path("data", "intermediate", "01_raw.rds"))
input_xlsx <- Sys.getenv("RAW_XLSX_PATH", unset = file.path("data", "raw", "dataset17022026.xlsx"))

# Ensure output folder exists before any export.
if (!dir.exists(validation_dir)) dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2) Simple run logger (human-readable execution trace)
# ------------------------------------------------------------
# We write a plain-text log so each run has an auditable trail.
log_file <- file.path(validation_dir, "02_validate_raw.log")
writeLines(c(
  paste0("Phase 02 raw validation started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Project root: ", project_root),
  ""
), con = log_file)

append_log <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n")
  write(txt, file = log_file, append = TRUE)
}

# ------------------------------------------------------------
# 3) Load and validate the codebook schema
# ------------------------------------------------------------
# This script is codebook-driven. If the codebook is missing or malformed,
# validation should stop immediately.
if (!file.exists(codebook_path)) {
  stop("Codebook not found at: ", codebook_path)
}

# Clean names to avoid casing/space issues (e.g., `valid_min` vs `Valid Min`).
cb <- readr::read_csv(codebook_path, show_col_types = FALSE) |> janitor::clean_names()

# Minimum required fields for this phase.
required_cb_cols <- c("var_name", "q_type", "valid_min", "valid_max", "skip_condition", "analytic_role")
missing_cb_cols <- setdiff(required_cb_cols, names(cb))
if (length(missing_cb_cols) > 0) {
  stop("Codebook missing required columns: ", paste(missing_cb_cols, collapse = ", "))
}

append_log("Loaded codebook rows: ", nrow(cb))

# ------------------------------------------------------------
# 4) Load raw input (RDS preferred, XLSX fallback)
# ------------------------------------------------------------
# WHY this order:
# - RDS is typically faster and already standardized from ingestion.
# - XLSX fallback keeps this script runnable independently when needed.
# - XLSX is read with col_types='text' to avoid silent type guessing.
read_raw <- function() {
  if (file.exists(input_rds)) {
    append_log("Loading raw input from RDS: ", input_rds)
    return(readRDS(input_rds))
  }
  if (file.exists(input_xlsx)) {
    append_log("Loading raw input from XLSX (all text): ", input_xlsx)
    return(readxl::read_excel(input_xlsx, sheet = 1, col_types = "text") |> as_tibble())
  }
  stop("No raw input found. Checked: ", input_rds, " and ", input_xlsx)
}

raw <- read_raw() |> as_tibble()

# Ensure row-level traceability in outputs. If ingestion has not created row_id,
# we create it here strictly for QA reporting.
if (!"row_id" %in% names(raw)) {
  raw <- raw |> mutate(row_id = row_number()) |> relocate(row_id)
  append_log("row_id was absent in input and has been added for validation tracking.")
}

append_log("Raw rows: ", nrow(raw), " | Raw columns: ", ncol(raw))

# ------------------------------------------------------------
# 5) Codebook-to-raw column presence audit
# ------------------------------------------------------------
# This is a direct reconciliation check: which expected codebook vars exist in raw?
expected_vars <- cb |> filter(!is.na(var_name), var_name != "") |> pull(var_name) |> unique()
present_vars <- intersect(expected_vars, names(raw))
missing_vars <- setdiff(expected_vars, names(raw))

mapping_df <- tibble(
  var_name = expected_vars,
  present_in_raw = var_name %in% names(raw)
)
readr::write_csv(mapping_df, file.path(validation_dir, "codebook_column_presence.csv"))

if (length(missing_vars) > 0) {
  append_log("Missing expected codebook vars in raw: ", length(missing_vars))
  readr::write_csv(tibble(var_name = missing_vars), file.path(validation_dir, "missing_codebook_vars.csv"))
} else {
  append_log("All expected codebook vars found in raw.")
}

# ------------------------------------------------------------
# 6) Build pointblank validation agent (core structural checks)
# ------------------------------------------------------------
# Checks included:
# - rows_distinct(): detect full-row duplicates
# - col_exists(expected_vars): ensure all expected columns are present
# - region non-null (if region exists)
agent <- create_agent(tbl = raw, label = "raw_validation") |>
  rows_distinct() |>
  col_exists(columns = expected_vars)

if ("region" %in% names(raw)) {
  agent <- agent |> col_vals_not_null(columns = "region")
}

# ------------------------------------------------------------
# 7) Add codebook-derived numeric range checks
# ------------------------------------------------------------
# For variables marked numeric/integer/binary/ordinal OR with min/max limits,
# enforce bounds using codebook metadata.
#
# IMPORTANT: this is validation only. We coerce temporary copies in-memory for
# checking, but we do not overwrite source files here.
cb_numeric <- cb |>
  filter(
    q_type %in% c("numeric", "integer", "binary", "ordinal") |
      !is.na(valid_min) |
      !is.na(valid_max)
  )

for (i in seq_len(nrow(cb_numeric))) {
  v <- cb_numeric$var_name[[i]]
  if (!v %in% names(raw)) next

  min_v <- suppressWarnings(as.numeric(cb_numeric$valid_min[[i]]))
  max_v <- suppressWarnings(as.numeric(cb_numeric$valid_max[[i]]))

  # If both bounds are missing/non-numeric, skip this variable.
  if (is.na(min_v) && is.na(max_v)) next

  # Convert raw column to numeric for checking only.
  if (!is.numeric(raw[[v]])) {
    raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
  }

  # Add the appropriate check based on which bounds exist.
  if (!is.na(min_v) && !is.na(max_v)) {
    agent <- agent |> col_vals_between(columns = all_of(v), left = min_v, right = max_v, na_pass = TRUE)
  } else if (!is.na(min_v)) {
    agent <- agent |> col_vals_gte(columns = all_of(v), value = min_v, na_pass = TRUE)
  } else {
    agent <- agent |> col_vals_lte(columns = all_of(v), value = max_v, na_pass = TRUE)
  }
}

# Execute all pointblank checks.
agent <- interrogate(agent)

# ------------------------------------------------------------
# 8) Export pointblank report and failing rows
# ------------------------------------------------------------
# HTML report is useful for reviewers; CSV failure export is machine-readable.
pb_report_path <- file.path(validation_dir, "02_validate_raw_pointblank.html")
export_report(agent, filename = pb_report_path)
append_log("Pointblank report written: ", pb_report_path)

all_fails <- get_sundered_data(agent = agent, type = "fail")
if (nrow(all_fails) > 0) {
  fails_path <- file.path(validation_dir, "02_validate_raw_failures.csv")
  readr::write_csv(all_fails, fails_path)
  append_log("Validation failures exported: ", fails_path, " (rows: ", nrow(all_fails), ")")
} else {
  append_log("No failing rows from pointblank checks.")
}

# ------------------------------------------------------------
# 9) Explicit skip-logic validation from codebook skip_condition
# ------------------------------------------------------------
# For each variable with a skip condition (e.g., has_loan == 1):
# - evaluate condition on each row
# - identify rows where condition is FALSE but variable has a non-missing value
# - export violations for review (do not modify data here)
skip_spec <- cb |>
  filter(!is.na(skip_condition), skip_condition != "", skip_condition != "—") |>
  select(var_name, skip_condition)

skip_fail_log <- tibble()
if (nrow(skip_spec) > 0) {
  for (i in seq_len(nrow(skip_spec))) {
    v <- skip_spec$var_name[[i]]
    cond <- skip_spec$skip_condition[[i]]
    if (!v %in% names(raw)) next

    # Parse/evaluate the condition in raw-data context.
    # If parsing fails, log and continue (no hard stop).
    condition_true <- tryCatch(
      eval(parse(text = cond), envir = raw),
      error = function(e) {
        append_log("Skip rule parse error for ", v, ": ", cond, " | ", conditionMessage(e))
        rep(NA, nrow(raw))
      }
    )

    # Normalize to logical vector if needed.
    if (!is.logical(condition_true)) {
      condition_true <- as.logical(condition_true)
    }

    # Violation definition:
    # condition is FALSE + field is non-empty/non-NA
    violating_rows <- which(!is.na(condition_true) & !condition_true & !is.na(raw[[v]]) & raw[[v]] != "")

    if (length(violating_rows) > 0) {
      skip_fail_log <- bind_rows(
        skip_fail_log,
        tibble(
          var_name = v,
          skip_condition = cond,
          row_id = raw$row_id[violating_rows],
          offending_value = as.character(raw[[v]][violating_rows])
        )
      )
    }
  }
}

if (nrow(skip_fail_log) > 0) {
  skip_path <- file.path(validation_dir, "02_validate_raw_skip_failures.csv")
  readr::write_csv(skip_fail_log, skip_path)
  append_log("Skip-condition violations exported: ", skip_path, " (rows: ", nrow(skip_fail_log), ")")
} else {
  append_log("No skip-condition violations detected.")
}

# ------------------------------------------------------------
# 10) Write compact run summary
# ------------------------------------------------------------
# This gives a quick status line for pipeline monitoring.
summary_tbl <- tibble(
  metric = c(
    "n_rows_raw", "n_cols_raw", "n_codebook_vars", "n_codebook_vars_present",
    "n_codebook_vars_missing", "n_pointblank_fail_rows", "n_skip_fail_rows"
  ),
  value = c(
    nrow(raw), ncol(raw), length(expected_vars), length(present_vars), length(missing_vars),
    nrow(all_fails), nrow(skip_fail_log)
  )
)

summary_path <- file.path(validation_dir, "02_validate_raw_summary.csv")
readr::write_csv(summary_tbl, summary_path)
append_log("Validation summary written: ", summary_path)
append_log("Phase 02 raw validation complete.")
