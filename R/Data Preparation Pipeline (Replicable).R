# ============================================================
# Data Preparation Pipeline (Replicable)
# - Input: raw survey export (Excel)
# - Outputs:
#   * survey_master_cleaned_v1 (csv + utf8-bom csv + xlsx)
#   * borrowers_clean_v1 (csv + utf8-bom csv + xlsx)
#   * nonborrowers_clean_v1 (csv + utf8-bom csv + xlsx)
#   * logs: duplicates, column map, column profile, skip-logic, interviewer QC
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(janitor)
  library(openxlsx)
  library(stringi)
  library(glue)
})

# -----------------------------
# USER CONFIG
# -----------------------------
RAW_FILE <- "D:/old/Desktop/Cerr_old/Cerr/2025/58. Марказий банк тадқиқоти/replication/03.version/dataset17022026.xlsx"       # path to raw file
RAW_SHEET <- 1                           # sheet index or name
OUT_DIR <- "outputs_prep_v1"             # output folder
VERSION_TAG <- "v1"                      # for naming
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Core question text used to identify borrower flag (Q2.4)
Q24_MAIN <- "2.4. Бугунги кунда номингизда қарз, кредит ёки насия (рассрочка) мавжудми?"

# Borrower-only blocks to check skip logic (main columns only, no "/")
BORROWER_ONLY_PREFIXES <- c("2.4.1", "2.4.2", "2.4.3", "2.4.4", "2.5", "2.7", "2.8", "2.9")

# -----------------------------
# HELPERS
# -----------------------------

norm_text <- function(x) {
  # Trim + collapse multiple whitespace; preserve NA
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", " ")
  x[x %in% c("nan", "NaN", "None", "NULL", "")] <- NA_character_
  x
}

# Convert multi-select dummy value to strict 0/1 integer
to01 <- function(x) {
  if (is.na(x)) return(0L)
  if (is.logical(x)) return(ifelse(x, 1L, 0L))
  if (is.numeric(x)) return(ifelse(x != 0, 1L, 0L))
  
  s <- str_to_lower(str_trim(as.character(x)))
  if (s %in% c("1","true","ҳа","ha","yes","y")) return(1L)
  if (s %in% c("0","false","йўқ","yo'q","yo‘q","no","n")) return(0L)
  
  # Try numeric parse
  suppressWarnings({
    v <- as.numeric(s)
  })
  if (!is.na(v)) return(ifelse(v != 0, 1L, 0L))
  0L
}

# Safe ASCII-ish column name map (keeps uniqueness)
make_safe_name <- function(x) {
  # Replace spaces with underscores and remove non-alphanumeric (ascii)
  y <- str_trim(as.character(x))
  y <- str_replace_all(y, "\\s+", "_")
  y <- str_replace_all(y, "[^0-9A-Za-z_]+", "_")
  y <- str_replace_all(y, "_+", "_")
  y <- str_replace_all(y, "^_|_$", "")
  if (y == "") y <- NA_character_
  y
}

dedupe_names <- function(x) {
  # Ensure unique names by suffixing _2, _3,...
  out <- character(length(x))
  seen <- list()
  for (i in seq_along(x)) {
    nm <- x[i]
    if (is.na(nm) || nm == "") nm <- paste0("col_", i)
    if (!is.null(seen[[nm]])) {
      seen[[nm]] <- seen[[nm]] + 1
      nm2 <- paste0(nm, "_", seen[[nm]])
      out[i] <- nm2
    } else {
      seen[[nm]] <- 1
      out[i] <- nm
    }
  }
  out
}

write_csv_utf8_bom <- function(df, path) {
  # Excel-friendly UTF-8 BOM CSV
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  utils::write.csv(df, con, row.names = FALSE, na = "")
}

write_outputs_all_formats <- function(df, stem, out_dir = OUT_DIR) {
  csv_path <- file.path(out_dir, paste0(stem, ".csv"))
  bom_path <- file.path(out_dir, paste0(stem, "_UTF8_BOM.csv"))
  xlsx_path <- file.path(out_dir, paste0(stem, ".xlsx"))
  
  # Plain CSV (UTF-8)
  readr::write_csv(df, csv_path, na = "")
  
  # UTF-8 BOM CSV for Excel Cyrillic
  write_csv_utf8_bom(df, bom_path)
  
  # XLSX
  openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
  
  tibble::tibble(
    stem = stem,
    csv = csv_path,
    csv_utf8_bom = bom_path,
    xlsx = xlsx_path
  )
}

# -----------------------------
# STEP 0: LOAD RAW
# -----------------------------
message("Loading raw file: ", RAW_FILE)
raw <- readxl::read_excel(RAW_FILE, sheet = RAW_SHEET)

raw_rows <- nrow(raw)
raw_cols <- ncol(raw)

# -----------------------------
# CHECKPOINT 1: DROP EXACT DUPLICATES (FULL ROW)
# -----------------------------
dup_mask <- duplicated(raw)
dropped_dups <- raw %>% filter(dup_mask)
df <- raw %>% filter(!dup_mask)

message(glue("Raw shape: {raw_rows} x {raw_cols}"))
message(glue("Dropped exact duplicate rows: {nrow(dropped_dups)}"))
message(glue("Remaining rows: {nrow(df)}"))

# Add row_id (post-drop)
df <- df %>% mutate(row_id = row_number()) %>% relocate(row_id)

# -----------------------------
# CHECKPOINT 2: NORMALIZE KEY TEXT FIELDS (IF EXIST)
# -----------------------------
for (col in c("region", "district")) {
  if (col %in% names(df)) {
    df[[col]] <- norm_text(df[[col]])
  }
}

# -----------------------------
# CHECKPOINT 3: IDENTIFY BORROWER FLAG (has_loan) FROM Q2.4
# Logic:
# 1) If dummy columns exist for Q2.4: use them
# 2) Else parse the main text response for "Ҳа"/"Йўқ"
# -----------------------------
q24_dummies <- names(df)[str_starts(names(df), paste0(Q24_MAIN, "/"))]

parse_yes_no <- function(x) {
  if (is.na(x)) return(NA_integer_)
  s <- as.character(x)
  if (str_detect(s, "Йўқ|Yo‘q|Yo'q|Yoq")) return(0L)
  if (str_detect(s, "Ҳа|Ha")) return(1L)
  NA_integer_
}

if (Q24_MAIN %in% names(df) && length(q24_dummies) > 0) {
  # Ensure dummies are numeric 0/1
  df <- df %>%
    mutate(across(all_of(q24_dummies), ~ as.integer(as.numeric(.) %>% replace_na(0))))
  
  yes_cols <- q24_dummies[str_detect(q24_dummies, "/Ҳа")]
  no_cols  <- q24_dummies[str_detect(q24_dummies, "/Йўқ")]
  
  df <- df %>%
    mutate(
      has_loan = as.integer(rowSums(across(all_of(yes_cols))) > 0),
      has_any  = rowSums(across(all_of(q24_dummies))) > 0
    ) %>%
    mutate(
      has_loan = ifelse(!has_any, NA_integer_, has_loan),
      has_loan = ifelse(rowSums(across(all_of(no_cols))) > 0 &
                          rowSums(across(all_of(yes_cols))) == 0, 0L, has_loan)
    ) %>%
    select(-has_any)
  
} else if (Q24_MAIN %in% names(df)) {
  df <- df %>% mutate(has_loan = vapply(.data[[Q24_MAIN]], parse_yes_no, integer(1)))
} else {
  warning("Q2.4 main question not found; has_loan will be NA.")
  df <- df %>% mutate(has_loan = NA_integer_)
}

# Sanity check
message("has_loan distribution:")
print(table(df$has_loan, useNA = "ifany"))

# -----------------------------
# CHECKPOINT 4: MULTI-SELECT DUMMY NORMALIZATION ("/" columns)
# Convert ALL dummy columns containing "/" to 0/1
# -----------------------------
dummy_cols <- names(df)[str_detect(names(df), "/")]

if (length(dummy_cols) > 0) {
  df <- df %>%
    mutate(across(all_of(dummy_cols), ~ vapply(., to01, integer(1))))
}

# Dummy blocks (prefixes before "/")
dummy_prefixes <- unique(str_split_fixed(dummy_cols, "/", 2)[,1])

message(glue("Dummy columns: {length(dummy_cols)}"))
message(glue("Dummy blocks (prefixes): {length(dummy_prefixes)}"))

# -----------------------------
# CHECKPOINT 5: SAFE COLUMN NAME MAP (FOR CODING)
# We do NOT rename columns in the data by default,
# we just export the mapping: original -> safe
# -----------------------------
safe <- make_safe_name(names(df))
safe <- dedupe_names(safe)

col_name_map <- tibble::tibble(
  original = names(df),
  safe = safe
)

# -----------------------------
# CHECKPOINT 6: COLUMN PROFILE (MISSINGNESS + UNIQUE)
# -----------------------------
column_profile <- tibble::tibble(
  column = names(df),
  dtype = vapply(df, function(x) class(x)[1], character(1)),
  missing_pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
  n_unique = vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1))
) %>%
  left_join(col_name_map, by = c("column" = "original")) %>%
  arrange(desc(missing_pct))

# -----------------------------
# CHECKPOINT 7: SPLIT BORROWERS VS NON-BORROWERS
# -----------------------------
borrowers <- df %>% filter(has_loan == 1)
nonborrowers <- df %>% filter(has_loan == 0)

message(glue("Borrowers: {nrow(borrowers)}"))
message(glue("Non-borrowers: {nrow(nonborrowers)}"))
message(glue("has_loan missing: {sum(is.na(df$has_loan))}"))

# -----------------------------
# CHECKPOINT 8: SKIP-LOGIC CHECK (borrower-only variables in non-borrowers)
# We check ONLY main columns (no "/") whose name starts with any prefix
# -----------------------------
borrower_only_main <- names(df)[
  map_lgl(names(df), function(nm) {
    any(str_starts(nm, BORROWER_ONLY_PREFIXES)) && !str_detect(nm, "/")
  })
]

skip_logic_check <- tibble::tibble(
  column = borrower_only_main,
  filled_pct_borrowers = map_dbl(borrower_only_main, ~ mean(!is.na(borrowers[[.x]])) * 100),
  filled_pct_nonborrowers = map_dbl(borrower_only_main, ~ mean(!is.na(nonborrowers[[.x]])) * 100)
) %>%
  arrange(desc(filled_pct_nonborrowers)) %>%
  mutate(nonborrowers_filled_flag = filled_pct_nonborrowers > 0.5)

# -----------------------------
# CHECKPOINT 9: INTERVIEWER QC (if interviewer field exists)
# - n interviews
# - avg missing pct across all vars (rough QC signal)
# -----------------------------
interviewer_candidates <- names(df)[
  str_detect(str_to_lower(names(df)), "интервьюер|interviewer")
]

qc_by_interviewer <- tibble::tibble()
if (length(interviewer_candidates) > 0) {
  interviewer_col <- interviewer_candidates[1]
  qc_by_interviewer <- df %>%
    group_by(.data[[interviewer_col]]) %>%
    summarise(
      n_interviews = n(),
      avg_missing_pct_all_vars = mean(sapply(across(everything()), ~ mean(is.na(.x)))) * 100,
      .groups = "drop"
    ) %>%
    rename(interviewer = 1) %>%
    arrange(desc(n_interviews))
} else {
  message("No interviewer column found for QC table.")
}

# -----------------------------
# EXPORTS (ALL FORMATS)
# -----------------------------
# Note: requires readr for write_csv; install if missing.
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Package 'readr' is required. Install with: install.packages('readr')")
}

manifest <- list()

manifest <- append(manifest, list(write_outputs_all_formats(df, glue("survey_master_cleaned_{VERSION_TAG}"))))
manifest <- append(manifest, list(write_outputs_all_formats(borrowers, glue("borrowers_clean_{VERSION_TAG}"))))
manifest <- append(manifest, list(write_outputs_all_formats(nonborrowers, glue("nonborrowers_clean_{VERSION_TAG}"))))
manifest <- append(manifest, list(write_outputs_all_formats(dropped_dups, glue("dropped_full_duplicates"))))
manifest <- append(manifest, list(write_outputs_all_formats(col_name_map, glue("column_name_map_{VERSION_TAG}"))))
manifest <- append(manifest, list(write_outputs_all_formats(column_profile, glue("column_profile_{VERSION_TAG}"))))
manifest <- append(manifest, list(write_outputs_all_formats(skip_logic_check, glue("skip_logic_check_{VERSION_TAG}"))))

if (nrow(qc_by_interviewer) > 0) {
  manifest <- append(manifest, list(write_outputs_all_formats(qc_by_interviewer, glue("qc_by_interviewer_{VERSION_TAG}"))))
}

manifest_df <- bind_rows(manifest)
openxlsx::write.xlsx(manifest_df, file.path(OUT_DIR, glue("EXPORT_MANIFEST_{VERSION_TAG}.xlsx")), overwrite = TRUE)

# -----------------------------
# FINAL CHECKPOINT SUMMARY
# -----------------------------
checkpoint_summary <- tibble::tibble(
  metric = c(
    "raw_rows","raw_cols",
    "dropped_exact_duplicates",
    "rows_after_drop",
    "dummy_cols_count",
    "dummy_blocks_count",
    "borrowers_n",
    "nonborrowers_n",
    "has_loan_missing_n"
  ),
  value = c(
    raw_rows, raw_cols,
    nrow(dropped_dups),
    nrow(df),
    length(dummy_cols),
    length(dummy_prefixes),
    nrow(borrowers),
    nrow(nonborrowers),
    sum(is.na(df$has_loan))
  )
)

write_outputs_all_formats(checkpoint_summary, glue("checkpoint_summary_{VERSION_TAG}"))

message("✅ Data preparation completed.")
message(glue("Outputs written to: {OUT_DIR}"))