# ============================================================
# Data Preparation Pipeline v2 (Questionnaire-aligned)
# Replicable end-to-end from RAW Excel export
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(janitor)
  library(openxlsx)
  library(glue)
  library(lubridate)
})

# -----------------------------
# USER CONFIG
# -----------------------------
RAW_FILE  <- Sys.getenv("RAW_FILE", unset = "dataset17022026.xlsx")
RAW_SHEET <- 1
OUT_DIR   <- Sys.getenv("OUT_DIR", unset = "outputs_prep_v2")
VERSION   <- "v2"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(RAW_FILE)) {
  legacy_raw <- "D:/old/Desktop/Cerr_old/Cerr/2025/58. Марказий банк тадқиқоти/replication/03.version/dataset17022026.xlsx"
  if (file.exists(legacy_raw)) {
    RAW_FILE <- legacy_raw
  } else {
    stop("Raw file not found. Set env var RAW_FILE or place dataset17022026.xlsx in the project root.")
  }
}

# Questionnaire anchor strings (edit only if questionnaire wording changes)
Q24_MAIN <- "2.4. Бугунги кунда номингизда қарз, кредит ёки насия (рассрочка) мавжудми?"

# Matrix blocks to parse (prefixes before "/")
# 2.1 is a matrix: purposes × (Appropriate/Not appropriate)
# 3.3 is a matrix: who to notify × (Yes/No) style in export
MATRIX_PREFIXES <- c(
  "2.1.",    # adjust if your export uses exact "2.1." prefix
  "3.3"      # notify matrix often appears as "3-3.3 ..." with numeric prefixes; we handle heuristically too
)

# Cardinality rules from questionnaire
# - 2.2 choose up to 3
# - 2.1 "appropriate" choose up to 3 (interpretation: up to 3 purposes marked appropriate)
# - 3.10 choose 2
CARDINALITY_RULES <- tibble::tribble(
  ~block_hint,   ~max_select, ~rule_name,
  "2.2.",        3L,          "Q2.2 max 3 selections",
  "2.1.",        3L,          "Q2.1 max 3 'appropriate' purposes",
  "3.10.",       2L,          "Q3.10 max 2 selections"
)

# Borrower-only prefixes (should be NA for non-borrowers after the 2.4 gate)
BORROWER_ONLY_PREFIXES <- c(
  "2.4.1", "2.4.2", "2.4.3", "2.4.4",
  "2.5", "2.6", "2.7", "2.8", "2.9", "2.10",
  "3.4", "3.5", "3.6", "3.7", "3.8", "3.9",
  "3.10", "3.11", "3.12", "3.13", "3.14", "3.15", "3.16"
)

# Numeric constraints (edit as needed)
AGE_COL_PREFERRED <- "1.1. Ёшингиз:"
CHILDREN_COL      <- "1.8. 18 ёшга етмаган фарзандларингиз сони?"
HH_SIZE_COL       <- "1.6 Уй хўжалигингиз жами аъзолари сони нечта?"
HH_WORKERS_COL    <- "1.7. Уй хўжалигингизда неча киши даромадли меҳнат (иш) билан банд?"

# -----------------------------
# HELPERS
# -----------------------------

norm_text <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", " ")
  x[x %in% c("nan", "NaN", "None", "NULL", "")] <- NA_character_
  x
}

to01 <- function(x) {
  if (is.na(x)) return(0L)
  if (is.logical(x)) return(ifelse(x, 1L, 0L))
  if (is.numeric(x)) return(ifelse(x != 0, 1L, 0L))
  s <- str_to_lower(str_trim(as.character(x)))
  if (s %in% c("1","true","ҳа","ha","yes","y")) return(1L)
  if (s %in% c("0","false","йўқ","yo'q","yo‘q","no","n")) return(0L)
  suppressWarnings(v <- as.numeric(s))
  if (!is.na(v)) return(ifelse(v != 0, 1L, 0L))
  0L
}

is_binary_like_column <- function(x) {
  if (is.logical(x)) return(TRUE)

  if (is.numeric(x)) {
    vals <- unique(x[!is.na(x)])
    return(length(vals) == 0 || all(vals %in% c(0, 1)))
  }

  s <- str_to_lower(str_trim(as.character(x)))
  s[s %in% c("", "nan", "none", "null")] <- NA_character_
  vals <- unique(s[!is.na(s)])

  if (length(vals) == 0) return(TRUE)

  allowed <- c("1", "0", "true", "false", "ҳа", "ha", "yes", "y", "йўқ", "yo'q", "yo‘q", "yoq", "no", "n")
  if (all(vals %in% allowed)) return(TRUE)

  suppressWarnings(num_vals <- as.numeric(vals))
  all(!is.na(num_vals) & num_vals %in% c(0, 1))
}

write_csv_utf8_bom <- function(df, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  utils::write.csv(df, con, row.names = FALSE, na = "")
}

write_outputs <- function(df, stem, out_dir = OUT_DIR) {
  csv_path <- file.path(out_dir, paste0(stem, ".csv"))
  bom_path <- file.path(out_dir, paste0(stem, "_UTF8_BOM.csv"))
  xlsx_path <- file.path(out_dir, paste0(stem, ".xlsx"))
  
  # Plain CSV (UTF-8)
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Install with: install.packages('readr')")
  }
  readr::write_csv(df, csv_path, na = "")
  
  # BOM CSV
  write_csv_utf8_bom(df, bom_path)
  
  # XLSX
  openxlsx::write.xlsx(df, xlsx_path, overwrite = TRUE)
  
  tibble::tibble(stem = stem, csv = csv_path, csv_utf8_bom = bom_path, xlsx = xlsx_path)
}

# Detect dummy blocks by "/" separator
get_dummy_blocks <- function(nms) {
  # Checkbox dummies are exported as <question stem>/<option label>.
  # BUT some non-dummy questions also contain "/" inside wording
  # (e.g., "кредит/қарз/насия"), so we combine two signals:
  #   1) explicit ?/option pattern, OR
  #   2) multiple columns sharing the same stem before the final slash.
  has_slash <- str_detect(nms, "/")
  stem <- str_replace(nms, "/[^/]*$", "")
  stem_n <- as.integer(table(stem)[stem])

  is_question_option <- str_detect(nms, "\\?/\\s*[^?]+$")
  is_repeated_stem <- has_slash & stem_n > 1L

  is_dummy <- has_slash & (is_question_option | is_repeated_stem)
  dummy_cols <- nms[is_dummy]
  blocks <- unique(str_replace(dummy_cols, "/[^/]*$", ""))
  list(dummy_cols = dummy_cols, blocks = blocks)
}

# Basic "registry" classification
build_registry <- function(df) {
  nms <- names(df)
  dummy_info <- get_dummy_blocks(nms)
  dummy_cols <- dummy_info$dummy_cols
  dummy_blocks <- dummy_info$blocks
  
  # Open-text heuristics: character with high cardinality
  open_text <- nms[
    vapply(df, function(x) is.character(x) && dplyr::n_distinct(x, na.rm = TRUE) > 200, logical(1))
  ]
  
  # Numeric-entry: numeric but not dummy and not an ID-like column
  numeric_vars <- nms[vapply(df, is.numeric, logical(1))]
  
  # Matrix blocks: dummy blocks whose suffix looks like a value category (e.g., "Мақсадга мувофиқ", "Мақсадга мувофиқ эмас")
  # We will still parse any matrix-like blocks by detecting paired categories per item.
  tibble::tibble(
    column = nms,
    type = case_when(
      column %in% dummy_cols ~ "dummy",
      column %in% open_text ~ "open_text",
      column %in% numeric_vars ~ "numeric",
      TRUE ~ "single_or_text"
    )
  ) %>%
    mutate(
      dummy_block = ifelse(type == "dummy", str_replace(column, "/[^/]*$", ""), NA_character_),
      dummy_option = ifelse(type == "dummy", str_extract(column, "[^/]*$"), NA_character_)
    )
}

# Identify likely matrix blocks among dummy blocks
# A matrix block often has many items and each item has 2+ response-category columns.
# Here we flag blocks where dummy_option contains words like "мувофиқ" or "эмас" or similar.
flag_matrix_blocks <- function(reg) {
  reg %>%
    filter(type == "dummy") %>%
    group_by(dummy_block) %>%
    summarise(
      n_cols = n(),
      any_matrix_words = any(str_detect(str_to_lower(dummy_option), "мувофиқ|эмас|approp|not")),
      .groups = "drop"
    ) %>%
    arrange(desc(n_cols))
}

# Enforce max selections per block (generic)
calc_block_selections <- function(df, block_prefix) {
  cols <- names(df)[str_starts(names(df), paste0(block_prefix)) & str_detect(names(df), "/")]
  if (length(cols) == 0) return(tibble::tibble(block = block_prefix, row_id = integer(0), n_selected = integer(0)))
  tibble::tibble(
    block = block_prefix,
    row_id = df$row_id,
    n_selected = rowSums(df[, cols, drop = FALSE], na.rm = TRUE)
  )
}

# -----------------------------
# STEP 0: LOAD RAW
# -----------------------------
message("Loading raw file: ", RAW_FILE)
raw <- readxl::read_excel(RAW_FILE, sheet = RAW_SHEET)

raw_rows <- nrow(raw); raw_cols <- ncol(raw)

# -----------------------------
# STEP 1: DROP EXACT DUPLICATES (FULL ROW)
# -----------------------------
dup_mask <- duplicated(raw)
dropped_dups <- raw %>% filter(dup_mask)
df <- raw %>% filter(!dup_mask)

df <- df %>% mutate(row_id = row_number()) %>% relocate(row_id)

# -----------------------------
# STEP 2: CLEAN BASIC TEXT FIELDS (IF EXIST)
# -----------------------------
for (col in c("region", "district")) {
  if (col %in% names(df)) df[[col]] <- norm_text(df[[col]])
}
# Common system-ish fields: interviewer, observation type, etc.
sys_like <- names(df)[str_detect(str_to_lower(names(df)), "интервьюер|kuzatuv|kuzat|рўйхат|royxat|resp")]
for (col in sys_like) {
  if (col %in% names(df) && is.character(df[[col]])) df[[col]] <- norm_text(df[[col]])
}

# -----------------------------
# STEP 3: DROP FULLY EMPTY COLUMNS
# -----------------------------
missing_pct <- vapply(df, function(x) mean(is.na(x)), numeric(1))
fully_empty <- names(df)[missing_pct >= 1]
if (length(fully_empty) > 0) {
  message(glue("Dropping fully empty columns: {length(fully_empty)}"))
  df <- df %>% select(-all_of(fully_empty))
}


fully_empty_log <- tibble::tibble(
  column = fully_empty,
  missing_pct = missing_pct[fully_empty]
)
# -----------------------------
# -----------------------------
# STEP 4: NORMALIZE CHECKBOX DUMMIES ("/") TO 0/1
# BUT EXCLUDE MATRIX COLUMNS (2.1, 3.3) FROM THIS STEP
# -----------------------------
dummy_info <- get_dummy_blocks(names(df))
dummy_cols <- dummy_info$dummy_cols

# 4.1 Identify matrix columns (adjust patterns if your export uses different naming)
matrix_cols <- names(df)[
  str_detect(names(df), "^\\d+-2\\.1\\.") |   # e.g., "1-2.1 ... /..."
    str_detect(names(df), "^\\d+-3\\.3")      # e.g., "1-3.3 ... /..."
]
matrix_cols <- matrix_cols[str_detect(matrix_cols, "/")]  # only the slash-encoded ones

# 4.2 Convert only non-matrix dummy cols to 0/1
# IMPORTANT: only convert columns that are actually binary-like in raw values.
# This protects matrix-like amount questions (e.g., Q2.3) that use "/" in headers
# but contain categorical values ("5-20 млн", "0 (олмайман)", etc.).
dummy_cols_nonmatrix <- setdiff(dummy_cols, matrix_cols)
dummy_cols_binary <- dummy_cols_nonmatrix[
  vapply(df[dummy_cols_nonmatrix], is_binary_like_column, logical(1))
]
dummy_cols_nonbinary <- setdiff(dummy_cols_nonmatrix, dummy_cols_binary)

if (length(dummy_cols_binary) > 0) {
  df <- df %>%
    mutate(across(all_of(dummy_cols_binary), ~ vapply(., to01, integer(1))))
}

if (length(dummy_cols_nonbinary) > 0) {
  message("ℹ Preserving non-binary slash columns without 0/1 conversion: ", length(dummy_cols_nonbinary))
  print(dummy_cols_nonbinary)
}

# ====== [CHANGED / ADDED] STEP 4.3 QC: detect any remaining non-numeric slash columns ======
non_numeric_slash <- dummy_cols_binary[!vapply(df[dummy_cols_binary], is.numeric, logical(1))]

if (length(non_numeric_slash) > 0) {
  message("⚠ Non-numeric slash columns AFTER dummy normalization (these can break rowSums/cardinality checks):")
  print(non_numeric_slash)
  
  # Optional: quick peek at first non-NA values for debugging
  peek <- lapply(non_numeric_slash, function(cn) {
    vals <- df[[cn]]
    vals <- vals[!is.na(vals)]
    head(unique(vals), 5)
  })
  names(peek) <- non_numeric_slash
  print(peek)
} else {
  message("✅ All non-matrix slash columns are numeric after normalization.")
}
# -----------------------------
# STEP 5: BUILD BORROWER FLAG FROM Q2.4
# Prefer dummy approach; fallback to parsing main text if needed
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
  yes_cols <- q24_dummies[str_detect(q24_dummies, "/Ҳа")]
  no_cols  <- q24_dummies[str_detect(q24_dummies, "/Йўқ")]
  
  df <- df %>%
    mutate(
      has_loan = as.integer(rowSums(across(all_of(yes_cols))) > 0),
      q24_sum  = rowSums(across(all_of(q24_dummies))),
      has_loan = ifelse(q24_sum == 0, NA_integer_, has_loan),
      has_loan = ifelse(rowSums(across(all_of(no_cols))) > 0 & rowSums(across(all_of(yes_cols))) == 0, 0L, has_loan)
    )
  
  # Log invalid multi-selections (should be 1 usually)
  q24_invalid <- df %>% filter(!is.na(q24_sum) & q24_sum > 1) %>% select(row_id, q24_sum)
} else if (Q24_MAIN %in% names(df)) {
  df <- df %>% mutate(has_loan = vapply(.data[[Q24_MAIN]], parse_yes_no, integer(1)))
  q24_invalid <- tibble::tibble()
} else {
  warning("Q2.4 not found; has_loan set to NA.")
  df <- df %>% mutate(has_loan = NA_integer_)
  q24_invalid <- tibble::tibble()
}

message("has_loan distribution:")
print(table(df$has_loan, useNA = "ifany"))

# -----------------------------
# STEP 6: QUESTION REGISTRY + MATRIX BLOCK FLAGS
# -----------------------------
registry <- build_registry(df)
matrix_block_flags <- flag_matrix_blocks(registry)

# -----------------------------
# STEP 7: MATRIX PARSING (LIGHTWEIGHT, SAFE)
# Goal: create tidy “matrix_long” for audit + keep wide dummies as-is
# We'll parse any dummy block that looks like a matrix:
#   - option labels contain "мувофиқ"/"эмас" (or similar)
#   - and block has many columns
# -----------------------------
matrix_blocks <- matrix_block_flags %>%
  filter(any_matrix_words, n_cols >= 6) %>%   # heuristic threshold
  pull(dummy_block)

# Create a long audit table for matrix blocks (useful for QA)
matrix_long <- tibble::tibble()
if (length(matrix_blocks) > 0) {
  for (b in matrix_blocks) {
    cols <- registry %>% filter(dummy_block == b) %>% pull(column)
    # In wide exports: column name = "item/response_category"
    # Here dummy_block already equals "item" (prefix before "/"), so we treat "dummy_option" as category
    # But matrix blocks often come as multiple items each with its own prefix; your export may encode as 1-2.1, 2-2.1 etc.
    # So we also parse the "numeric prefix - question" structure:
    # example: "1-2.1 ... /SomeCategory"
    tmp <- df %>%
      select(row_id, all_of(cols)) %>%
      pivot_longer(-row_id, names_to = "col", values_to = "val") %>%
      left_join(registry %>% select(column, dummy_block, dummy_option), by = c("col" = "column")) %>%
      mutate(block = dummy_block, category = dummy_option) %>%
      select(row_id, block, category, col, val)
    matrix_long <- bind_rows(matrix_long, tmp)
  }
}

# -----------------------------
# STEP 8: CARDINALITY CHECKS (AND LOG VIOLATIONS)
# -----------------------------
cardinality_violations <- tibble::tibble()

# Generic blocks: 2.2 and 3.10 are plain multi-select (sum dummies)
for (i in seq_len(nrow(CARDINALITY_RULES))) {
  hint <- CARDINALITY_RULES$block_hint[i]
  maxn <- CARDINALITY_RULES$max_select[i]
  rule <- CARDINALITY_RULES$rule_name[i]
  
  cols <- names(df)[str_starts(names(df), hint) & str_detect(names(df), "/")]
  if (length(cols) == 0) next
  
  # ====== [CHANGED / ADDED] Keep only numeric columns for summation ======
  cols_num <- cols[vapply(df[cols], is.numeric, logical(1))]
  if (length(cols_num) == 0) next
  
  tmp <- df %>%
    transmute(
      row_id,
      rule_name = rule,
      block_hint = hint,
      n_selected = rowSums(across(all_of(cols_num)), na.rm = TRUE)
    ) %>%
    filter(!is.na(n_selected) & n_selected > maxn)
  
  cardinality_violations <- bind_rows(cardinality_violations, tmp)
}

# -----------------------------
# Q2.1 special: “appropriate” side max=3
# We approximate by selecting Q2.1 columns containing "мувофиқ" and excluding "эмас"
# -----------------------------
q21_cols <- names(df)[str_detect(names(df), "2\\.1\\.") & str_detect(names(df), "/")]

if (length(q21_cols) > 0) {
  
  q21_appropriate <- q21_cols[
    str_detect(str_to_lower(q21_cols), "мувофиқ") &
      !str_detect(str_to_lower(q21_cols), "эмас")
  ]
  
  if (length(q21_appropriate) > 0) {
    
    # ====== [CHANGED] keep only numeric columns to avoid rowSums crash ======
    q21_app_num <- q21_appropriate[vapply(df[q21_appropriate], is.numeric, logical(1))]
    
    # If nothing numeric, skip safely (but log it)
    if (length(q21_app_num) == 0) {
      message("⚠ Q2.1 appropriate columns found, but none are numeric after Step 4. Skipping Q2.1 cardinality check.")
    } else {
      
      tmp <- df %>%
        transmute(
          row_id,
          rule_name = "Q2.1 max 3 'appropriate' purposes",
          block_hint = "2.1.",
          n_selected = rowSums(across(all_of(q21_app_num)), na.rm = TRUE)
        ) %>%
        filter(n_selected > 3)
      
      cardinality_violations <- bind_rows(cardinality_violations, tmp)
    }
  }
}

# -----------------------------
# STEP 9: VALUE CONSTRAINTS (FIX + LOG)
# -----------------------------
constraint_fixes <- tibble::tibble(column = character(), row_id = integer(), old = character(), new = character(), rule = character())

fix_numeric_min0 <- function(df, col, rule_label) {
  if (!col %in% names(df)) return(list(df = df, log = tibble::tibble()))
  x <- df[[col]]
  if (!is.numeric(x)) return(list(df = df, log = tibble::tibble()))
  bad <- which(!is.na(x) & x < 0)
  if (length(bad) == 0) return(list(df = df, log = tibble::tibble()))
  log <- tibble::tibble(
    column = col, row_id = df$row_id[bad],
    old = as.character(x[bad]),
    new = NA_character_,
    rule = rule_label
  )
  df[[col]][bad] <- NA_real_
  list(df = df, log = log)
}

res <- fix_numeric_min0(df, CHILDREN_COL, "Children count must be >= 0")
df <- res$df; constraint_fixes <- bind_rows(constraint_fixes, res$log)

res <- fix_numeric_min0(df, HH_WORKERS_COL, "HH workers must be >= 0")
df <- res$df; constraint_fixes <- bind_rows(constraint_fixes, res$log)

# Household size must be >= 1 (if present)
if (HH_SIZE_COL %in% names(df) && is.numeric(df[[HH_SIZE_COL]])) {
  bad <- which(!is.na(df[[HH_SIZE_COL]]) & df[[HH_SIZE_COL]] < 1)
  if (length(bad) > 0) {
    constraint_fixes <- bind_rows(constraint_fixes, tibble::tibble(
      column = HH_SIZE_COL, row_id = df$row_id[bad],
      old = as.character(df[[HH_SIZE_COL]][bad]),
      new = NA_character_,
      rule = "Household size must be >= 1"
    ))
    df[[HH_SIZE_COL]][bad] <- NA_real_
  }
}

# Age: prefer questionnaire age column; drop artifact "age" if present
age_cols_present <- intersect(c("age", AGE_COL_PREFERRED), names(df))
age_mismatch_log <- tibble::tibble()
if (all(c("age", AGE_COL_PREFERRED) %in% names(df)) &&
    is.numeric(df[["age"]]) && is.numeric(df[[AGE_COL_PREFERRED]])) {
  mismatch <- which(!is.na(df[["age"]]) & !is.na(df[[AGE_COL_PREFERRED]]) & df[["age"]] != df[[AGE_COL_PREFERRED]])
  mismatch_rate <- ifelse(sum(!is.na(df[["age"]]) & !is.na(df[[AGE_COL_PREFERRED]])) == 0, NA_real_,
                          length(mismatch) / sum(!is.na(df[["age"]]) & !is.na(df[[AGE_COL_PREFERRED]])))
  message(glue("Age mismatch rate (age vs 1.1): {round(100*mismatch_rate, 2)}%"))
  if (length(mismatch) > 0) {
    age_mismatch_log <- tibble::tibble(row_id = df$row_id[mismatch],
                                       age = df[["age"]][mismatch],
                                       age_q = df[[AGE_COL_PREFERRED]][mismatch])
  }
  # Always keep questionnaire column as canonical; drop artifact age
  df <- df %>% select(-age)
}

# -----------------------------
# STEP 10: HARD SKIP LOGIC ENFORCEMENT (2.4 gate)
# If has_loan == 0 -> set borrower-only questions to NA.
# For borrower-only dummy columns, set to 0 (clean) AND also set NA for main numeric/text borrower-only.
# -----------------------------
borrower_only_cols <- names(df)[map_lgl(names(df), \(nm) any(str_starts(nm, BORROWER_ONLY_PREFIXES)))]
borrower_only_dummy <- borrower_only_cols[str_detect(borrower_only_cols, "/")]
borrower_only_main  <- setdiff(borrower_only_cols, borrower_only_dummy)

# Apply:
df <- df %>%
  mutate(
    across(all_of(borrower_only_main), ~ ifelse(has_loan == 0, NA, .))
  )

if (length(borrower_only_dummy) > 0) {
  df <- df %>%
    mutate(
      across(all_of(borrower_only_dummy), ~ ifelse(has_loan == 0, 0L, .))
    )
}

# Skip-logic violation log (non-borrowers with filled borrower-only main fields)
skip_logic_violations <- tibble::tibble()
if (length(borrower_only_main) > 0) {
  tmp <- df %>%
    filter(has_loan == 0) %>%
    transmute(
      row_id,
      n_filled_borrower_only_main = rowSums(across(all_of(borrower_only_main), ~ !is.na(.)))
    ) %>%
    filter(n_filled_borrower_only_main > 0)
  skip_logic_violations <- tmp
}

# -----------------------------
# STEP 11: SPLITS
# -----------------------------
borrowers <- df %>% filter(has_loan == 1)
nonborrowers <- df %>% filter(has_loan == 0)

# -----------------------------
# STEP 12: QC BY INTERVIEWER (IF PRESENT)
# -----------------------------
interviewer_candidates <- names(df)[str_detect(str_to_lower(names(df)), "интервьюер|interviewer")]
qc_by_interviewer <- tibble::tibble()

if (length(interviewer_candidates) > 0) {
  ic <- interviewer_candidates[1]
  
  df_qc <- df %>%
    mutate(
      missing_share_row = rowMeans(is.na(across(everything())))
    )
  
  qc_by_interviewer <- df_qc %>%
    group_by(.data[[ic]]) %>%
    summarise(
      n_interviews = n(),
      avg_missing_pct_all_vars = mean(missing_share_row, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    rename(interviewer = 1) %>%
    arrange(desc(n_interviews))
}
# -----------------------------
# STEP 13: COLUMN PROFILE (POST-CLEAN)
# -----------------------------
column_profile <- tibble::tibble(
  column = names(df),
  dtype = vapply(df, function(x) class(x)[1], character(1)),
  missing_pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
  n_unique = vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1))
) %>% arrange(desc(missing_pct))

# Checkpoint summary
checkpoint_summary <- tibble::tibble(
  metric = c(
    "raw_rows","raw_cols",
    "dropped_exact_duplicates",
    "rows_after_drop",
    "cols_after_drop_empty",
    "dummy_cols_count",
    "borrowers_n","nonborrowers_n","has_loan_missing_n",
    "q24_invalid_multi_select_n",
    "cardinality_violations_n",
    "skip_logic_violations_n",
    "constraint_fixes_n"
  ),
  value = c(
    raw_rows, raw_cols,
    nrow(dropped_dups),
    nrow(df),
    ncol(df),
    length(dummy_cols),
    nrow(borrowers), nrow(nonborrowers), sum(is.na(df$has_loan)),
    nrow(q24_invalid),
    nrow(cardinality_violations),
    nrow(skip_logic_violations),
    nrow(constraint_fixes)
  )
)

# -----------------------------
# EXPORT EVERYTHING (ALL FORMATS)
# -----------------------------
manifest <- list()

manifest <- append(manifest, list(write_outputs(df, glue("survey_master_cleaned_{VERSION}"))))
manifest <- append(manifest, list(write_outputs(borrowers, glue("borrowers_clean_{VERSION}"))))
manifest <- append(manifest, list(write_outputs(nonborrowers, glue("nonborrowers_clean_{VERSION}"))))

manifest <- append(manifest, list(write_outputs(dropped_dups, "dropped_full_duplicates")))
manifest <- append(manifest, list(write_outputs(registry, glue("question_registry_{VERSION}"))))
manifest <- append(manifest, list(write_outputs(matrix_block_flags, glue("matrix_block_flags_{VERSION}"))))

if (nrow(matrix_long) > 0) {
  manifest <- append(manifest, list(write_outputs(matrix_long, glue("matrix_long_audit_{VERSION}"))))
}
if (nrow(q24_invalid) > 0) {
  manifest <- append(manifest, list(write_outputs(q24_invalid, glue("q24_invalid_multi_select_{VERSION}"))))
}
if (nrow(cardinality_violations) > 0) {
  manifest <- append(manifest, list(write_outputs(cardinality_violations, glue("cardinality_violations_{VERSION}"))))
}
if (nrow(skip_logic_violations) > 0) {
  manifest <- append(manifest, list(write_outputs(skip_logic_violations, glue("skip_logic_violations_{VERSION}"))))
}
if (nrow(constraint_fixes) > 0) {
  manifest <- append(manifest, list(write_outputs(constraint_fixes, glue("constraint_fixes_{VERSION}"))))
}
if (nrow(age_mismatch_log) > 0) {
  manifest <- append(manifest, list(write_outputs(age_mismatch_log, glue("age_mismatch_log_{VERSION}"))))
}
if (nrow(qc_by_interviewer) > 0) {
  manifest <- append(manifest, list(write_outputs(qc_by_interviewer, glue("qc_by_interviewer_{VERSION}"))))
}

manifest <- append(manifest, list(write_outputs(column_profile, glue("column_profile_{VERSION}"))))
manifest <- append(manifest, list(write_outputs(checkpoint_summary, glue("checkpoint_summary_{VERSION}"))))

manifest_df <- bind_rows(manifest)
openxlsx::write.xlsx(manifest_df, file.path(OUT_DIR, glue("EXPORT_MANIFEST_{VERSION}.xlsx")), overwrite = TRUE)

message("✅ Data preparation v2 completed.")
message(glue("Outputs written to: {OUT_DIR}"))
