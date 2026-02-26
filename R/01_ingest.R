# =============================================================================
# 01_ingest.R  —  Raw data ingestion, PII separation, intermediate write
# =============================================================================
# Pipeline:  dataset17022026.xlsx  →  data/intermediate/01_raw.rds
#                                   →  data/pii/pii_table.rds
#            Checksum logged to:      logs/checksums.log
#
# What this script does (in order):
#   1. Checksum  — MD5 of raw Excel; abort if file changed since last run
#   2. Read      — all 248 columns with UTF-8 locale; skip first row header only
#   3. Reconstruct false-split cols — "/" in Q-text caused CAPI to export
#                  single-value questions as dummy cols; we rename them back
#   4. Rename    — raw column headers → codebook var_names
#   5. row_id    — add stable integer key before any sorting/filtering
#   6. Dedup     — drop 8 exact duplicate rows
#   7. PII sep   — move name/phone/DOB into encrypted-adjacent pii_table.rds
#   8. Write     — data/intermediate/01_raw.rds  (analytic dataset, no PII)
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(digest)    # for md5 checksum
  library(here)
})

# ── 0. Paths ──────────────────────────────────────────────────────────────────
raw_xlsx   <- here("dataset17022026.xlsx")
cb_csv     <- here("codebook", "codebook.csv")
out_rds    <- here("data", "intermediate", "01_raw.rds")
pii_rds    <- here("data", "pii", "pii_table.rds")
chk_log    <- here("logs", "checksums.log")

stopifnot(file.exists(raw_xlsx), file.exists(cb_csv))
dir.create(dirname(out_rds), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(pii_rds), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(chk_log), showWarnings = FALSE, recursive = TRUE)

# ── 1. Checksum ───────────────────────────────────────────────────────────────
md5_now <- digest(raw_xlsx, algo = "md5", file = TRUE)
cat("Raw file MD5:", md5_now, "\n")

log_lines <- if (file.exists(chk_log)) readLines(chk_log) else character(0)
prev_entry <- log_lines[str_detect(log_lines, "dataset17022026.xlsx")]

if (length(prev_entry) > 0) {
  prev_md5 <- str_extract(prev_entry[length(prev_entry)], "[a-f0-9]{32}")
  if (!is.na(prev_md5) && prev_md5 != md5_now) {
    warning(
      "Raw file has changed since last run!\n",
      "  Previous MD5: ", prev_md5, "\n",
      "  Current  MD5: ", md5_now,  "\n",
      "  If intentional, delete or update logs/checksums.log and re-run."
    )
  } else {
    cat("Checksum matches previous run — raw file unchanged.\n")
  }
}
write(
  paste0(Sys.time(), "  dataset17022026.xlsx  md5=", md5_now),
  file = chk_log, append = TRUE
)

# ── 2. Read codebook (non-derived rows with a real raw_col_name) ──────────────
cb <- read_csv(cb_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
  filter(
    section != "derived",
    !is.na(raw_col_name),
    !str_starts(raw_col_name, "NOT IN RAW"),
    !str_starts(raw_col_name, "DERIVED"),
    !str_detect(raw_col_name, fixed("(matrix"))
  ) %>%
  select(var_name, raw_col_name, q_type, analytic_role, section)

cat("Codebook rows with mappable raw_col_name:", nrow(cb), "\n")

# ── 3. Read raw Excel (all columns, text-only first pass) ─────────────────────
cat("Reading raw Excel...\n")
raw <- read_excel(
  raw_xlsx,
  col_types = "text",   # read everything as text; retype in 02_clean.R
  .name_repair = "minimal"
)
cat("Raw dimensions:", nrow(raw), "rows ×", ncol(raw), "cols\n")

# ── 4. Reconstruct false-dummy-split single-value columns ─────────────────────
# CAPI splits any column whose header contains "/" into dummy sub-blocks.
# Three single-value questions were affected:
#   • n_loans         (Q2.4.1) — numeric
#   • financial_buffer(Q2.4.3) — single_choice
#   • bank_explained_terms(Q3.6) — single_choice
#
# Strategy: the full raw column name IS the raw_col_name in the codebook
# (the "/" is part of the actual header string).  read_excel preserves these
# as-is, so we only need to verify they are present.

false_split_vars <- cb %>%
  filter(str_detect(raw_col_name, "/")) %>%
  pull(raw_col_name)

cat("False-split single-value columns found in codebook:", length(false_split_vars), "\n")
for (fc in false_split_vars) {
  present <- fc %in% names(raw)
  cat("  [", if (present) "OK" else "MISSING", "] ", fc, "\n", sep = "")
}

# ── 4b. Resolve admin 'age' column collision ──────────────────────────────────
# Raw data has two 'age'-related columns:
#   col 15: 'age'          — admin pre-filled age from bank records
#   col 19: '1.1. Ёшингиз:' — Q1.1 self-reported age (codebook var_name = 'age')
# Rename the admin one to 'age_admin' before applying the codebook renames.
if ("age" %in% names(raw)) {
  raw <- raw %>% rename(age_admin = age)
  cat("Admin 'age' column renamed to 'age_admin' to avoid Q1.1 collision.\n")
}

# ── 5. Select & rename codebook-mapped columns ────────────────────────────────
# Only rename columns that exist in raw AND are in the codebook mapping.
present_map <- cb %>%
  filter(raw_col_name %in% names(raw)) %>%
  select(raw_col_name, var_name)

cat("\nColumns mapped (raw → var_name):", nrow(present_map), "\n")
missing_map  <- cb %>% filter(!raw_col_name %in% names(raw))
if (nrow(missing_map) > 0) {
  cat("WARNING — codebook columns NOT found in raw data:\n")
  for (i in seq_len(nrow(missing_map))) {
    cat("  ", missing_map$var_name[i], " → '", missing_map$raw_col_name[i], "'\n", sep = "")
  }
}

# Build rename vector: setNames(old_names, new_names) for dplyr::rename
rename_vec <- setNames(present_map$raw_col_name, present_map$var_name)

# Keep ALL 248 columns; rename the ones we have mappings for; leave dummy cols as-is
raw_renamed <- raw %>%
  rename(all_of(rename_vec))

cat("After rename — named cols:", sum(names(raw_renamed) %in% present_map$var_name), "\n")

# ── 6. Add stable row_id BEFORE dedup ─────────────────────────────────────────
raw_renamed <- raw_renamed %>%
  mutate(row_id = row_number(), .before = 1)

# ── 7. Drop exact duplicate rows ──────────────────────────────────────────────
n_before <- nrow(raw_renamed)
raw_dedup <- raw_renamed %>%
  distinct(across(-row_id), .keep_all = TRUE)
n_after  <- nrow(raw_dedup)
cat("\nDuplicate rows dropped:", n_before - n_after,
    " (", n_before, " → ", n_after, ")\n", sep = "")

# ── 8. Separate PII columns ───────────────────────────────────────────────────
pii_vars <- c("resp_name_main", "resp_name_add", "phone_number", "date_of_birth")
pii_present <- intersect(pii_vars, names(raw_dedup))
cat("\nPII columns separated:", paste(pii_present, collapse = ", "), "\n")

pii_table <- raw_dedup %>%
  select(row_id, any_of(pii_vars))

analytic <- raw_dedup %>%
  select(-any_of(setdiff(pii_vars, "row_id")))

cat("Analytic dataset:", nrow(analytic), "rows ×", ncol(analytic), "cols\n")
cat("PII table       :", nrow(pii_table), "rows ×", ncol(pii_table), "cols\n")

# ── 9. Write outputs ───────────────────────────────────────────────────────────
saveRDS(analytic,  out_rds)
saveRDS(pii_table, pii_rds)

cat("\n✓ Saved:", out_rds, "\n")
cat("✓ Saved:", pii_rds, "\n")

# ── 10. Summary report ────────────────────────────────────────────────────────
cat("\n=== INGEST SUMMARY ===\n")
cat("Raw file          :", basename(raw_xlsx), "\n")
cat("MD5               :", md5_now, "\n")
cat("Rows in raw       :", n_before, "\n")
cat("Duplicates dropped:", n_before - n_after, "\n")
cat("Rows in analytic  :", nrow(analytic), "\n")
cat("Cols in analytic  :", ncol(analytic), "\n")
cat("  of which named  :", sum(names(analytic) %in% cb$var_name), "\n")
cat("  still raw-named :", sum(!names(analytic) %in% c(cb$var_name, "row_id")), "\n")
cat("PII rows / cols   :", nrow(pii_table), "/", ncol(pii_table), "\n")
cat("Date/time         :", format(Sys.time()), "\n")
cat("======================\n")
