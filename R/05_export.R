# =============================================================================
# 05_export.R  —  Export analytical datasets to CSV / XLSX
# =============================================================================
# Pipeline:  data/analytical/*.rds  →  outputs/
#                                        ├── survey_master.csv
#                                        ├── survey_master_UTF8_BOM.csv
#                                        ├── survey_master.xlsx
#                                        ├── borrowers.{csv,xlsx}
#                                        ├── non_borrowers.{csv,xlsx}
#                                        ├── family_biz.{csv,xlsx}
#                                        └── EXPORT_MANIFEST.xlsx
#            Execution record appended to:  logs/05_export.log
#
# Notes:
#   • Ordered / unordered factors → character before export so CSV readers
#     see the label strings, not integer codes.
#   • UTF-8 BOM variant makes Cyrillic labels readable in Excel on Windows.
#   • XLSX is written with openxlsx (no Java dependency).
#   • Column manifest from 04_construct (logs/04_column_manifest.csv) is
#     embedded as a second sheet in EXPORT_MANIFEST.xlsx for provenance.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})

if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("Package 'openxlsx' required.  Install with: install.packages('openxlsx')")

# ── 0. Paths ──────────────────────────────────────────────────────────────────
analytical_dir <- here("data", "analytical")
out_dir        <- here("outputs")
log_file       <- here("logs", "05_export.log")
manifest_xlsx  <- here("outputs", "EXPORT_MANIFEST.xlsx")
col_manifest   <- here("logs", "04_column_manifest.csv")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)

# ── 1. Helpers ────────────────────────────────────────────────────────────────

# Convert all factor columns to character so CSV exports show labels not codes
defactor <- function(df) {
  df %>% mutate(across(where(is.factor), as.character))
}

# Write UTF-8 BOM CSV (makes Cyrillic / Uzbek labels open correctly in Excel)
write_bom_csv <- function(df, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  utils::write.csv(df, con, row.names = FALSE, na = "")
}

# Write all three formats; return a one-row tibble for the manifest
write_dataset <- function(df, stem, out_dir) {
  df_out <- defactor(df)

  csv_path  <- file.path(out_dir, paste0(stem, ".csv"))
  bom_path  <- file.path(out_dir, paste0(stem, "_UTF8_BOM.csv"))
  xlsx_path <- file.path(out_dir, paste0(stem, ".xlsx"))

  write_csv(df_out, csv_path,  na = "")
  write_bom_csv(df_out, bom_path)
  openxlsx::write.xlsx(df_out, xlsx_path, overwrite = TRUE)

  cat(sprintf("  ✓ %-40s  %d rows  %d cols\n",
              basename(csv_path), nrow(df_out), ncol(df_out)))

  tibble(
    stem       = stem,
    csv        = csv_path,
    csv_bom    = bom_path,
    xlsx       = xlsx_path,
    n_rows     = nrow(df_out),
    n_cols     = ncol(df_out),
    exported_at = format(Sys.time())
  )
}

# ── 2. Load datasets ──────────────────────────────────────────────────────────
cat("=== LOADING DATASETS ===\n")

load_rds <- function(name) {
  path <- here("data", "analytical", paste0(name, ".rds"))
  if (!file.exists(path)) {
    cat(sprintf("  [SKIP] %s not found: %s\n", name, path))
    return(NULL)
  }
  df <- readRDS(path)
  cat(sprintf("  Loaded %-20s  %d rows x %d cols\n", name, nrow(df), ncol(df)))
  df
}

master       <- readRDS(here("data", "intermediate", "04_analytical.rds"))
borrowers    <- load_rds("borrowers")
non_borrowers <- load_rds("non_borrowers")
family_biz   <- load_rds("family_biz")

cat(sprintf("\nFull dataset: %d rows x %d cols\n\n", nrow(master), ncol(master)))

# ── 3. Export ─────────────────────────────────────────────────────────────────
cat("=== EXPORTING ===\n")

manifest_rows <- list()

manifest_rows[["master"]] <- write_dataset(master, "survey_master", out_dir)

if (!is.null(borrowers))
  manifest_rows[["borrowers"]] <- write_dataset(borrowers, "borrowers", out_dir)

if (!is.null(non_borrowers))
  manifest_rows[["non_borrowers"]] <- write_dataset(non_borrowers, "non_borrowers", out_dir)

if (!is.null(family_biz) && nrow(family_biz) > 0)
  manifest_rows[["family_biz"]] <- write_dataset(family_biz, "family_biz", out_dir)

manifest_df <- bind_rows(manifest_rows)

# ── 4. Column type summary (appended to manifest) ────────────────────────────
col_summary <- tibble(
  col_name    = names(master),
  col_class   = vapply(master, function(x) paste(class(x), collapse = "/"), character(1)),
  is_factor   = vapply(master, is.factor,   logical(1)),
  is_ordered  = vapply(master, is.ordered,  logical(1)),
  pct_missing = round(vapply(master, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
  n_unique    = vapply(master, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1))
)

# ── 5. Write EXPORT_MANIFEST.xlsx (multi-sheet) ───────────────────────────────
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "exports")
openxlsx::writeData(wb, "exports", manifest_df)

openxlsx::addWorksheet(wb, "column_types")
openxlsx::writeData(wb, "column_types", col_summary)

# Embed 04_column_manifest if it exists
if (file.exists(col_manifest)) {
  cm <- read_csv(col_manifest, show_col_types = FALSE)
  openxlsx::addWorksheet(wb, "04_column_manifest")
  openxlsx::writeData(wb, "04_column_manifest", cm)
}

# Style: freeze top row, auto-width on exports sheet
openxlsx::freezePane(wb, "exports",      firstRow = TRUE)
openxlsx::freezePane(wb, "column_types", firstRow = TRUE)
openxlsx::setColWidths(wb, "exports",      cols = seq_len(ncol(manifest_df)), widths = "auto")
openxlsx::setColWidths(wb, "column_types", cols = seq_len(ncol(col_summary)),  widths = "auto")

openxlsx::saveWorkbook(wb, manifest_xlsx, overwrite = TRUE)
cat(sprintf("\n✓ Export manifest: %s\n", manifest_xlsx))

# ── 6. Console summary ────────────────────────────────────────────────────────
cat("\n=== EXPORT SUMMARY ===\n")
cat(sprintf("Output directory  : %s\n", out_dir))
cat(sprintf("Datasets exported : %d\n", nrow(manifest_df)))
cat(sprintf("Total files       : %d\n", nrow(manifest_df) * 3L))  # csv + bom + xlsx

cat("\nFactor columns → character in exports:\n")
fac_cols <- col_summary %>% filter(is_factor) %>% pull(col_name)
cat(sprintf("  %d factor columns defactored\n", length(fac_cols)))

# ── 7. Append to log ──────────────────────────────────────────────────────────
log_line <- sprintf(
  "%s  05_export  datasets=%d  master_rows=%d  master_cols=%d  out_dir=%s",
  format(Sys.time()), nrow(manifest_df), nrow(master), ncol(master), out_dir
)
write(log_line, file = log_file, append = TRUE)
cat(sprintf("\n✓ Log appended: %s\n", log_file))

cat("\n=== 05_export COMPLETE ===\n")
