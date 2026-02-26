#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(tibble)
  library(dplyr)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
log_dir <- file.path(project_root, "outputs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, "replication_run_log.txt")
manifest_file <- file.path(log_dir, "replication_run_manifest.csv")

writeLines(c(
  paste0("Full replication run started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Project root: ", project_root),
  ""
), con = log_file)

append_log <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n")
  write(txt, file = log_file, append = TRUE)
}

run_step <- function(step_id, label, command, args = character()) {
  append_log("[", step_id, "] START  ", label)
  append_log("[", step_id, "] CMD    ", paste(c(command, args), collapse = " "))

  out <- character()
  err <- character()
  status <- tryCatch(
    {
      out <- system2(command, args = args, stdout = TRUE, stderr = TRUE)
      0L
    },
    error = function(e) {
      err <<- conditionMessage(e)
      1L
    }
  )

  if (length(out) > 0) {
    write(out, file = log_file, append = TRUE)
  }
  if (length(err) > 0) {
    write(err, file = log_file, append = TRUE)
  }

  if (status != 0L) {
    append_log("[", step_id, "] FAIL   ", label)
    stop("Step failed: ", label, " (see ", log_file, ")")
  }

  append_log("[", step_id, "] DONE   ", label)
  append_log("")
  invisible(TRUE)
}

steps <- tribble(
  ~step_id, ~label, ~command, ~args,
  "A", "Run cleaning pipeline", "Rscript", list(c("R/Data Preparation Pipeline v2.R")),
  "B", "Run cleaning audit", "python", list(c("audit_cleaning.py")),
  "C1", "Recreate base charts", "Rscript", list(c("replication/recreate_claudecode_charts.R")),
  "C2", "Recreate block-2 charts", "Rscript", list(c("replication/recreate_claudecode_block2_charts.R")),
  "C3", "Recreate remaining charts", "Rscript", list(c("replication/recreate_claudecode_remaining_charts.R")),
  "D", "Recreate tables", "Rscript", list(c("replication/recreate_claudecode_tables.R"))
)

for (i in seq_len(nrow(steps))) {
  run_step(steps$step_id[[i]], steps$label[[i]], steps$command[[i]], steps$args[[i]])
}

expected_files <- c(
  "outputs_prep_v2/survey_master_cleaned_v2.csv",
  "outputs_prep_v2/checkpoint_summary_v2.csv",
  "outputs_prep_v2/EXPORT_MANIFEST_v2.xlsx",
  "replication/charts_ggplot2/chart_01_regional_distribution_ggplot2.png",
  "replication/charts_ggplot2/chart_38_payment_priorities_ggplot2.png",
  "outputs/tables/replication/table_01_income_levels.csv",
  "outputs/tables/replication/table_13_correlation_matrix.csv",
  "outputs/tables/replication/table_validation_log.csv"
)

manifest <- tibble(
  path = expected_files,
  exists = file.exists(expected_files),
  size_bytes = ifelse(file.exists(expected_files), file.info(expected_files)$size, NA_real_),
  modified_at = ifelse(file.exists(expected_files), as.character(file.info(expected_files)$mtime), NA_character_)
)

write_csv(manifest, manifest_file)

if (!all(manifest$exists)) {
  missing <- manifest %>% filter(!exists) %>% pull(path)
  append_log("Missing expected artifacts: ", paste(missing, collapse = ", "))
  stop("Replication completed with missing artifacts. See ", manifest_file)
}

append_log("Full replication run completed successfully.")
append_log("Artifact manifest: ", manifest_file)
