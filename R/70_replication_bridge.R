# 70_replication_bridge.R
source(here::here("R/00_setup.R"))

replication_chart_files <- function() {
  ids <- c(sprintf("%02d", 1:23), sprintf("%02d", 25:30), sprintf("%02d", 33:38))
  file.path("replication/charts_ggplot2", paste0("chart_", ids, "_", c(
    "regional_distribution", "age_distribution", "gender", "income", "credit_type",
    "repayment_status", "income_sources", "education", "first_source", "source_by_region",
    "why_informal", "why_banks", "credit_purpose", "borrowing_reason", "purpose_by_status",
    "decision_factors", "dti_by_status", "volatility_by_status", "npl_reasons", "contract_by_status",
    "terms_by_status", "regret_by_status", "online_vs_offline",
    "collection_by_status", "effective_reminders", "family_npl_reasons", "family_support",
    "npl_by_credit_type", "why_family_credit", "npl_causes_detailed", "decision_factors",
    "credit_rating_awareness", "purpose_npl", "payment_norms", "payment_priorities"
  ), "_ggplot2.png"))
}

replication_table_files <- function() {
  file.path("outputs/tables/replication", c(
    sprintf("table_%02d_%s.csv", 1, "income_levels"),
    sprintf("table_%02d_%s.csv", 2, "npl_cause_categories"),
    sprintf("table_%02d_%s.csv", 3, "contract_familiarity"),
    sprintf("table_%02d_%s.csv", 4, "collection_methods_by_status"),
    sprintf("table_%02d_%s.csv", 5, "family_nonpayment_reasons"),
    sprintf("table_%02d_%s.csv", 6, "research_parameters"),
    sprintf("table_%02d_%s.csv", 7, "credit_type_distribution"),
    sprintf("table_%02d_%s.csv", 8, "payment_status_distribution"),
    sprintf("table_%02d_%s.csv", 9, "demographic_profile"),
    sprintf("table_%02d_%s.csv", 10, "model_quality"),
    sprintf("table_%02d_%s.csv", 11, "statistical_summary"),
    sprintf("table_%02d_%s.csv", 12, "econometric_results"),
    sprintf("table_%02d_%s.csv", 13, "correlation_matrix")
  ))
}

run_replication_charts <- function() {
  scripts <- c(
    "replication/recreate_claudecode_charts.R",
    "replication/recreate_claudecode_block2_charts.R",
    "replication/recreate_claudecode_remaining_charts.R"
  )
  purrr::walk(scripts, ~ system2("Rscript", .x, stdout = TRUE, stderr = TRUE))
  files <- replication_chart_files()
  missing <- files[!file.exists(here::here(files))]
  if (length(missing) > 0) {
    stop("Missing replication charts: ", paste(missing, collapse = ", "))
  }
  here::here(files)
}

run_replication_tables <- function() {
  system2("Rscript", "replication/recreate_claudecode_tables.R", stdout = TRUE, stderr = TRUE)
  files <- replication_table_files()
  missing <- files[!file.exists(here::here(files))]
  if (length(missing) > 0) {
    stop("Missing replication tables: ", paste(missing, collapse = ", "))
  }
  here::here(files)
}
