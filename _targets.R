library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "here","yaml","dplyr","tidyr","stringr","readxl","janitor","purrr",
    "ggplot2","scales","glue","gtsummary","gt","tidymodels","broom"
  )
)

source(here::here("R/00_setup.R"))
source(here::here("R/01_import.R"))
source(here::here("R/02_clean.R"))
source(here::here("R/10_descriptives.R"))
source(here::here("R/20_charts.R"))
source(here::here("R/30_models.R"))
source(here::here("R/40_outputs.R"))

list(
  tar_target(cfg, read_config(), format = "rds"),

  tar_target(raw_data, import_raw(cfg), format = "rds"),
  tar_target(analysis_data, make_analysis_data(raw_data, cfg), format = "rds"),
  tar_target(late_reason_cols, get_late_reason_cols(raw_data, cfg)),

  tar_target(write_analysis_rds, {
    path <- here::here("data/processed/analysis_data.rds")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(analysis_data, path)
    path
  }, format = "file"),

  # Tables
  tar_target(tables, make_tables(analysis_data), format = "rds"),

  tar_target(save_tables, {
    paths <- c(
      save_gt(tables$tbl_params, here::here("outputs/tables/table_06_parameters.html")),
      save_gt(tables$tbl_credit_type, here::here("outputs/tables/table_07_credit_type.html")),
      save_gt(tables$tbl_payment, here::here("outputs/tables/table_08_payment_status.html")),
      save_gt(tables$tbl_demo, here::here("outputs/tables/table_09_demographics.html"))
    )
    paths
  }, format = "file"),

  # Charts 1–6
  tar_target(fig1, plot_extra_funds_sources(analysis_data), format = "rds"),
  tar_target(fig2, plot_extra_funds_by_region(analysis_data), format = "rds"),
  tar_target(fig3, plot_late_payment_reasons(analysis_data, late_reason_cols), format = "rds"),
  tar_target(fig4, plot_debt_burden(analysis_data), format = "rds"),
  tar_target(fig5, plot_microloan_purpose(analysis_data), format = "rds"),
  tar_target(fig6, plot_read_contract(analysis_data), format = "rds"),

  tar_target(save_figures, {
    paths <- c(
      save_plot(fig1, here::here("outputs/figures/fig1_extra_funds_sources.png"), width=7, height=4.5),
      save_plot(fig2, here::here("outputs/figures/fig2_sources_by_region.png"), width=8.2, height=4.8),
      save_plot(fig3, here::here("outputs/figures/fig3_late_reasons.png"), width=7.5, height=4.8),
      save_plot(fig4, here::here("outputs/figures/fig4_debt_burden.png"), width=7, height=4.5),
      save_plot(fig5, here::here("outputs/figures/fig5_microloan_purpose.png"), width=7.5, height=4.8),
      save_plot(fig6, here::here("outputs/figures/fig6_read_contract.png"), width=7, height=4.2)
    )
    paths
  }, format = "file"),

  # Models + coefficient plots (Charts 7–9 analogs)
  tar_target(model_on_time, fit_logit_tidymodels(analysis_data, "y_on_time")),
  tar_target(model_late30, fit_logit_tidymodels(analysis_data, "y_late30")),
  tar_target(model_npl, fit_logit_tidymodels(analysis_data, "y_npl")),

  tar_target(tbl_model_quality, {
    # produce a single combined table-like data frame
    library(tidymodels)
    bind_rows(
      collect_metrics(model_on_time$resamples) %>% mutate(outcome = "On-time"),
      collect_metrics(model_late30$resamples) %>% mutate(outcome = "≤30 days late"),
      collect_metrics(model_npl$resamples) %>% mutate(outcome = "NPL")
    )
  }, format = "rds"),

  tar_target(save_model_quality, {
    library(gt)
    t <- tbl_model_quality %>%
      select(outcome, .metric, mean, std_err) %>%
      tidyr::pivot_wider(names_from = .metric, values_from = c(mean, std_err))
    g <- gt::gt(t) %>%
      gt::tab_header(title = "Model quality metrics (5-fold CV)") %>%
      gt::fmt_number(columns = everything(), decimals = 3)
    save_gt(g, here::here("outputs/tables/table_10_model_quality.html"))
  }, format = "file"),

  tar_target(fig7, coef_forest_plot(model_on_time, "Logistic regression: factors associated with on-time repayment"), format="rds"),
  tar_target(fig8, coef_forest_plot(model_late30, "Logistic regression: factors associated with ≤30 days delinquency"), format="rds"),
  tar_target(fig9, coef_forest_plot(model_npl, "Logistic regression: factors associated with NPL risk"), format="rds"),

  tar_target(save_model_figures, {
    paths <- c(
      save_plot(fig7, here::here("outputs/figures/fig7_logit_on_time.png"), width=8.2, height=5.6),
      save_plot(fig8, here::here("outputs/figures/fig8_logit_late30.png"), width=8.2, height=5.6),
      save_plot(fig9, here::here("outputs/figures/fig9_logit_npl.png"), width=8.2, height=5.6)
    )
    paths
  }, format="file")
)
