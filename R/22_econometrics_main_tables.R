#!/usr/bin/env Rscript
root <- normalizePath('.', winslash = '/', mustWork = TRUE)
local_lib <- file.path(root, 'r_libs')
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

m4_path <- here('outputs', 'tables', 'econometrics', 'M4_marginal_effects.csv')
m5_path <- here('outputs', 'tables', 'econometrics', 'M5_model_fit_comparison.csv')
out_dir <- here('outputs', 'tables', 'econometrics', 'main_report')
log_file <- here('logs', '22_econometrics_main_tables.log')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here('logs'), showWarnings = FALSE, recursive = TRUE)
writeLines('', log_file)
tee <- function(...) {
  msg <- paste0(...)
  cat(msg, '\n')
  cat(msg, '\n', file = log_file, append = TRUE)
}

md_table <- function(df) {
  hdr <- paste(names(df), collapse = ' | ')
  sep <- paste(rep('---', ncol(df)), collapse = ' | ')
  rows <- apply(df, 1, function(x) paste(x, collapse = ' | '))
  paste(c(paste0('| ', hdr, ' |'), paste0('| ', sep, ' |'), paste0('| ', rows, ' |')), collapse = '\n')
}

stopifnot(file.exists(m4_path), file.exists(m5_path))

m4 <- readr::read_csv(m4_path, show_col_types = FALSE)
m5 <- readr::read_csv(m5_path, show_col_types = FALSE)

fmt_p <- function(x) ifelse(is.na(x), '', ifelse(x < 0.001, '<0.001', sprintf('%.3f', x)))
fmt_ci <- function(lo, hi, digits = 2) sprintf('[%0.*f; %0.*f]', digits, lo, digits, hi)

model_tbl <- m5 %>%
  mutate(
    model_label = recode(model,
      base_logit = 'Базавий logit',
      region_fe_logit = 'Logit + ҳудудий FE',
      nonlinear_logit = 'Logit + ночизиқли terms',
      penalized_lasso = 'Penalized logit (L1)',
      penalized_ridge = 'Penalized logit (L2)'
    )
  ) %>%
  transmute(
    `Модель` = model_label,
    `N` = n,
    `NPL улуши, %` = sprintf('%.1f', event_rate),
    `AUC` = sprintf('%.3f', auc),
    `Brier` = sprintf('%.3f', brier),
    `Тўғри таснифлаш, %` = sprintf('%.1f', accuracy),
    `McFadden R2` = ifelse(is.na(mcfadden_r2), '', sprintf('%.3f', mcfadden_r2))
  )

sig_terms <- c('age', 'dsr_midpoint', 'financial_buffer_months', 'income_variability_num', 'fin_lit_score', 'has_higher_edu', 'income_midpoint_mln_uzs')

term_tbl <- m4 %>%
  filter(model == 'region_fe_logit', term %in% sig_terms, p_value < 0.05) %>%
  mutate(
    term_label = recode(term,
      age = 'Ёш',
      dsr_midpoint = 'Қарз юки (DSR)',
      financial_buffer_months = 'Молиявий захира',
      income_variability_num = 'Даромад беқарорлиги',
      fin_lit_score = 'Молиявий саводхонлик',
      has_higher_edu = 'Олий маълумот',
      income_midpoint_mln_uzs = 'Даромад миқдори'
    ),
    change_label = recode(term,
      age = '+10 йил',
      dsr_midpoint = '+10 фоиз пункт',
      financial_buffer_months = '+1 ой',
      income_variability_num = '+1 поғона',
      fin_lit_score = '+1 балл',
      has_higher_edu = '0 -> 1',
      income_midpoint_mln_uzs = '+1 млн сўм'
    ),
    scale_mult = case_when(
      term == 'age' ~ 10,
      term == 'dsr_midpoint' ~ 0.10,
      TRUE ~ 1
    ),
    ame_pp = 100 * marginal_effect * scale_mult,
    ci_low_pp = 100 * conf_low * scale_mult,
    ci_high_pp = 100 * conf_high * scale_mult
  ) %>%
  arrange(desc(abs(ame_pp))) %>%
  transmute(
    `Омил` = term_label,
    `Ўзгариш` = change_label,
    `AME, п.п.` = sprintf('%+.2f', ame_pp),
    `95% ИО` = fmt_ci(ci_low_pp, ci_high_pp, 2),
    `p` = fmt_p(p_value)
  )

write.csv(model_tbl, file.path(out_dir, 'T1_model_fit_main_report.csv'), row.names = FALSE, na = '')
write.table(model_tbl, file.path(out_dir, 'T1_model_fit_main_report.tsv'), sep = '\t', row.names = FALSE, quote = FALSE, fileEncoding = 'UTF-8')
write.csv(term_tbl, file.path(out_dir, 'T2_region_fe_ame_main_report.csv'), row.names = FALSE, na = '')
write.table(term_tbl, file.path(out_dir, 'T2_region_fe_ame_main_report.tsv'), sep = '\t', row.names = FALSE, quote = FALSE, fileEncoding = 'UTF-8')

md_lines <- c(
  '# Эконометрик натижалар: асосий жадваллар',
  '',
  '## Жадвал A. Робаст моделлар таққосланиши',
  '',
  md_table(model_tbl),
  '',
  'Эслатма: AUC ва Brier кўрсаткичлари прогностик сифатни, McFadden R2 эса GLM мослашувини ифодалайди.',
  '',
  '## Жадвал B. Танланган region FE модели бўйича асосий AME натижалари',
  '',
  md_table(term_tbl),
  '',
  'Эслатма: AME — NPL эҳтимолидаги ўртача ўзгариш. DSR учун эффект +10 фоиз пунктга, ёш учун +10 йилга ҳисобланган.'
)
writeLines(md_lines, file.path(out_dir, 'econometrics_main_report_tables.md'), useBytes = TRUE)

tee(sprintf('Wrote tables to: %s', out_dir))
tee('Done.')
