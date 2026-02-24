# 30_models.R
source(here::here("R/00_setup.R"))

fit_logit_tidymodels <- function(dat, outcome) {
  library(tidymodels)

  dat_m <- dat %>%
    filter(borrower) %>%
    filter(!is.na(.data[[outcome]])) %>%
    select(
      all_of(outcome),
      age, gender, education, hh_income, income_earners,
      debt_burden_share, income_volatility,
      credit_type, region,
      read_contract
    ) %>%
    mutate(across(where(is.character), as.factor))

  # Tidymodels classification expects the outcome to be a factor.
  # We standardize outcomes to levels c('yes','no') so the event level is 'yes'.
  y <- dat_m[[outcome]]
  if (is.factor(y)) {
    y <- as.character(y)
  }
  if (is.logical(y)) {
    y <- ifelse(y == TRUE, "yes", ifelse(y == FALSE, "no", NA_character_))
  }
  if (is.numeric(y) || is.integer(y)) {
    y <- ifelse(y == 1, "yes", ifelse(y == 0, "no", NA_character_))
  }
  # fall back: treat anything else as character labels
  y <- as.character(y)
  dat_m[[outcome]] <- factor(y, levels = c("yes", "no"))

  # remove rows with missing predictors (simple approach; can be swapped to imputation)
  dat_m <- tidyr::drop_na(dat_m)

  rec <- recipe(stats::as.formula(paste0(outcome, " ~ .")), data = dat_m) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_zv(all_predictors())

  model <- logistic_reg() %>% set_engine("glm")

  wf <- workflow() %>% add_model(model) %>% add_recipe(rec)

  set.seed(42)
  # Stratify folds by outcome to reduce the chance of folds with zero events.
  folds <- vfold_cv(dat_m, v = 5, strata = !!rlang::sym(outcome))

  res <- fit_resamples(
    wf, resamples = folds,
    metrics = metric_set(accuracy, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

  # final fit
  final_fit <- fit(wf, dat_m)

  list(
    data = dat_m,
    workflow = wf,
    resamples = res,
    final_fit = final_fit
  )
}

model_quality_table <- function(model_list, outcome_label) {
  library(tidymodels)
  library(gt)

  m <- collect_metrics(model_list$resamples) %>%
    select(.metric, mean, std_err) %>%
    mutate(outcome = outcome_label)

  wide <- m %>%
    tidyr::pivot_wider(names_from = .metric, values_from = c(mean, std_err))

  gt::gt(wide) %>%
    gt::tab_header(title = "Logistic regression model quality (5-fold CV)") %>%
    gt::fmt_number(columns = everything(), decimals = 3)
}

coef_forest_plot <- function(model_list, title) {
  library(tidymodels)
  library(broom)
  library(dplyr)
  library(ggplot2)

  glm_fit <- extract_fit_parsnip(model_list$final_fit)$fit
  td <- broom::tidy(glm_fit)
  # Use Wald confidence intervals to avoid profile-likelihood failures
  z <- stats::qnorm(0.975)
  td <- td %>%
    dplyr::mutate(
      conf.low = estimate - z * std.error,
      conf.high = estimate + z * std.error
    )

  coefs <- td %>%
    filter(term != "(Intercept)") %>%
    mutate(or = exp(estimate),
           or_low = exp(conf.low),
           or_high = exp(conf.high)) %>%
    arrange(or)

  ggplot(coefs, aes(x = or, y = reorder(term, or))) +
    geom_point() +
    geom_errorbarh(aes(xmin = or_low, xmax = or_high), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10() +
    labs(title = title, subtitle = "Odds ratios with 95% CI (log scale)",
         caption = "Controls: demographics, income, debt burden, credit type, region, contract reading") +
    theme_report()
}
