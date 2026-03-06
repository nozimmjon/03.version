# =============================================================================
# 11_econometrics_robust.R
# Robust econometric modeling suite for NPL risk
# =============================================================================
# Input:
#   data/intermediate/04_analytical.rds
# Outputs:
#   outputs/tables/econometrics/*.csv
#   logs/11_econometrics_robust.log
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
    library(tibble)
  library(stringr)
  library(here)
})

has_proc   <- requireNamespace("pROC", quietly = TRUE)
has_glmnet <- requireNamespace("glmnet", quietly = TRUE)
has_mfx    <- requireNamespace("marginaleffects", quietly = TRUE)

in_rds   <- here("data", "intermediate", "04_analytical.rds")
out_dir  <- here("outputs", "tables", "econometrics")
log_file <- here("logs", "11_econometrics_robust.log")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here("logs"), showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(in_rds))
writeLines("", log_file)

tee <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

save_tbl <- function(tbl, stem, caption = NULL) {
  path <- file.path(out_dir, paste0(stem, ".csv"))
  utils::write.csv(tbl, path, row.names = FALSE, na = "")
  if (!is.null(caption)) tee(sprintf("\n=== %s ===", caption))
  print(tbl, n = Inf)
  invisible(tbl)
}

encode_model_vars <- function(dat) {
  dat %>%
    mutate(
      income_variability_num = case_when(
        str_detect(as.character(income_variability), "Барқарор") ~ 1L,
        str_detect(as.character(income_variability), "Бироз")    ~ 2L,
        str_detect(as.character(income_variability), "Жуда")     ~ 3L,
        TRUE                                                      ~ NA_integer_
      ),
      is_male = as.integer(!is.na(gender) & str_detect(as.character(gender), "Эркак")),
      is_married = as.integer(!is.na(marital_status) & str_detect(as.character(marital_status), "Турмуш")),
      has_higher_edu = as.integer(!is.na(education) & str_detect(as.character(education), "Олий")),
      region = if ("region" %in% names(dat)) as.factor(region) else NULL
    )
}

build_model_df <- function(dat, predictors, extra = character(0)) {
  vars <- unique(c("is_npl", predictors, extra))
  dat %>%
    select(any_of(vars)) %>%
    filter(!is.na(is_npl)) %>%
    drop_na()
}

compute_auc <- function(y, p) {
  if (!has_proc) return(NA_real_)
  as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE)))
}

fit_metrics_glm <- function(name, fit, dat) {
  p <- predict(fit, newdata = dat, type = "response")
  y <- dat$is_npl
  base_rate <- mean(y)
  baseline_acc <- max(base_rate, 1 - base_rate)

  tibble(
    model = name,
    family = "GLM logit",
    n = nrow(dat),
    events = sum(y),
    event_rate = round(base_rate * 100, 1),
    predictors = length(attr(stats::terms(fit), "term.labels")),
    nonzero_coeffs = NA_integer_,
    mcfadden_r2 = round(1 - fit$deviance / fit$null.deviance, 4),
    aic = round(AIC(fit), 2),
    auc = round(compute_auc(y, p), 4),
    brier = round(mean((y - p)^2), 4),
    accuracy = round(mean((p >= 0.5) == y) * 100, 1),
    baseline_accuracy = round(baseline_acc * 100, 1),
    lambda_1se = NA_real_,
    lambda_min = NA_real_
  )
}

tidy_glm_coef <- function(fit, model_name) {
  cmat <- summary(fit)$coefficients
  ci <- suppressWarnings(confint.default(fit))

  out <- tibble(
    model = model_name,
    term = rownames(cmat),
    estimate = cmat[, "Estimate"],
    std_err = cmat[, "Std. Error"],
    z_value = cmat[, "z value"],
    p_value = cmat[, "Pr(>|z|)"],
    or = exp(estimate),
    ci_low = exp(ci[rownames(cmat), 1]),
    ci_high = exp(ci[rownames(cmat), 2])
  ) %>%
    mutate(
      across(c(estimate, std_err, z_value, or, ci_low, ci_high), ~ round(.x, 4)),
      p_value = round(p_value, 6),
      signif = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    )

  out
}

extract_mfx <- function(fit, dat, model_name) {
  if (!has_mfx) {
    return(tibble(
      model = character(), term = character(),
      marginal_effect = numeric(), std_error = numeric(),
      conf_low = numeric(), conf_high = numeric(), p_value = numeric()
    ))
  }

  out <- tryCatch({
    me <- marginaleffects::avg_slopes(fit, newdata = dat)
    as_tibble(me) %>%
      transmute(
        model = model_name,
        term = term,
        marginal_effect = estimate,
        std_error = std.error,
        conf_low = conf.low,
        conf_high = conf.high,
        p_value = p.value
      )
  }, error = function(e) {
    tee(sprintf("  [WARN] marginaleffects failed for %s: %s", model_name, e$message))
    tibble(
      model = character(), term = character(),
      marginal_effect = numeric(), std_error = numeric(),
      conf_low = numeric(), conf_high = numeric(), p_value = numeric()
    )
  })

  out %>%
    mutate(
      across(c(marginal_effect, std_error, conf_low, conf_high), ~ round(.x, 6)),
      p_value = round(p_value, 6)
    )
}

tee(sprintf("=== 11_econometrics_robust.R | %s ===", format(Sys.time(), "%Y-%m-%d %H:%M")))
df <- readRDS(in_rds)
borrowers <- df %>% filter(has_loan == 1L) %>% encode_model_vars()
tee(sprintf("Loaded borrowers: %d rows", nrow(borrowers)))

core_predictors <- c(
  "age", "is_male", "is_married", "has_higher_edu",
  "income_midpoint_mln_uzs", "dsr_midpoint", "financial_buffer_months",
  "income_variability_num", "fin_lit_score", "employed_binary",
  "hh_size", "productive_use_flag"
)
core_predictors <- intersect(core_predictors, names(borrowers))

spec_tbl <- tibble(
  model = c("base_logit", "region_fe_logit", "nonlinear_logit", "penalized_lasso", "penalized_ridge"),
  formula_or_spec = c(
    paste("is_npl ~", paste(core_predictors, collapse = " + ")),
    if ("region" %in% names(borrowers)) paste("is_npl ~", paste(core_predictors, collapse = " + "), "+ factor(region)") else NA_character_,
    if (all(c("dsr_midpoint", "income_midpoint_mln_uzs") %in% core_predictors)) {
      paste("is_npl ~", paste(core_predictors, collapse = " + "), "+ I(dsr_midpoint^2) + I(income_midpoint_mln_uzs^2)")
    } else NA_character_,
    "cv.glmnet(alpha=1, lambda=lambda.1se)",
    "cv.glmnet(alpha=0, lambda=lambda.1se)"
  )
)
save_tbl(spec_tbl, "M0_model_specifications", "M0. Model specifications")

# -----------------------------------------------------------------------------
# M1. Base logit
# -----------------------------------------------------------------------------
base_df <- build_model_df(borrowers, core_predictors)
tee(sprintf("Base model sample: %d", nrow(base_df)))
base_formula <- as.formula(paste("is_npl ~", paste(core_predictors, collapse = " + ")))
fit_base <- glm(base_formula, data = base_df, family = binomial(link = "logit"))

m1 <- tidy_glm_coef(fit_base, "base_logit")
save_tbl(m1, "M1_base_logit_coefficients", "M1. Base logit coefficients")

# -----------------------------------------------------------------------------
# Robust GLM variants
# -----------------------------------------------------------------------------
fit_summary <- list(fit_metrics_glm("base_logit", fit_base, base_df))
all_coef <- list(m1)
all_mfx <- list(extract_mfx(fit_base, base_df, "base_logit"))

# region FE model
if ("region" %in% names(borrowers) && dplyr::n_distinct(borrowers$region, na.rm = TRUE) > 1) {
  reg_df <- build_model_df(borrowers, core_predictors, extra = "region")
  reg_formula <- as.formula(paste("is_npl ~", paste(core_predictors, collapse = " + "), "+ factor(region)"))
  fit_reg <- glm(reg_formula, data = reg_df, family = binomial(link = "logit"))

  fit_summary[[length(fit_summary) + 1]] <- fit_metrics_glm("region_fe_logit", fit_reg, reg_df)
  all_coef[[length(all_coef) + 1]] <- tidy_glm_coef(fit_reg, "region_fe_logit")
  all_mfx[[length(all_mfx) + 1]] <- extract_mfx(fit_reg, reg_df, "region_fe_logit")
} else {
  tee("[SKIP] region_fe_logit: region not available")
}

# nonlinear DSR/income model
if (all(c("dsr_midpoint", "income_midpoint_mln_uzs") %in% core_predictors)) {
  nonlin_df <- build_model_df(borrowers, core_predictors)
  nonlin_formula <- as.formula(
    paste(
      "is_npl ~", paste(core_predictors, collapse = " + "),
      "+ I(dsr_midpoint^2) + I(income_midpoint_mln_uzs^2)"
    )
  )
  fit_nonlin <- glm(nonlin_formula, data = nonlin_df, family = binomial(link = "logit"))

  fit_summary[[length(fit_summary) + 1]] <- fit_metrics_glm("nonlinear_logit", fit_nonlin, nonlin_df)
  all_coef[[length(all_coef) + 1]] <- tidy_glm_coef(fit_nonlin, "nonlinear_logit")
  all_mfx[[length(all_mfx) + 1]] <- extract_mfx(fit_nonlin, nonlin_df, "nonlinear_logit")
} else {
  tee("[SKIP] nonlinear_logit: dsr_midpoint and/or income_midpoint_mln_uzs missing")
}

coef_all_tbl <- bind_rows(all_coef)
save_tbl(coef_all_tbl, "M2_all_glm_coefficients", "M2. All GLM coefficients")

key_terms <- c(
  "age", "is_male", "is_married", "has_higher_edu", "income_midpoint_mln_uzs",
  "dsr_midpoint", "financial_buffer_months", "income_variability_num", "fin_lit_score",
  "employed_binary", "hh_size", "productive_use_flag",
  "I(dsr_midpoint^2)", "I(income_midpoint_mln_uzs^2)"
)

coef_key_tbl <- coef_all_tbl %>%
  filter(term %in% key_terms)
save_tbl(coef_key_tbl, "M3_key_glm_coefficients", "M3. Key GLM coefficients")

mfx_tbl <- bind_rows(all_mfx) %>%
  filter(term %in% key_terms)

if (nrow(mfx_tbl) > 0) {
  save_tbl(mfx_tbl, "M4_marginal_effects", "M4. Average marginal effects with uncertainty")
} else {
  tee("[SKIP] M4_marginal_effects: marginaleffects package missing or failed")
}

# -----------------------------------------------------------------------------
# Penalized logit (L1/L2)
# -----------------------------------------------------------------------------
pen_metrics <- list()
pen_coef <- list()

if (has_glmnet) {
  pen_df <- borrowers %>%
    mutate(
      dsr_sq = if ("dsr_midpoint" %in% names(.)) dsr_midpoint^2 else NA_real_,
      income_sq = if ("income_midpoint_mln_uzs" %in% names(.)) income_midpoint_mln_uzs^2 else NA_real_
    ) %>%
    select(any_of(c("is_npl", core_predictors, "region", "dsr_sq", "income_sq"))) %>%
    filter(!is.na(is_npl)) %>%
    drop_na()

  if (nrow(pen_df) > 50) {
    x <- model.matrix(is_npl ~ ., data = pen_df)[, -1, drop = FALSE]
    y <- pen_df$is_npl

    fit_lasso <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)
    fit_ridge <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 0, nfolds = 10)

    pred_lasso <- as.numeric(predict(fit_lasso, newx = x, s = "lambda.1se", type = "response"))
    pred_ridge <- as.numeric(predict(fit_ridge, newx = x, s = "lambda.1se", type = "response"))

    add_pen_metric <- function(name, fit_cv, pred, alpha_label) {
      nz <- sum(abs(as.numeric(coef(fit_cv, s = "lambda.1se"))[-1]) > 0)
      base_rate <- mean(y)
      baseline_acc <- max(base_rate, 1 - base_rate)
      tibble(
        model = name,
        family = paste0("Penalized logit ", alpha_label),
        n = nrow(pen_df),
        events = sum(y),
        event_rate = round(base_rate * 100, 1),
        predictors = ncol(x),
        nonzero_coeffs = nz,
        mcfadden_r2 = NA_real_,
        aic = NA_real_,
        auc = round(compute_auc(y, pred), 4),
        brier = round(mean((y - pred)^2), 4),
        accuracy = round(mean((pred >= 0.5) == y) * 100, 1),
        baseline_accuracy = round(baseline_acc * 100, 1),
        lambda_1se = unname(fit_cv$lambda.1se),
        lambda_min = unname(fit_cv$lambda.min)
      )
    }

    pen_metrics[[length(pen_metrics) + 1]] <- add_pen_metric("penalized_lasso", fit_lasso, pred_lasso, "(L1)")
    pen_metrics[[length(pen_metrics) + 1]] <- add_pen_metric("penalized_ridge", fit_ridge, pred_ridge, "(L2)")

    get_pen_coef <- function(fit_cv, name, keep_all = FALSE) {
      cmat <- as.matrix(coef(fit_cv, s = "lambda.1se"))
      out <- tibble(term = rownames(cmat), estimate = as.numeric(cmat[, 1])) %>%
        filter(term != "(Intercept)") %>%
        mutate(model = name, abs_estimate = abs(estimate))
      if (!keep_all) out <- out %>% filter(abs_estimate > 0)
      if (keep_all) out <- out %>% arrange(desc(abs_estimate)) %>% slice_head(n = 40)
      out %>% select(model, term, estimate, abs_estimate)
    }

    pen_coef[[length(pen_coef) + 1]] <- get_pen_coef(fit_lasso, "penalized_lasso", keep_all = FALSE)
    pen_coef[[length(pen_coef) + 1]] <- get_pen_coef(fit_ridge, "penalized_ridge", keep_all = TRUE)
  } else {
    tee("[SKIP] penalized models: insufficient rows after complete-case filtering")
  }
} else {
  tee("[SKIP] penalized models: glmnet not installed")
}

fit_summary_tbl <- bind_rows(fit_summary, pen_metrics)
save_tbl(fit_summary_tbl, "M5_model_fit_comparison", "M5. Model fit comparison (base + robustness + penalized)")

if (length(pen_coef) > 0) {
  pen_coef_tbl <- bind_rows(pen_coef) %>%
    mutate(across(c(estimate, abs_estimate), ~ round(.x, 6)))
  save_tbl(pen_coef_tbl, "M6_penalized_coefficients", "M6. Penalized model coefficients")
}

tee("\n=== 11_econometrics_robust.R complete ===")
