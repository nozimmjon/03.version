# 02_clean.R
source(here::here("R/00_setup.R"))
source(here::here("R/01_import.R"))

# Helper: in Excel we have both Uzbek/Russian columns and already-clean fields.
# After clean_names(), mapped variables must also be clean_names()'d to match.
map_var <- function(cfg, key) {
  janitor::make_clean_names(cfg$vars[[key]])
}

make_analysis_data <- function(df, cfg) {
  # required columns
  required <- purrr::map_chr(names(cfg$vars), ~ map_var(cfg, .x))
  assert_has_cols(df, required)

  region <- map_var(cfg, "region")
  district <- map_var(cfg, "district")
  age <- map_var(cfg, "age")
  gender <- map_var(cfg, "gender")
  education <- map_var(cfg, "education")
  hh_income <- map_var(cfg, "hh_income")
  income_earners <- map_var(cfg, "income_earners")
  credit_type <- map_var(cfg, "credit_type")
  payment_status <- map_var(cfg, "payment_status")
  extra_src <- map_var(cfg, "extra_funds_source")
  debt_share <- map_var(cfg, "debt_burden_share")
  income_vol <- map_var(cfg, "income_volatility")
  read_contract <- map_var(cfg, "read_contract")
  loan_purpose <- map_var(cfg, "loan_purpose_main")

  # outcome recodes (based on observed Uzbek labels in the dataset)
  df2 <- df %>%
    mutate(
      borrower = !is.na(.data[[credit_type]]) | !is.na(.data[[payment_status]]),
      payment_status_raw = as.character(.data[[payment_status]]),
      payment_group = case_when(
        str_detect(payment_status_raw, "ўз вақтида") ~ "On-time",
        str_detect(payment_status_raw, "1-3 ой") ~ "1–3 months late",
        str_detect(payment_status_raw, "30 кун") ~ "≤30 days late",
        str_detect(payment_status_raw, "3 ой ва ундан кўп|NPL") ~ "NPL (≥3 months)",
        TRUE ~ NA_character_
      ),
      y_on_time = if_else(payment_group == "On-time", 1L, 0L),
      y_late30 = if_else(payment_group == "≤30 days late", 1L, 0L),
      y_npl = if_else(payment_group == "NPL (≥3 months)", 1L, 0L)
    ) %>%
    mutate(
      region = as.factor(.data[[region]]),
      district = as.factor(.data[[district]]),
      credit_type = as.factor(.data[[credit_type]]),
      gender = as.factor(.data[[gender]]),
      education = as.factor(.data[[education]]),
      extra_funds_source = as.factor(.data[[extra_src]]),
      read_contract = as.factor(.data[[read_contract]]),
      loan_purpose_main = as.factor(.data[[loan_purpose]]),
      age = suppressWarnings(as.numeric(.data[[age]])),
      hh_income = as.factor(.data[[hh_income]]),
      income_earners = suppressWarnings(as.numeric(.data[[income_earners]])),
      debt_burden_share = as.factor(.data[[debt_share]]),
      income_volatility = as.factor(.data[[income_vol]])
    )

  # Multi-response reasons for late payment (block 3.7) -> long format helper later
  df2
}

get_late_reason_cols <- function(df, cfg) {
  prefix_raw <- cfg$blocks$late_payment_reasons_prefix
  prefix <- janitor::make_clean_names(prefix_raw)
  cols <- names(df)[stringr::str_starts(names(df), prefix)]
  cols
}
