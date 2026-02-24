# 10_descriptives.R
source(here::here("R/00_setup.R"))

make_tables <- function(dat) {
  library(gtsummary)
  library(gt)

  # Table 6: key parameters
  tbl_params <- tibble::tibble(
    metric = c("Total respondents (N)", "Borrowers (N)", "Non-borrowers (N)", "Regions (count)"),
    value = c(
      nrow(dat),
      sum(dat$borrower, na.rm = TRUE),
      sum(!dat$borrower, na.rm = TRUE),
      dplyr::n_distinct(dat$region)
    )
  ) %>%
    gt::gt() %>%
    gt::tab_header(title = "Key study parameters")

  # Table 7: distribution by credit type (borrowers only)
  tbl_credit_type <- dat %>%
    filter(borrower) %>%
    count(credit_type) %>%
    mutate(share = n / sum(n)) %>%
    gt::gt() %>%
    gt::fmt_percent(columns = "share", decimals = 1) %>%
    gt::tab_header(title = "Borrowers by credit type")

  # Table 8: distribution by payment status group (borrowers with status)
  tbl_payment <- dat %>%
    filter(borrower, !is.na(payment_group)) %>%
    count(payment_group) %>%
    mutate(share = n / sum(n)) %>%
    gt::gt() %>%
    gt::fmt_percent(columns = "share", decimals = 1) %>%
    gt::tab_header(title = "Borrowers by payment status")

  # Table 9: demographic profile (overall vs borrowers vs non-borrowers)
  demo <- dat %>%
    mutate(group = case_when(
      borrower ~ "Borrowers",
      !borrower ~ "Non-borrowers",
      TRUE ~ NA_character_
    )) %>%
    mutate(group = factor(group, levels = c("Borrowers","Non-borrowers")))

  tbl_demo <- demo %>%
    select(group, age, gender, education, income_earners, hh_income) %>%
    gtsummary::tbl_summary(by = group, missing = "no") %>%
    gtsummary::add_overall() %>%
    gtsummary::bold_labels() %>%
    gtsummary::as_gt() %>%
    gt::tab_header(title = "Demographic profile")

  list(
    tbl_params = tbl_params,
    tbl_credit_type = tbl_credit_type,
    tbl_payment = tbl_payment,
    tbl_demo = tbl_demo
  )
}
