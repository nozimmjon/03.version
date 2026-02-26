library(testthat)

source("R/rebuild/R/contract.R")


test_that("required columns validator errors when columns are missing", {
  data <- data.frame(id = 1:3)
  expect_error(
    validate_required_columns(data, c("has_loan")),
    "Missing required columns"
  )
})


test_that("has_loan domain validator accepts only 0/1/NA", {
  ok <- data.frame(has_loan = c(0, 1, NA))
  expect_silent(validate_has_loan_domain(ok, c(0, 1)))

  bad <- data.frame(has_loan = c(0, 2, NA))
  expect_error(
    validate_has_loan_domain(bad, c(0, 1)),
    "Invalid has_loan values"
  )
})
