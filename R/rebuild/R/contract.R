validate_required_columns <- function(data, required_columns) {
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

validate_has_loan_domain <- function(data, allowed_values = c(0, 1)) {
  if (!"has_loan" %in% names(data)) {
    stop("Column `has_loan` is required for domain validation.")
  }

  observed <- unique(stats::na.omit(data$has_loan))
  invalid <- setdiff(observed, allowed_values)

  if (length(invalid) > 0) {
    stop("Invalid has_loan values detected: ", paste(invalid, collapse = ", "))
  }

  invisible(TRUE)
}

apply_data_contract <- function(data, cfg) {
  validate_required_columns(data, cfg$contract$required_columns)
  validate_has_loan_domain(data, cfg$contract$has_loan_values)
  data
}
