normalize_key_fields <- function(data) {
  data |>
    dplyr::mutate(
      has_loan = dplyr::if_else(is.na(has_loan), NA_real_, as.numeric(has_loan))
    )
}

split_borrower_groups <- function(data) {
  list(
    borrowers = dplyr::filter(data, has_loan == 1),
    nonborrowers = dplyr::filter(data, has_loan == 0)
  )
}

build_run_manifest <- function(cleaned, split_data, cfg) {
  tibble::tibble(
    run_timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    project_name = cfg$project$name,
    raw_input = cfg$paths$raw_data_xlsx,
    cleaned_rows = nrow(cleaned),
    borrower_rows = nrow(split_data$borrowers),
    nonborrower_rows = nrow(split_data$nonborrowers)
  )
}
