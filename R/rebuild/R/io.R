read_raw_survey <- function(path) {
  if (!file.exists(path)) {
    stop("Raw input file not found: ", path)
  }

  readxl::read_excel(path) |>
    janitor::clean_names()
}

write_csv_utf8 <- function(data, path) {
  readr::write_csv(data, file = path, na = "")
  invisible(path)
}
