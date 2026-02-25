# 01_import.R
source(here::here("R/00_setup.R"))

read_config <- function() {
  yaml::read_yaml(here::here("reports/config.yml"))
}

import_raw <- function(cfg) {
  csv_path <- cfg$data$csv_path
  if (!is.null(csv_path)) {
    csv_abs <- here::here(csv_path)
    if (file.exists(csv_abs)) {
      df <- readr::read_csv(csv_abs, show_col_types = FALSE)
      df <- janitor::clean_names(df) %>%
        dplyr::mutate(dplyr::across(where(is.logical), as.character)) %>%
        tibble::as_tibble()
      return(df)
    }
  }

  path <- here::here(cfg$data$excel_path)
  sheet <- cfg$data$sheet
  # Read with a larger type-guessing window to reduce "Expecting logical ... got text" warnings.
  df <- readxl::read_excel(path, sheet = sheet, guess_max = 10000)
  # keep original names; also create cleaned version for easier matching
  df <- janitor::clean_names(df) %>%
    # If a column is guessed as logical but contains text later, keep values as character.
    dplyr::mutate(dplyr::across(where(is.logical), as.character)) %>%
    tibble::as_tibble()
  df
}
