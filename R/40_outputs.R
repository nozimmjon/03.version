# 40_outputs.R
source(here::here("R/00_setup.R"))

save_gt <- function(tbl, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  gt::gtsave(tbl, path)
  invisible(path)
}
