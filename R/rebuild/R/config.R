read_pipeline_config <- function(path = "R/rebuild/config/default.yaml") {
  cfg <- yaml::read_yaml(path)

  stopifnot(is.list(cfg), !is.null(cfg$paths), !is.null(cfg$contract), !is.null(cfg$exports))

  cfg
}

ensure_output_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}
