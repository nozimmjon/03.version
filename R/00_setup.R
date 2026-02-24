# 00_setup.R
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en_US.UTF-8")

library(here)
library(dplyr)
library(stringr)
library(readxl)
library(janitor)
library(ggplot2)
library(scales)
library(glue)

theme_report <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 6)),
      plot.caption = element_text(size = rel(0.85), margin = margin(t = 8)),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
}

save_plot <- function(p, path, width = 7, height = 4.2, dpi = 320) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(path)
}

assert_has_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) stop(glue("Missing required columns: {paste(miss, collapse = ', ')}"), call. = FALSE)
  invisible(TRUE)
}
