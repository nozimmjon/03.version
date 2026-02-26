#!/usr/bin/env Rscript

required <- c("targets")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing required packages: ", paste(missing, collapse = ", "))
}

targets::tar_make(script = "R/rebuild/_targets.R")
cat("Pipeline completed. Artifacts written to R/rebuild/outputs\n")
