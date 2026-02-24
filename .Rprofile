# Optional renv activation.
# By default, we *do not* auto-activate renv to avoid "out-of-sync" noise.
# To use renv, set an environment variable before starting R:
#   Sys.setenv(USE_RENV = "1")
if (interactive() && identical(Sys.getenv("USE_RENV"), "1") &&
    requireNamespace("renv", quietly = TRUE) && file.exists("renv.lock")) {
  try(renv::activate(), silent = TRUE)
}
