library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "readxl", "readr", "dplyr", "stringr", "tidyr", "janitor",
    "yaml", "openxlsx", "purrr", "tibble"
  ),
  format = "rds"
)

source("R/rebuild/R/config.R")
source("R/rebuild/R/io.R")
source("R/rebuild/R/contract.R")
source("R/rebuild/R/transform.R")
source("R/rebuild/R/export.R")

list(
  tar_target(cfg, read_pipeline_config()),
  tar_target(raw_data, read_raw_survey(cfg$paths$raw_data_xlsx)),
  tar_target(contract_validated, apply_data_contract(raw_data, cfg)),
  tar_target(cleaned, normalize_key_fields(contract_validated)),
  tar_target(split_data, split_borrower_groups(cleaned)),
  tar_target(manifest, build_run_manifest(cleaned, split_data, cfg)),
  tar_target(artifact_registry, export_pipeline_outputs(cleaned, split_data, manifest, cfg))
)
