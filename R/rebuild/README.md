# Professional R-Only Rebuild

This folder is a clean, R-first rebuild of the survey pipeline with reproducible data-management principles:

- **Single language for the canonical pipeline** (R only).
- **Data contract + validation** before analysis.
- **Deterministic workflow orchestration** with `targets`.
- **Versioned outputs + run manifest** for auditability.
- **Automated tests** for critical assumptions.

## Directory layout

- `config/default.yaml` — centralized paths and core variable definitions.
- `R/` — modular pipeline functions (I/O, validation, transformations, export).
- `_targets.R` — declarative pipeline graph.
- `scripts/run_pipeline.R` — simple entrypoint for analysts.
- `tests/testthat/` — unit tests for data-contract logic.
- `outputs/` — pipeline-generated artifacts.

## Quick start

```r
install.packages(c(
  "targets", "tarchetypes", "readxl", "readr", "dplyr", "stringr",
  "tidyr", "janitor", "yaml", "openxlsx", "purrr", "tibble", "testthat"
))
```

From repository root:

```bash
Rscript R/rebuild/scripts/run_pipeline.R
```

Run tests:

```bash
Rscript -e "testthat::test_dir('R/rebuild/tests/testthat')"
```

## Design principles

1. **Raw data is immutable.** The pipeline only reads from the raw input and writes new outputs.
2. **Validation happens early.** Required columns, value domains, and row-level expectations are checked before heavy processing.
3. **Transformations are pure where possible.** Core functions return data frames and avoid side effects.
4. **All outputs are reproducible.** Exported artifacts can be regenerated from code + config.
5. **Audit trails are explicit.** A run manifest captures timestamps, row counts, and output file locations.
