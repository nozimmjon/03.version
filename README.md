# Uzbekistan credit survey — reproducible R pipeline

This project builds a fully reproducible analysis pipeline (tables, figures, and models) from the cleaned survey Excel file.

## Quick start

1. Open this folder as an RStudio Project (optional).
2. Install required packages (once):

```r
install.packages(c(
  "targets","tarchetypes","here","yaml","dplyr","tidyr","stringr","readxl","janitor","purrr",
  "ggplot2","scales","glue","gtsummary","gt","tidymodels","broom"
))
```

3. Build everything (tables, figures, models) via **targets**:

```r
targets::tar_make()
```

4. Render the report:

```r
quarto::quarto_render("reports/report.qmd")
```

Outputs:
- Figures: `outputs/figures/`
- Tables: `outputs/tables/`
- Rendered report: `outputs/report/`

## Data
- Input Excel is stored at `data/raw/survey_master_cleaned_v1.xlsx` and is **never modified**.
- The pipeline writes an analysis-ready dataset to `data/processed/analysis_data.rds`.

## Notes
- Column mappings are in `reports/config.yml`. You can adjust them if the questionnaire wording changes.
- The pipeline is designed to be robust: if a mapped column is missing, it fails loudly with a clear message.

## Optional: renv
If you want locked package versions, you can use `renv`. This repository includes a minimal `renv.lock` (with an R section) so that `renv::init()` / `renv::snapshot()` works cleanly in your environment.
