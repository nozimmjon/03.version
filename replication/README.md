# ggplot2 reproduction of ClaudeCode charts

This folder reproduces Claude-generated charts using `ggplot2`.

## Data source
Both scripts use the cleaned dataset produced by **Data Preparation Pipeline v2**:
- `outputs_prep_v2/survey_master_cleaned_v2.csv`

## Scripts
- `recreate_claudecode_charts.R`: Reproduces **block 1** charts (01–08).
- `recreate_claudecode_block2_charts.R`: Reproduces **block 2** charts (09–16).
- `charts_ggplot2/`: Output folder for rendered charts.

## Visualization approach
- Replaces pie charts with bar-based alternatives where comparisons are clearer.
- Uses consistent color mapping for borrower vs non-borrower categories.
- Adds direct labels and percentage formatting where useful.
- Applies minimal, readable theming and reduced visual clutter.
- Sorts categorical bars to improve ranking comparisons.

## Run
```bash
Rscript replication/recreate_claudecode_charts.R
Rscript replication/recreate_claudecode_block2_charts.R
```

If your environment does not provide `Rscript`, run the scripts from an R session.
