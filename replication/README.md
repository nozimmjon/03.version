# ggplot2 reproduction of ClaudeCode charts

This folder reproduces the **first chart block (charts 01–08)** from `claudecodes/block1_charts.py` using `ggplot2`.

## Data source
This script uses the cleaned dataset produced by **Data Preparation Pipeline v2**:
- `outputs_prep_v2/survey_master_cleaned_v2.csv`

## Files
- `recreate_claudecode_charts.R`: Reads the v2 cleaned dataset and exports ggplot2 charts.
- `charts_ggplot2/`: Output folder for rendered charts.

## Why these are improved visualizations
- Replaces pie charts with bars/100% stacked bars for easier comparison.
- Uses consistent color mapping for borrower vs non-borrower categories.
- Uses direct labels and percentages where helpful.
- Applies minimal, readable theming and decluttered grids.
- Sorts categorical bars to support ranking comparisons.

## Run
```bash
Rscript replication/recreate_claudecode_charts.R
```

If your environment does not provide `Rscript`, run the script from an R session.
