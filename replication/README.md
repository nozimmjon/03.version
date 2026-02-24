# ggplot2 reproduction of ClaudeCode charts

This folder reproduces Claude-generated charts using `ggplot2` from the cleaned v2 pipeline dataset.

## Data source
All scripts use:
- `outputs_prep_v2/survey_master_cleaned_v2.csv`

## Scripts
- `recreate_claudecode_charts.R`: Block 1 charts (01–08).
- `recreate_claudecode_block2_charts.R`: Block 2 charts (09–16).
- `recreate_claudecode_remaining_charts.R`: Remaining descriptive charts (17–23, 25–30, 33–38).
- `charts_ggplot2/`: Output folder.

## Note on model-driven charts
Charts `24`, `31`, and `32` are model-driven (logistic coefficients/odds ratios/ROC) and are produced by separate modeling scripts in `claudecodes/`.

## Run
```bash
Rscript replication/recreate_claudecode_charts.R
Rscript replication/recreate_claudecode_block2_charts.R
Rscript replication/recreate_claudecode_remaining_charts.R
```

If `Rscript` is unavailable, run from an interactive R session.
