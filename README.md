# End-to-end pipeline documentation

This repository contains the full workflow used to move from the raw survey export to cleaned analytical datasets, QA logs, replicated tables/charts, and report-ready artifacts.

## 1) What this pipeline does

The project is organized in four stages:

1. **Data preparation (R)**
   - Loads the raw Excel export, removes exact duplicates, normalizes response fields, creates/validates the `has_loan` gate, applies skip-logic and cardinality checks, enforces value constraints, and exports cleaned outputs in CSV/BOM/XLSX formats.
2. **Cleaning audit (Python)**
   - Re-reads raw + cleaned outputs and performs consistency checks (row accounting, duplicate validation, dummy integrity, borrower splits, and other QA diagnostics).
3. **Replication artifacts (R)**
   - Recreates descriptive charts and publication-style tables from the cleaned `v2` master file.
4. **Legacy analysis/report scripts (Python)**
   - Contains block-level exploratory/modeling/report-generation scripts that produced earlier `claudecodes/` artifacts.

---

## 2) Repository map

- `dataset17022026.xlsx` — raw survey export input.
- `R/Data Preparation Pipeline v2.R` — **primary** cleaning pipeline.
- `R/Data Preparation Pipeline (Replicable).R` — earlier v1 cleaning version.
- `outputs_prep_v2/` — cleaned v2 datasets + QA logs + export manifest.
- `audit_cleaning.py` — post-cleaning QA/audit script.
- `replication/` — chart/table recreation scripts and plan/registry.
- `outputs/tables/replication/` — generated replication tables.
- `replication/charts_ggplot2/` — generated replication charts.
- `claudecodes/` — legacy Python analysis/chart/report scripts and generated figures.

---

## 3) Environment requirements

### R (data prep + replication)
Install packages:

```r
install.packages(c(
  "readxl", "dplyr", "stringr", "tidyr", "purrr", "janitor", "openxlsx",
  "glue", "lubridate", "readr", "yaml", "ggplot2", "scales", "forcats", "gt", "tibble"
))
```

### Python (audit + legacy scripts)
Install packages:

```bash
pip install pandas numpy openpyxl matplotlib python-docx scikit-learn
```

> Note: some Python scripts use local absolute Windows-style `base` paths; update those paths for your machine before running.

---

## 4) Recommended execution order (canonical)

Run everything from the repository root (`/workspace/03.version` in this environment).

### Step A — Run the v2 cleaning pipeline

```bash
Rscript "R/Data Preparation Pipeline v2.R"
```

This script writes the canonical cleaned files and logs into `outputs_prep_v2/`, including:

- `survey_master_cleaned_v2.*`
- `borrowers_clean_v2.*`
- `nonborrowers_clean_v2.*`
- duplicate/cardinality/constraint/age/QC logs (when non-empty)
- `question_registry_v2.*`, `matrix_block_flags_v2.*`, `matrix_long_audit_v2.*`
- `checkpoint_summary_v2.*`
- `EXPORT_MANIFEST_v2.xlsx`

### Step A.1 — Run codebook-driven raw validation (Phase 02)

```bash
Rscript R/02_validate_raw.R
```

This validates raw input against `codebook/codebook.csv` and writes QA artifacts to `outputs/validation_reports/`, including:

- `02_validate_raw_pointblank.html`
- `02_validate_raw_failures.csv` (if failing checks exist)
- `02_validate_raw_skip_failures.csv` (if skip-rule violations exist)
- `02_validate_raw_summary.csv`

### Step B — Run the cleaning audit

```bash
python audit_cleaning.py
```

This checks whether row counts and subset logic reconcile between raw and cleaned outputs and verifies key structural assumptions (e.g., dummy columns and borrower/non-borrower segmentation).

### Step C — Recreate charts and tables

```bash
Rscript replication/recreate_claudecode_charts.R
Rscript replication/recreate_claudecode_block2_charts.R
Rscript replication/recreate_claudecode_remaining_charts.R
Rscript replication/recreate_claudecode_tables.R
```

Outputs are written to:

- `replication/charts_ggplot2/` (PNG charts)
- `outputs/tables/replication/` (CSV + HTML tables)

### Step E — One-command full replication (recommended)

```bash
Rscript replication/run_full_replication.R
```

This orchestrates the full reproducibility chain in sequence:

1. `R/Data Preparation Pipeline v2.R`
2. `python audit_cleaning.py`
3. all three chart recreation scripts
4. `replication/recreate_claudecode_tables.R`

It also writes:

- `outputs/replication_run_log.txt`
- `outputs/replication_run_manifest.csv`

The manifest validates that key cleaned files, chart outputs, and table outputs were all generated.

### Step D — (Optional) Run legacy block/report scripts

These scripts under `claudecodes/` are useful for tracing historical analytical outputs, but are not required for the core reproducible v2 pipeline.

Example:

```bash
python claudecodes/block1_charts.py
python claudecodes/block2_analysis.py
python claudecodes/block2_charts.py
python claudecodes/block3_analysis.py
python claudecodes/block4_analysis.py
python claudecodes/block5_fixed.py
python claudecodes/block56_analysis.py
python claudecodes/block7_analysis.py
python claudecodes/generate_report_v6.py
```

---

## 5) Data contracts and key assumptions

- `has_loan` is the core split flag (`1 = borrower`, `0 = non-borrower`, possible `NA` when unresolved).
- Slash-encoded columns are treated carefully:
  - binary-like non-matrix dummy columns are normalized to 0/1;
  - matrix-structured columns (notably `2.1` and `3.3`) are excluded from blanket 0/1 conversion and audited separately.
- Borrower-only blocks are enforced via skip logic after `has_loan` creation.
- Cardinality rules are explicitly checked for selected blocks (`2.1`, `2.2`, `3.10`) and violations are logged.
- Numeric constraints (e.g., negative values where invalid) are repaired with audit trails in `constraint_fixes`.

---

## 6) How to validate a successful run

After a successful canonical run, verify:

1. `outputs_prep_v2/survey_master_cleaned_v2.csv` exists and is non-empty.
2. `outputs_prep_v2/checkpoint_summary_v2.csv` has expected metrics.
3. `outputs_prep_v2/EXPORT_MANIFEST_v2.xlsx` is present.
4. `outputs/tables/replication/` contains `table_01` through `table_13` (CSV and HTML where `gt` is available).
5. `replication/charts_ggplot2/` contains recreated chart PNG files.

Quick checks:

```bash
test -f outputs_prep_v2/survey_master_cleaned_v2.csv && echo "cleaned master OK"
test -f outputs_prep_v2/EXPORT_MANIFEST_v2.xlsx && echo "manifest OK"
```

---

## 7) Legacy vs canonical components

- **Canonical for reproducibility:**
  - `R/Data Preparation Pipeline v2.R`
  - `audit_cleaning.py`
  - scripts in `replication/`
- **Legacy / historical reference:**
  - `R/Data Preparation Pipeline (Replicable).R` (v1)
  - `claudecodes/*.py` scripts and associated image artifacts

If you are onboarding a new collaborator, start with the canonical path above and only use legacy scripts when you need to trace previously published/intermediate outputs.
