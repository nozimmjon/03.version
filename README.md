# Uzbekistan Credit Culture Survey — Reproducible R Pipeline

This repository contains the complete, end-to-end analytical pipeline for a nationwide survey on credit behaviour, financial responsibility, and NPL (non-performing loan) dynamics in Uzbekistan. The pipeline transforms a raw CAPI survey export into 42 publication-ready charts and a fully rendered analytical report. Every step is written in R; no Python is required.

------------------------------------------------------------------------

## Project context

The study surveyed **2,131 respondents** across all 14 regions of Uzbekistan on their borrowing behaviour, repayment history, financial literacy, attitudes toward debt, and experience with collection practices. Fieldwork was conducted by enumerators using a CAPI system that exported data in Excel format.

Key statistics used throughout the analysis:

| Metric                                   | Value               |
|------------------------------------------|---------------------|
| Total respondents                        | 2,131               |
| Active borrowers                         | 1,521 (71.4 %)      |
| Non-borrowers                            | 610 (28.6 %)        |
| NPL / delayed repayment rate             | \~52 % of borrowers |
| Logistic model AUC                       | 0.662               |
| Multi-debt share (2+ simultaneous loans) | 31.5 %              |
| Zero financial buffer                    | 36.2 %              |
| DSR ≥ 35 % (over-indebted threshold)     | \~33.7 %            |

------------------------------------------------------------------------

## Pipeline overview

The pipeline has eight sequential steps. Each step reads from the previous step's output so they must be run in order.

```         
dataset17022026.xlsx
        │
        ▼
  01_ingest.R       → 01_raw.rds  +  pii_table.rds  +  checksums.log
        │
        ▼
  02_validate_raw.R → 02_validate_raw.log  +  02_validate_raw_report.html
        │
        ▼
  03_clean.R        → 03_clean.rds  +  03_clean.log  +  03_multiselect_rename_log.csv
        │
        ▼
  04_construct.R    → 04_analytical.rds  +  borrowers.rds  +  non_borrowers.rds
        │               +  family_biz.rds  +  04_column_manifest.csv  +  04_construct.log
        │
        ├──▶ 05_export.R    → outputs/ CSV/XLSX files  +  05_export.log
        │
        ├──▶ 06_analyse.R   → outputs/tables/analysis/ (A1–G3 CSVs)  +  06_analyse.log
        │
        ├──▶ 07_visualise.R → outputs/figures/ (42 PNG charts)
        │
        └──▶ 08_report.qmd  → outputs/reports/08_report.pdf
```

Steps 05–08 all read from the same `04_analytical.rds` and can be re-run independently if you only need to refresh outputs.

------------------------------------------------------------------------

## Step-by-step script descriptions

### `R/01_ingest.R` — Raw ingestion and PII separation

**Input:** `dataset17022026.xlsx` (248 columns, all-text) **Output:** `data/intermediate/01_raw.rds`, `data/pii/pii_table.rds`

This script is the entry point. It performs the minimum necessary transformations to make the raw CAPI export safe and consistently structured for downstream processing.

What it does:

1.  **MD5 checksum** — hashes the raw Excel file and compares against `logs/checksums.log`. Warns if the file has changed since the last run, protecting against accidental data modification.

2.  **Read raw Excel** — loads all columns as plain text (`col_types = "text"`) using `read_excel()` with UTF-8 locale. All type-casting is deliberately deferred to `03_clean.R` to avoid silent coercion errors.

3.  **False-split column detection** — the CAPI system exported some single-value questions (e.g., `n_loans`, `financial_buffer`, `bank_explained_terms`) as dummy-style sub-columns because their question text contained a "/" character. These are flagged and verified present.

4.  **Admin `age` collision fix** — the raw file contains two age-related columns: an admin pre-filled age (col 15) and the respondent's self-reported age from Q1.1 (col 19). The admin column is renamed to `age_admin` before applying codebook renames.

5.  **Codebook-driven rename** — maps raw CAPI column headers to clean `var_name` identifiers from `codebook/codebook.csv`. All 248 columns are retained; only those with codebook entries are renamed.

6.  **Stable `row_id`** — adds an integer row key before any sorting or filtering. This key is preserved through all downstream steps for audit traceability.

7.  **Exact duplicate removal** — drops 8 fully identical rows (identical on all columns except `row_id`).

8.  **PII separation** — moves respondent name, phone number, and date of birth into a separate `data/pii/pii_table.rds` file that must not be shared externally. The analytic dataset retains only `row_id` as a linkage key.

------------------------------------------------------------------------

### `R/02_validate_raw.R` — Pre-cleaning quality checks

**Input:** `data/intermediate/01_raw.rds` **Output:** `logs/02_validate_raw_report.html`, `logs/02_validate_raw.log`

Runs a structured battery of data-quality checks against the raw (pre-clean) dataset using the `{pointblank}` package, then writes a timestamped log summarising pass/fail counts.

Validation layers:

| Layer | What is checked |
|------------------------------------|------------------------------------|
| A. Coverage | All codebook `var_name` entries are present in the raw dataset |
| B. Completeness | Variables marked `always` in `skip_condition` must have no NA values |
| C. Set membership | Single-choice and binary responses fall within the codebook-defined option set |
| D. Range bounds | Numeric variables (age, household size, etc.) are within `valid_min`/`valid_max` |
| E. Cross-column | `hh_workers ≤ hh_size`; `children_under18 < hh_size` |
| F. Consent gate | All rows have `consent == "yes"` (no refusals in analytic dataset) |
| G. Skip-logic | Borrower-block variables are NA for respondents who said they have no loan |

The HTML report produced by `pointblank` provides an interactive row-level drill-down for any failed checks.

------------------------------------------------------------------------

### `R/03_clean.R` — Type-casting, expansion, and derived flags

**Input:** `data/intermediate/01_raw.rds` **Output:** `data/intermediate/03_clean.rds`, `logs/03_multiselect_rename_log.csv`

The core cleaning script. Transforms the all-text ingested data into properly typed, analysis-ready columns.

Processing steps:

1.  **Helper functions** — `norm_text()` trims whitespace and converts blank/null-like strings to `NA`; `to01()` converts any truthy representation (Uzbek "Ҳа"/"Йўқ", "1"/"0", `TRUE`/`FALSE`) to strict `0L`/`1L` integers.

2.  **Open-text normalisation** — all free-text fields are trimmed and normalised via `norm_text()`.

3.  **Numeric type-casting** — codebook columns with `q_type == "numeric"` are converted with `as.numeric()`; coercion failures are captured as `NA`.

4.  **Multi-select dummy expansion** — multi-select questions in CAPI are exported as one dummy column per response option. Each dummy is converted to `0L`/`1L` and renamed `{var_name}__{option_code}` using positional mapping from the codebook. Falls back to a slugified Uzbek label when option counts do not match. The rename log is written to `logs/03_multiselect_rename_log.csv`.

5.  **Matrix block processing** — Q2.1 (credit sources used), Q2.3 (maximum loan willingness by source), and Q3.3 (collection methods) are structured as row × column matrix blocks in CAPI. These are converted to `0L`/`1L` integer flags (Q2.1, Q3.3) or numeric mln-UZS midpoints (Q2.3 via `q23_to_mln()`).

6.  **Derived variable construction** — the following analytical flags are built from the cleaned columns:

    | Variable | Definition |
    |------------------------------------|------------------------------------|
    | `has_loan` | `1` if `has_loan_type != "no"`, else `0` |
    | `is_npl` | `1` if `repayment_status == "Тўламаётган"` (NPL) |
    | `is_delayed` | `1` if `repayment_status == "Кечикиш билан"` (delayed) |
    | `dsr_midpoint` | Numeric midpoint of `dsr_cat` category in % |
    | `income_midpoint_mln_uzs` | Numeric midpoint of `hh_income_cat` in mln UZS |
    | `multi_debt_flag` | `1` if `n_loans ≥ 2` |
    | `productive_use_flag` | `1` if loan purpose is productive (business/agriculture) |
    | `over_indebted_flag` | `1` if DSR midpoint ≥ 35 % |
    | `fin_lit_score` | Sum of financial literacy indicator dummies |
    | `delay_tolerance_score` | Numeric score from `acceptable_delay_days` |
    | `credit_formality` | `"formal"` / `"informal"` derived from `credit_source_primary` |
    | `is_straw_borrower` | `1` if `used_straw_borrower == "yes"` |

------------------------------------------------------------------------

### `R/04_construct.R` — Ordered factors, labels, and analytical splits

**Input:** `data/intermediate/03_clean.rds` **Output:** `data/intermediate/04_analytical.rds`, `data/analytical/` subsets

Finalises the analytical dataset by applying factor ordering and creating convenience subsets.

What it does:

1.  **Ordered factors** — all ordinal variables receive explicit level ordering aligned with the questionnaire sequence. This ensures tables and ggplot2 charts always display categories in the correct order without any manual `fct_relevel()` calls in downstream scripts. Variables ordered include: `hh_income_cat`, `dsr_cat`, `financial_buffer`, `income_variability`, `repayment_status`, `education`, `employment`, and others.

2.  **Analytical flags** — `repay_group` (`"NPL"` / `"Delay"` / `"On-time"`) and `employed_binary` are added as convenience labels for cross-tabulation and chart labelling.

3.  **Score winsorising** — extreme outliers in `fin_lit_score` and `dsr_midpoint` are capped at the 99th percentile to prevent distortion in regression and visualisation.

4.  **Analytical subsets** — three subsets are written to `data/analytical/`:

    -   `borrowers.rds` — active borrowers only (`has_loan == 1`, n ≈ 1,521)
    -   `non_borrowers.rds` — non-borrowers (`has_loan == 0`, n ≈ 610)
    -   `family_biz.rds` — borrowers whose primary loan purpose relates to family business or informal enterprise

5.  **Column manifest** — a full column inventory including type, class, and non-NA count is written to `logs/04_column_manifest.csv`.

------------------------------------------------------------------------

### `R/05_export.R` — Cleaned dataset exports

**Input:** `data/intermediate/04_analytical.rds` **Output:** `outputs/` CSV and XLSX files

Writes the analytical dataset and the three subsets to `outputs/` in both plain CSV and XLSX format for use by team members who prefer spreadsheet tools. Also writes a UTF-8 BOM variant for Excel compatibility on Windows (not needed for R/Python users).

------------------------------------------------------------------------

### `R/06_analyse.R` — Descriptive statistics and logistic regression

**Input:** `data/intermediate/04_analytical.rds` **Output:** `outputs/tables/analysis/` (13 CSV files), `logs/06_analyse.log`

Produces all tabular analytical outputs. Organised in seven sections:

| Section | Tables produced | Description |
|------------------------|------------------------|------------------------|
| A. Sample | A1 (region), A2 (totals) | Regional distribution, borrower/non-borrower split |
| B. Demographics | B1–B6 | Age, gender, education, employment, income, household size |
| C. Credit profile | C1–C6 | Credit type, repayment status, DSR, financial buffer, income volatility, special flags |
| D. Financial literacy | D1–D5 | Fin-lit score distribution, contract reading, delay tolerance — all by repayment status |
| E. Cross-tabs | E1–E2 | Key categorical variables × NPL/delay status; numeric means by NPL |
| F. Logistic model | F1–F2 | GLM coefficients (broom-tidy), model fit (AUC, accuracy, Nagelkerke R²) |
| G. Family enterprise | G1–G3 | Family-loan borrowers vs others; NPL by credit type; three-way NPL × type × income |

The logistic regression in section F predicts NPL status (`is_npl`) from: DSR category, income volatility, financial buffer, multi-debt flag, productive use flag, fin-lit score, over-indebted flag, and loan channel. AUC is computed with `{pROC}`; coefficients with `{broom}`.

------------------------------------------------------------------------

### `R/07_visualise.R` — 42 ggplot2 charts

**Input:** `data/intermediate/04_analytical.rds` **Output:** `outputs/figures/chart_01_*.png` … `chart_42_*.png`

Produces all charts used in the report and in the team's presentation. All charts share a consistent visual style:

-   Theme: `theme_minimal(base_size = 14)` with a left-aligned bold title
-   Colour: `RColorBrewer` palettes via `expand_palette()` helper (handles palette overflow when number of categories exceeds palette maximum)
-   Labels: Uzbek (Cyrillic) throughout, matching the source questionnaire
-   Output: PNG at 160 dpi, 8 × 5.5 inches

Charts are grouped by analytical theme:

| Charts | Theme |
|------------------------------------|------------------------------------|
| 01–08 | Sample composition: region, age, gender, income, credit type, repayment status, income sources, education |
| 09–16 | Credit decision-making: primary source, reasons for informal/formal credit, actual sources used, loan purposes, purpose × status, decision factors |
| 17–19 | Financial health: DSR × status, income volatility × status, self-reported NPL causes |
| 20–23 | Financial literacy & contracts: contract read × status, terms understood × status, loan regret × status, online vs offline behaviour |
| 24–25 | NPL risk model: logistic coefficients, odds ratio forest plot |
| 26–32 | Collection and reminders: collection methods × status, effective reminders, ROC curve |
| 27–30 | Family entrepreneurship: family-loan NPL reasons, family support, NPL by credit type, why family credit |
| 33–38 | Attitudes and norms: financial literacy × status, credit score awareness, delay effect knowledge, repayment priority, acceptable delay, peer repayment perception |
| 39–42 | Supplementary (added for team report enrichment): informal credit by region, Q2.3 max loan willingness by source, financial buffer distribution, NPL by productive use |

------------------------------------------------------------------------

### `08_report.qmd` — Quarto analytical report

**Input:** `data/intermediate/04_analytical.rds` + `outputs/figures/*.png` **Output:** `outputs/reports/08_report.pdf` **Config:** `_quarto.yml`

A fully reproducible Quarto document that assembles all charts, re-estimates the logistic model inline, and writes the full analytical narrative in English prose. Sections:

1.  Executive Summary
2.  Introduction and Methodology
3.  Sample Demographics
4.  Credit Participation
5.  Credit Decision-Making
6.  Financial Health Indicators
7.  Financial Literacy and Contract Behaviour
8.  NPL Risk Model
9.  Collection and Reminder Methods
10. Family Entrepreneurship and Informal Lending
11. Attitudes and Social Norms
12. Conclusions and Policy Recommendations
13. Appendices A–D (methodology, regression tables, model diagnostics, supplementary charts)

The report uses `{flextable}` for formatted tables and `knitr::include_graphics()` to embed pre-generated PNG charts. Inline R expressions (`r N_total`, `r pct_npl`, etc.) ensure all statistics are computed from the live dataset, not hard-coded.

------------------------------------------------------------------------

## Folder structure

```         
03.version/
│
├── dataset17022026.xlsx          ← RAW INPUT (required to run from scratch)
│
├── codebook/
│   ├── codebook.csv              ← canonical variable dictionary (248 vars)
│   └── value_labels.csv          ← factor level lookup used by 02 & 03
│
├── R/
│   ├── 01_ingest.R
│   ├── 02_validate_raw.R
│   ├── 03_clean.R
│   ├── 04_construct.R
│   ├── 05_export.R
│   ├── 06_analyse.R
│   └── 07_visualise.R
│
├── _quarto.yml                   ← Quarto project config (PDF, TOC, figure sizes)
├── 08_report.qmd                 ← Quarto report source
│
├── logs/                         ← auto-generated pipeline audit trail
│   ├── checksums.log             ← MD5 of raw Excel (written by 01)
│   ├── 02_validate_raw.log       ← pointblank pass/fail counts (written by 02)
│   ├── 02_validate_raw_report.html ← interactive pointblank report (written by 02)
│   ├── 03_clean.log              ← cleaning summary (written by 03)
│   ├── 03_multiselect_rename_log.csv ← dummy column rename map (written by 03)
│   ├── 04_column_manifest.csv    ← full column inventory (written by 04)
│   ├── 04_construct.log          ← construct summary + row counts (written by 04)
│   ├── 05_export.log             ← export manifest (written by 05)
│   └── 06_analyse.log            ← analysis run record (written by 06)
│
├── data/
│   ├── intermediate/             ← RDS snapshots (regenerable; skip re-run)
│   │   ├── 01_raw.rds            ← output of 01_ingest.R
│   │   ├── 03_clean.rds          ← output of 03_clean.R
│   │   └── 04_analytical.rds     ← output of 04_construct.R (main input for 06–08)
│   └── analytical/               ← convenience subsets (output of 04_construct.R)
│       ├── borrowers.rds
│       ├── non_borrowers.rds
│       └── family_biz.rds
│
├── outputs/
│   ├── figures/                  ← 42 PNG charts (output of 07_visualise.R)
│   ├── tables/
│   │   ├── analysis/             ← A1–G3 descriptive/model CSVs (output of 06)
│   │   └── replication/          ← table_01–table_13 CSV + HTML (output of 06)
│   ├── reports/
│   │   └── 08_report.pdf         ← rendered report (output of quarto render)
│   ├── survey_master.xlsx        ← full cleaned dataset (output of 05)
│   ├── borrowers.xlsx
│   ├── non_borrowers.xlsx
│   └── family_biz.xlsx
│
├── uzb_credit_survey_r_pipeline.Rproj
├── .gitignore
└── README.md
```

------------------------------------------------------------------------

## Environment requirements

Tested on **R ≥ 4.3** and **Quarto ≥ 1.4**.

Install all R packages in one call:

``` r
install.packages(c(
  # core pipeline
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "forcats",
  "lubridate", "janitor", "glue", "readr", "here", "digest", "stringi",
  # validation
  "pointblank",
  # export
  "openxlsx", "writexl",
  # analysis
  "broom", "pROC",
  # visualisation
  "ggplot2", "scales", "RColorBrewer", "ggrepel",
  # report
  "knitr", "flextable", "quarto"
))
```

For the PDF report, also install a LaTeX distribution:

``` r
# Lightweight option (recommended)
install.packages("tinytex")
tinytex::install_tinytex()
```

------------------------------------------------------------------------

## How to run

Open `uzb_credit_survey_r_pipeline.Rproj` in RStudio. The working directory will be set to `03.version/` automatically. Then run scripts in order:

``` r
source("R/01_ingest.R")
source("R/02_validate_raw.R")
source("R/03_clean.R")
source("R/04_construct.R")
source("R/05_export.R")
source("R/06_analyse.R")
source("R/07_visualise.R")
```

Then render the report:

``` bash
quarto render 08_report.qmd
```

The PDF lands in `outputs/reports/08_report.pdf`.

> **Shortcut:** If `data/intermediate/04_analytical.rds` already exists, you can skip steps 01–05 and run only `06_analyse.R`, `07_visualise.R`, and the Quarto render. The analytical RDS is the single gateway into all reporting.

------------------------------------------------------------------------

## Validation checklist

After a full run, confirm these outputs exist:

| Check | Expected result |
|------------------------------------|------------------------------------|
| `data/intermediate/04_analytical.rds` | n = 2,131 rows |
| `outputs/figures/` | 42 PNG files: `chart_01_*` … `chart_42_*` |
| `outputs/tables/analysis/` | 13 CSV files: A1, A2, B1–B6, C1–C6, D1–D5, E1–E2, F1–F2, G1–G3 |
| `outputs/reports/08_report.pdf` | Non-empty PDF |
| `logs/04_construct.log` | Shows n = 2,131 |

------------------------------------------------------------------------

## Key data contracts

Understanding these conventions is important if you modify or extend the pipeline.

-   **`has_loan`** — the primary split flag. `1` = active borrower; `0` = non-borrower. Derived in `03_clean.R` from `has_loan_type != "no"`.
-   **Borrower-only blocks** — variables from Q2.x (credit behaviour), Q3.x (collection), and Q4.x (financial literacy details) are only asked to borrowers. They will be `NA` for all non-borrowers. Skip-logic is enforced in `03_clean.R` and verified in `02_validate_raw.R`.
-   **`repayment_status`** — three-level factor: `"Ўз вақтида"` (on-time) / `"Кечикиш билан"` (delayed) / `"Тўламаётган"` (NPL). Ordered in `04_construct.R`. The derived flags `is_npl` and `is_delayed` are `0L`/`1L` versions.
-   **`financial_buffer`** — ordered factor with 6 levels (none → \>6 months). Level 1 (`"умуман эга эмасман"`) = zero savings buffer, affecting 36.2 % of borrowers.
-   **`dsr_cat`** — self-reported debt-service ratio category. `dsr_midpoint` is the numeric midpoint in percent. Over-indebted threshold: ≥ 35 %.
-   **Q2.3 columns** — named `{n}-2.3` in the raw data. After `q23_to_mln()` in `03_clean.R` these hold the respondent's maximum willingness to borrow from each source, in million UZS. Source order: bank, family, formal instalment, MFI, informal instalment, pawnshop, street lender.
-   **Multi-select dummies** — named `{var_name}__{option_code}`. Always `0L`/`1L`. Never `NA` after cleaning (missing = not selected = `0`).
-   **Chart labels** — all in Uzbek (Cyrillic), matching the questionnaire language. Report prose is in English.

------------------------------------------------------------------------

## Files to delete before sharing

### Entire folders — delete

| Folder | Reason |
|------------------------------------|------------------------------------|
| `claudecodes/` | Old Python analysis scripts and early-iteration PNG charts (38 files); fully superseded by `07_visualise.R` |
| `replication/` | Old ggplot2 chart-recreation scripts and `charts_ggplot2/` PNGs; superseded |
| `outputs_prep_v2/` | Python-era cleaning outputs (\~30 files in triple CSV/BOM/XLSX format); superseded by `R/01–05` |
| `outputs/reports/outputs/` | Accidental nested duplicate of `outputs/figures/` inside the reports folder |
| `outputs/validation_reports/` | Old Python-era validation logs; superseded by `logs/` |
| `_targets/` | Unused `{targets}` workflow cache; not part of this pipeline |
| `.Rproj.user/` | Personal RStudio UI state; machine-specific, should not be shared |
| `.quarto/` | Quarto render cache; regenerated automatically on next render |
| `data/pii/` | **⚠ Contains PII (names, phone numbers, DOB) — must never be shared** |

### Individual files — delete

| File | Reason |
|------------------------------------|------------------------------------|
| `R/Data Preparation Pipeline v2.R` | Old monolithic cleaning script; superseded by `R/01–05` |
| `R/Data Preparation Pipeline (Replicable).R` | Earlier v1 cleaning script; superseded |
| `audit_cleaning.py` | Python cleaning audit; superseded by `R/02_validate_raw.R` |
| `~$dataset17022026.xlsx` | Excel temporary lock file (created when file is open) |
| `.Rhistory` | Personal R session command history |
| `codebook/codebook_bom_tmp.csv` | Temporary BOM-encoded artifact from earlier session |
| `codebook/codebook_ORIGINAL.csv` | Pre-correction version of the codebook |
| `codebook/codebook_CORRECTED.csv` | Intermediate correction stage; superseded by `codebook.csv` |
| `codebook/codebook_v2.csv` | Intermediate version; superseded by `codebook.csv` |
| `codebook/README.md` | Old codebook-only readme; replaced by this file |
| `Доклад_катта-28.docx` | Old team report draft (version 28) |
| `Доклад_катта-30.docx` | Old team report draft (version 30) |
| `outputs/borrowers_UTF8_BOM.csv` | BOM variant; keep `.xlsx` or plain `.csv` instead |
| `outputs/non_borrowers_UTF8_BOM.csv` | BOM variant |
| `outputs/family_biz_UTF8_BOM.csv` | BOM variant |
| `outputs/survey_master_UTF8_BOM.csv` | BOM variant |
| `outputs/EXPORT_MANIFEST.xlsx` | Auto-generated; recreated on next run of `05_export.R` |

### Keep or delete — your call

| File | Notes |
|------------------------------------|------------------------------------|
| `Доклад_катта-32.docx` | Current team report; not part of the pipeline but useful as reference context |
| `Саволнома_final.docx` | Survey questionnaire; useful for understanding variable definitions |

------------------------------------------------------------------------

## Chart index

All charts are saved to `outputs/figures/` at 160 dpi, 8 × 5.5 inches.

| \# | Filename | Topic |
|------------------------|------------------------|------------------------|
| 01 | `chart_01_regional_distribution.png` | Sample by region (n per region) |
| 02 | `chart_02_age_distribution.png` | Age distribution of respondents |
| 03 | `chart_03_gender.png` | Gender split |
| 04 | `chart_04_income.png` | Household income category distribution |
| 05 | `chart_05_credit_type.png` | Mix of credit types held |
| 06 | `chart_06_repayment_status.png` | Repayment status breakdown |
| 07 | `chart_07_income_sources.png` | Income source breakdown (multi-select) |
| 08 | `chart_08_education.png` | Education level distribution |
| 09 | `chart_09_primary_credit_source.png` | Primary credit source preference |
| 10 | `chart_10_informal_reasons.png` | Why respondents prefer informal credit |
| 11 | `chart_11_bank_reasons.png` | Why respondents prefer formal (bank) credit |
| 12 | `chart_12_no_credit_reasons.png` | Reasons for not borrowing at all |
| 13 | `chart_13_actual_credit_sources.png` | Actual credit sources currently used |
| 14 | `chart_14_loan_purposes.png` | Purpose of most recent loan |
| 15 | `chart_15_purpose_by_status.png` | Loan purpose × repayment status |
| 16 | `chart_16_decision_factors.png` | Factors influencing loan decision |
| 17 | `chart_17_dti_by_status.png` | Debt-service ratio category × repayment status |
| 18 | `chart_18_volatility_by_status.png` | Income volatility × repayment status |
| 19 | `chart_19_npl_reasons.png` | Self-reported causes of non-payment |
| 20 | `chart_20_contract_by_status.png` | Contract read before signing × repayment status |
| 21 | `chart_21_terms_by_status.png` | Loan terms understood × repayment status |
| 22 | `chart_22_regret_by_status.png` | Loan regret × repayment status |
| 23 | `chart_23_online_vs_offline.png` | Online vs offline loan-taking behaviour |
| 24 | `chart_24_logistic_coef.png` | Logistic regression coefficients (NPL model) |
| 25 | `chart_25_collection_by_status.png` | Collection methods received × repayment status |
| 26 | `chart_26_effective_reminders.png` | Reminder methods rated as effective by borrowers |
| 27 | `chart_27_family_npl_reasons.png` | Reasons for NPL on family loans |
| 28 | `chart_28_family_support.png` | Willingness of family to provide financial support |
| 29 | `chart_29_npl_by_credit_type.png` | NPL rate by credit type |
| 30 | `chart_30_why_family_credit.png` | Reasons for borrowing from family rather than bank |
| 31 | `chart_31_odds_ratios.png` | Odds ratio forest plot (logistic model) |
| 32 | `chart_32_roc_curve.png` | ROC curve (AUC = 0.662) |
| 33 | `chart_33_finlit_by_status.png` | Financial literacy score × repayment status |
| 34 | `chart_34_credit_score_motivates.png` | Credit score as motivation to repay |
| 35 | `chart_35_knows_delay_effect.png` | Knowledge of consequences of payment delays |
| 36 | `chart_36_repayment_priority.png` | Loan repayment ranking among monthly priorities |
| 37 | `chart_37_acceptable_delay.png` | Acceptable number of days of payment delay |
| 38 | `chart_38_peers_repay_perception.png` | Perception of peers' repayment behaviour |
| 39 | `chart_39_informal_by_region.png` | Share of informal credit by region (14 regions) |
| 40 | `chart_40_q23_loan_size_by_source.png` | Maximum loan willingness by credit source (Q2.3) |
| 41 | `chart_41_financial_buffer.png` | Financial buffer distribution (36.2 % have zero) |
| 42 | `chart_42_npl_by_productive_use.png` | NPL rate by productive vs non-productive loan use |
