# Codebook — CBU Household Credit Survey 2025

This folder contains the machine-readable codebook for the Central Bank of Uzbekistan
Household Credit Survey. These files are the **single source of truth** for all variable
definitions, skip logic, valid ranges, and analytic roles. All cleaning and construction
scripts must read from here — no questionnaire knowledge is hard-coded in R scripts.

---

## Files

| File | Purpose |
|---|---|
| `codebook.csv` | One row per question/variable. Defines type, skip logic, valid ranges, analytic role. |
| `value_labels.csv` | One row per response option. Maps every code to its Uzbek and English label. |
| `README.md` | This file. |

---

## `codebook.csv` — Column Schema

| Column | Description |
|---|---|
| `q_number` | Questionnaire numbering (e.g. `1.1`, `2.4`, `admin_a`). `DERIVED` for constructed variables. |
| `var_name` | Clean R-safe snake_case variable name used in all scripts. |
| `q_text_uz` | Abbreviated question text in Uzbek. |
| `section` | `admin` / `block1` / `block2` / `block3` / `derived` |
| `q_type` | `single_choice` / `multi_select` / `numeric` / `matrix` / `open_text` / `binary` |
| `response_options` | Pipe-separated (`\|`) list of valid option codes. For matrices: `rows: ... ; cols: ...` format. Empty for numeric/open_text. |
| `valid_min` | Minimum valid value (numeric variables only). |
| `valid_max` | Maximum valid value (numeric variables only). |
| `skip_condition` | Plain-English R-like condition describing when this question is shown. `always` = shown to all respondents. Use `has_loan_type != 'no'` for borrower-only questions. |
| `analytic_role` | See roles table below. |
| `notes` | Researcher decisions, construction rules, cross-checks, and pipeline warnings. |

### Analytic Roles

| Role | Meaning |
|---|---|
| `admin_metadata` | Pre-filled system or administrative field — NOT a respondent answer. |
| `identifier` | PII field (name, phone, ID). Separate from analytic dataset immediately on ingestion. |
| `gate` | Controls routing / sample splits. `has_loan_type` is the primary gate. |
| `demographic` | Socio-demographic covariate (Block 1). |
| `covariate` | Explanatory variable used in regression models. |
| `primary_outcome` | Outcome variable listed in the Pre-Analysis Plan. |
| `secondary_outcome` | Exploratory outcome, not pre-registered. |
| `attitudinal` | Belief/norm/attitude indicator (not a behavioral fact). |
| `financial_literacy` | Component of the financial literacy composite index. |
| `open_text` | Free-text response requiring thematic coding before analysis. |

---

## `value_labels.csv` — Column Schema

| Column | Description |
|---|---|
| `var_name` | Variable name from `codebook.csv`. For matrix row/col labels: `<var>_row` and `<var>_col`. |
| `code` | The code value stored in the dataset (after cleaning). |
| `label_uz` | Full Uzbek label from questionnaire. |
| `label_en` | English translation. |
| `sort_order` | Display order for tables and charts (matches questionnaire order). |

---

## Key Design Decisions (Researcher Judgments)

The following decisions are documented here so they are explicit and reviewable:

### 1. `has_loan_type` is 4-category, not binary
Q2.4 has three "Yes" options that must be preserved:
- `own_for_self` — standard borrower
- `own_for_others` — straw borrower (registered to respondent, used by someone else)
- `others_for_self` — name-lending (registered to another, used by respondent)

The derived `has_loan` binary collapses all three for backward compatibility only.
**Always use `has_loan_type` for any NPL or risk analysis.**

### 2. Admin metadata is separate from questionnaire data
Fields `holat1`, `credit_type_admin`, `obs_type`, `list_type`, `resp_id` are pre-filled
from bank records or CAPI system — they are not respondent answers. They must be managed
in a separate administrative table and joined to the survey data by `row_id` only when needed.

### 3. Financial literacy score components (Q3.4 + Q3.5 + Q3.13 + Q3.14)
Binary scoring:
- `contract_read`: `yes_fully` = 1, all others = 0
- `terms_understood`: `yes` = 1, all others = 0
- `credit_score_motivates`: any value except `not_aware_of_credit_score` = 1
- `knows_delay_effect`: `yes_clearly` or `partly` = 1, `no` or `hard_to_say` = 0

Score is NA if any component is missing. Do not impute.

### 4. Income midpoints (million UZS)
| Category | Midpoint |
|---|---|
| `lt5m` | 2.5 |
| `5to10m` | 7.5 |
| `10to20m` | 15.0 |
| `20to50m` | 35.0 |
| `gt50m` | 75.0 |
| `hard_to_say` | NA |

### 5. DSR midpoints
| Category | Midpoint |
|---|---|
| `lt10pct` | 0.05 |
| `10to20pct` | 0.15 |
| `20to35pct` | 0.275 |
| `35to50pct` | 0.425 |
| `gt50pct` | 0.625 |
| `hard_to_say` | NA |

DSR ≥ 0.50 = over-indebtedness threshold per CBU standards.

### 6. Matrix questions (Q2.1, Q2.3, Q3.3) are excluded from blanket 0/1 conversion
These blocks have categorical response labels as column suffixes, not simple Yes/No.
They must be processed separately using the matrix row/col structure in `value_labels.csv`.

### 7. Q3.7 is shown to ALL borrowers, not only NPL
Despite asking "why couldn't you repay", options include "I always pay on time" and
"I never had credit" — confirming this is an attitudinal/factual mixed question for all
borrowers. Do not restrict denominator to NPL borrowers.

### 8. Numbering discrepancy: Q3.12.1 vs Q3.11 in CAPI export
The questionnaire numbers collection methods as Q3.12.1, but the CAPI export uses Q3.11.
This is a known instrument numbering error. `var_name = collection_methods_received`
is canonical; do not rely on numbering for column matching.

### 9. PII fields
`phone_number` (Q3.17), `resp_id` (admin_e), `resp_name_add` (admin_j),
and respondent name fields are PII. They must be:
1. Separated from the analytic dataset on first ingestion
2. Stored in an encrypted, separately-permissioned file
3. Linked only via `row_id`
4. Never included in any exported analytic output

---

## How to Use in R

```r
library(readr)
library(dplyr)

cb <- read_csv(here::here("codebook/codebook.csv"))
vl <- read_csv(here::here("codebook/value_labels.csv"))

# Get all borrower-only variables
borrower_only_vars <- cb |>
  filter(str_detect(skip_condition, "has_loan_type != 'no'")) |>
  pull(var_name)

# Get all numeric variables with valid ranges
numeric_constraints <- cb |>
  filter(q_type == "numeric", !is.na(valid_min) | !is.na(valid_max)) |>
  select(var_name, valid_min, valid_max)

# Get financial literacy components
fin_lit_vars <- cb |>
  filter(analytic_role == "financial_literacy") |>
  pull(var_name)

# Get labels for a specific variable
vl |> filter(var_name == "has_loan_type") |> select(code, label_en, sort_order)
```

---

## Version History

| Date | Change | Author |
|---|---|---|
| 2026-02-26 | Initial codebook created from Саволнома_final.docx | — |
