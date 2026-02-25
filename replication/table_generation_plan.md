# Table generation plan (revised using `Доклад_катта-25` reference)

## What changed after checking the Word reference

The report reference (`data/raw/report_reference.docx`) includes a concrete **table-of-contents contract** for tables, not just charts. Based on that, table replication should be ordered to match the report structure and numbering used in the document.

Target table sequence from the reference structure:
- 1-жадвал: Respondent income levels
- 2-жадвал: Detailed NPL cause categories
- 3-жадвал: Contract familiarity status
- 4-жадвал: Repayment/collection methods by payment status
- 5-жадвал: Family entrepreneurship non-payment reasons
- 6–13-жадвал: research parameters, distributions, demographic profile, model quality/statistics/econometrics/correlation

## Revised implementation approach

### Phase A — Canonical table registry (first deliverable)
Create `replication/table_registry.yml` with one row per table:
- `table_id` (e.g. `table_01_income_levels`)
- `report_label` (e.g. `1-жадвал`)
- `section` (III, IV, V, VI, VII, Appendix)
- `source_questions` (e.g. `1.10`, `3.7`, `3.4`, `3.9`, `2.7.б`)
- `population` (`all`, `borrowers`, `npl`, `family_credit`)
- `metric_type` (`single_choice_share`, `multi_select_share`, `status_crosstab`, `model_summary`)
- `output_files` (csv/html)

### Phase B — One reproducible R table script per block
Add a new script:
- `replication/recreate_claudecode_tables.R`

This script should:
1. Read cleaned v2 data (`outputs_prep_v2/survey_master_cleaned_v2.csv`).
2. Derive stable status fields (`repay_group`, `is_npl`).
3. Build reusable helpers:
   - `build_single_choice_table()`
   - `build_multi_select_table()`
   - `build_status_crosstab_table()`
   - `save_table_csv_html()`
4. Generate tables in report order (1 → 13), using dynamic column detection for matrix/multi-select questions.

### Phase C — Priority order aligned to report content
Generate in this strict order:
1. **Tables 1–5** (core narrative tables from Sections III–VII):
   - income levels
   - detailed NPL causes
   - contract familiarity
   - collection/repayment methods by status
   - family-credit non-payment reasons
2. **Tables 6–10** (already partly present in pipeline; reconcile naming/denominators with report labels).
3. **Tables 11–13** (statistics/econometrics/correlation), sourced from model outputs and tidy summaries.

### Phase D — Validation and reconciliation
For each table:
- Write raw counts and percentages to CSV.
- Validate denominator logic (`all`, `borrowers`, `NPL`, etc.).
- Add `replication/table_validation_log.csv` with checks:
  - sum of shares by block
  - expected category presence
  - non-missing essential groups

## Notes on source of truth

- Use cleaned v2 data as computational source of truth.
- Use Word file numbering/content as **presentation contract** (order, labels, scope).
- Use `claudecodes/generate_report_v6.py` as secondary reference for headings/row semantics where needed.
