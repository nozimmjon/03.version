# Claude codes audit and reproduction plan

## 1) What I checked

I reviewed:
- The reproducible `targets` pipeline in this repository (`_targets.R`, `R/*.R`, `reports/config.yml`).
- The Python scripts in `claudecodes/` that generated the Claude charts and report draft.
- The chart artifacts already present in `claudecodes/`.

## 2) Key findings from the Claude codebase

1. **Runtime path is hard-coded to a Windows local directory**, e.g. `D:\old\Desktop\...` in multiple scripts. This prevents direct execution in the repo without edits.
2. **Each Python script reads the Excel file directly** and often repeats cleaning/filtering logic instead of using a shared preprocessing layer.
3. **Column access is mixed**:
   - some charts use stable named columns (e.g., `region`, `kredit_turi`, `holat1`),
   - others rely on **column indices** discovered by exploratory scanning (`explore_blocks.py`), which is fragile when questionnaire layouts change.
4. Claude scripts collectively reference **41 PNG outputs** (including alternative model versions such as chart 24/31 variants), and all referenced PNG files are present in `claudecodes/`.
5. The repository's R pipeline currently produces a smaller, reproducible core set (fig1–fig9 + tables), with config-driven variable mapping and target-based orchestration.

## 3) Gap between existing reproducible pipeline and Claude artifacts

- Current R pipeline is production-oriented and reproducible, but covers only a subset of Claude's chart universe.
- Claude output includes additional thematic blocks (decision factors, collection/reminders, family credit behavior, payment norms/priorities, detailed NPL causes) not yet represented as R targets.
- Claude's model charts have multiple variants (`chart_24_logistic_coef`, `chart_24_logistic_coef_FIXED`, `chart_24_v2_no_scaler`, etc.), so we need one explicit canonical model spec before reproducing.

## 4) Reproduction strategy using existing code

### Phase A — Stabilize inputs and naming

1. **Create a chart registry file** (YAML/CSV) mapping:
   - `claude_chart_id` (e.g., `chart_17_dti_by_status`),
   - source script,
   - required columns,
   - filter universe (all, borrowers, NPL only),
   - target output path in `outputs/figures/`.
2. Replace all Python hard-coded `base` paths with repo-relative `Path(__file__).resolve().parents[...]` + CLI args.
3. Freeze one canonical dataset: `data/processed/analysis_data.rds` + optional parquet/CSV export for Python compatibility.

### Phase B — Reproduce Claude charts block-by-block

Use the existing R pipeline architecture as the primary orchestrator.

1. Add new chart modules in R by theme:
   - `R/21_claude_block1.R` (charts 01–08),
   - `R/22_claude_block2.R` (charts 09–16),
   - `R/23_claude_block3_4.R` (charts 17–23),
   - `R/24_claude_models.R` (charts 24, 31, 32 canonicalized),
   - `R/25_claude_block7_enrichment.R` (charts 27–30, 33–38).
2. For each chart, implement a deterministic function with:
   - explicit input columns from config,
   - explicit ordering/label dictionaries,
   - consistent theme and export size.
3. Extend `_targets.R` with one target per chart + grouped save targets.

### Phase C — Model harmonization

1. Decide one logistic spec to reproduce (recommended: current tidymodels-based R spec as canonical).
2. Recreate Claude model visuals from that single spec:
   - coefficient/OR forest,
   - ROC,
   - optional calibration plot.
3. Keep legacy Claude variants as appendix comparisons, not main outputs.

### Phase D — Equivalence validation

1. Build a validation script that compares reproduced charts vs Claude charts:
   - key percentages/totals extracted from plotting data,
   - tolerances for rounding differences.
2. Produce a `reproduction_log.md` with pass/fail by chart.

## 5) Concrete execution plan (recommended order)

1. **Inventory pass (1 day)**
   - Build registry for all Claude charts and dependent fields.
2. **Block 1+2 migration (2–3 days)**
   - Reproduce charts 01–16 in R targets.
3. **Block 3+4 migration (1–2 days)**
   - Reproduce charts 17–23.
4. **Model canonicalization (1–2 days)**
   - Finalize one model family and reproduce 24/31/32 equivalents.
5. **Family/enrichment migration (2 days)**
   - Reproduce 27–30 and 33–38.
6. **Validation and report wiring (1 day)**
   - Automated checks + optional Quarto section embedding.

## 6) Minimal command sequence to start reproduction now

```bash
# 1) Build current reproducible baseline
Rscript -e "targets::tar_make()"

# 2) Add and run a Claude-equivalence target group once implemented
Rscript -e "targets::tar_make(names = starts_with('claude_'))"

# 3) Render report after chart migration
Rscript -e "quarto::quarto_render('reports/report.qmd')"
```

## 7) Definition of done

Reproduction is complete when:
- Every selected Claude chart has a corresponding deterministic target in the R pipeline.
- Chart titles/base N/filter logic match agreed specs.
- Validation log confirms numeric agreement (or documents justified deviations).
- Outputs are generated without hard-coded local paths.
