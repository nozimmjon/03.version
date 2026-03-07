# Visual Redesign Spec for `14_visualize_report_recheck_uz.R`

## Objective
This spec defines a focused redesign pass for the reproduced Uzbek chart pack. The goal is not to rebuild all 42 charts again, but to improve the highest-impact figures using stronger data-visualization principles.

Baseline script:
- `R/14_visualize_report_recheck_uz.R`

Redesign script:
- `R/15_visualize_report_redesign_uz.R`

Output folder for redesigned charts:
- `outputs/figures/redesign_uz/`

## Project-wide Visual Rules

1. One chart must answer one sentence only.
2. Every percentage chart must expose its denominator in subtitle, caption, or data label.
3. Colors must be semantic:
   - green = better / lower risk / on-time
   - orange = intermediate / caution
   - red = higher risk / NPL / non-formal risk
   - blue = neutral structural comparison / formal source
   - grey = neutral ranking with no analytical color meaning
4. Ranking charts should use one neutral hue unless color itself carries meaning.
5. Long category labels should be horizontal whenever possible.
6. Group comparisons should avoid rotated labels if a horizontal or faceted layout can solve the same problem.
7. Small subgroup risk must be visible when subgroup sizes vary materially.
8. Direct labels are preferred over legends when the chart can support them cleanly.

## Redesign Scope

### RD01. Region sample size
Old reference:
- `chart_01_regional_distribution`

Problem:
- The current figure mixes total sample size, borrower/non-borrower composition, percentages, and `N` labels in one chart.

Redesign:
- Separate the total sample-size task into its own ranking chart.
- Show only `n` by region, ordered descending.
- Use one neutral structural color.

Reason:
- Region sample size is a structural fact, not a compositional one.

### RD02. Borrower share by region
Old reference:
- `chart_01_regional_distribution`

Problem:
- Borrower share is visually buried inside a mixed stacked bar chart.

Redesign:
- Show borrower share by region as an ordered dot/lollipop chart.
- Add `n` labels.
- Flag low-`n` regions.

Reason:
- This makes regional differences interpretable without mixing them with total sample size.

### RD03. Primary source preference, simplified
Old reference:
- `chart_09_primary_credit_source`

Problem:
- Too many colors for a ranking task.
- Category semantics are not visually structured.

Redesign:
- Show top categories only.
- Use semantic grouping: formal / close network / informal / no borrowing.
- Keep direct labels with count and share.

Reason:
- The chart should communicate source structure, not just category count.

### RD04. Loan purpose ranking, simplified
Old reference:
- `chart_14_loan_purposes`

Problem:
- The figure is exhaustive but visually busy.

Redesign:
- Show top 8 purposes only in the main redesign pack.
- Use one neutral ranking color.
- Include both `n` and `%` in labels.

Reason:
- A visual reader needs the main ranking first; long tails belong in appendix.

### RD05. Purpose by repayment status
Old reference:
- `chart_15_purpose_by_status`

Problem:
- Vertical grouped bars plus angled labels create friction.

Redesign:
- Replace with faceted horizontal bars by repayment group.
- Keep consistent repayment-status colors.
- Put subgroup denominators into the subtitle.

Reason:
- Same information, lower cognitive load.

### RD06. NPL rate by credit type
Old reference:
- `chart_29_npl_by_credit_type`

Problem:
- Threshold colors imply stronger categorical boundaries than the data support.
- Sample-size differences matter a lot here.

Redesign:
- Use lollipop / dot chart with labels and `n`.
- Use point size or explicit labels for subgroup size.
- Keep one neutral risk color and flag only very small categories if necessary.

Reason:
- The task is comparison, not classification into arbitrary tiers.

### RD07. Q2.3 average amount by source
Old reference:
- `chart_40_q23_loan_size_by_source`

Problem:
- Layout clipping.
- Mixed labeling polish.
- Legend does too much work.

Redesign:
- Keep a horizontal ranking chart.
- Use wrapped title and tighter chart typography.
- Use semantic source-type colors.
- Keep direct amount labels.

Reason:
- This is analytically useful and should be publication-ready.

## Deliverables in the redesign script
The redesign script should generate:
- `rd01_region_sample_size.png`
- `rd02_region_borrower_share.png`
- `rd03_primary_credit_source_top.png`
- `rd04_loan_purpose_top8.png`
- `rd05_purpose_by_status_faceted.png`
- `rd06_npl_by_credit_type_lollipop.png`
- `rd07_q23_amount_by_source.png`

## Integration Recommendation
Use the redesign pack as a candidate replacement set for the main report. Do not replace appendix-style exhaustive charts automatically until the redesigned versions are reviewed.