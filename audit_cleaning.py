# ============================================================
# COMPREHENSIVE DATA CLEANING AUDIT SCRIPT
# ============================================================
import sys
sys.stdout.reconfigure(encoding='utf-8', errors='replace')
sys.stderr.reconfigure(encoding='utf-8', errors='replace')

import pandas as pd
import numpy as np
import re
import warnings
warnings.filterwarnings('ignore')

BASE = "D:/old/Desktop/Cerr_old/Cerr/2025/58. \u041c\u0430\u0440\u043a\u0430\u0437\u0438\u0439 \u0431\u0430\u043d\u043a \u0442\u0430\u0434\u049b\u0438\u049b\u043e\u0442\u0438/\u0443\u043c\u0443\u043c\u0438\u0439 \u0440\u0435\u043f\u043e\u0440\u0442/replication/03.version"
OUT  = BASE + "/outputs_prep_v2"

# ---- Load all files ----
print("=" * 70)
print("LOADING FILES...")
print("=" * 70)

raw       = pd.read_excel(BASE + "/dataset17022026.xlsx")
cleaned   = pd.read_excel(OUT  + "/survey_master_cleaned_v2.xlsx")
borrowers = pd.read_excel(OUT  + "/borrowers_clean_v2.xlsx")
nonborr   = pd.read_excel(OUT  + "/nonborrowers_clean_v2.xlsx")
dropped   = pd.read_excel(OUT  + "/dropped_full_duplicates.xlsx")
chk       = pd.read_excel(OUT  + "/checkpoint_summary_v2.xlsx")
col_prof  = pd.read_excel(OUT  + "/column_profile_v2.xlsx")
q_reg     = pd.read_excel(OUT  + "/question_registry_v2.xlsx")
card_viol = pd.read_excel(OUT  + "/cardinality_violations_v2.xlsx")
con_fixes = pd.read_excel(OUT  + "/constraint_fixes_v2.xlsx")
age_mm    = pd.read_excel(OUT  + "/age_mismatch_log_v2.xlsx")
qc_int    = pd.read_excel(OUT  + "/qc_by_interviewer_v2.xlsx")
mat_flags = pd.read_excel(OUT  + "/matrix_block_flags_v2.xlsx")
mat_long  = pd.read_excel(OUT  + "/matrix_long_audit_v2.xlsx")

print(f"  RAW            : {raw.shape[0]:>6} rows x {raw.shape[1]:>4} cols")
print(f"  CLEANED        : {cleaned.shape[0]:>6} rows x {cleaned.shape[1]:>4} cols")
print(f"  BORROWERS      : {borrowers.shape[0]:>6} rows x {borrowers.shape[1]:>4} cols")
print(f"  NON-BORROWERS  : {nonborr.shape[0]:>6} rows x {nonborr.shape[1]:>4} cols")
print(f"  DROPPED DUPS   : {dropped.shape[0]:>6} rows x {dropped.shape[1]:>4} cols")
print(f"  CARD.VIOLATIONS: {card_viol.shape[0]:>6} rows x {card_viol.shape[1]:>4} cols")
print(f"  CONSTRAINT FIXES:{con_fixes.shape[0]:>5} rows x {con_fixes.shape[1]:>4} cols")
print(f"  AGE MISMATCH   : {age_mm.shape[0]:>6} rows x {age_mm.shape[1]:>4} cols")

# ============================================================
# CHECK 1: ROW COUNTS
# ============================================================
print("\n" + "=" * 70)
print("CHECK 1: ROW COUNTS")
print("=" * 70)

chk_dict = dict(zip(chk['metric'], chk['value']))
ck_raw_rows   = int(chk_dict.get('raw_rows', 0))
ck_dropped    = int(chk_dict.get('dropped_exact_duplicates', 0))
ck_after_drop = int(chk_dict.get('rows_after_drop', 0))
ck_borrowers  = int(chk_dict.get('borrowers_n', 0))
ck_nonborr    = int(chk_dict.get('nonborrowers_n', 0))
ck_hl_miss    = int(chk_dict.get('has_loan_missing_n', 0))

actual_raw    = raw.shape[0]
actual_clean  = cleaned.shape[0]
actual_borr   = borrowers.shape[0]
actual_nonb   = nonborr.shape[0]
actual_drop   = dropped.shape[0]

print(f"  Checkpoint raw_rows               : {ck_raw_rows}")
print(f"  Actual raw rows                   : {actual_raw}")
print(f"  Match?                            : {'YES' if ck_raw_rows == actual_raw else 'NO <<< MISMATCH'}")

print(f"\n  Checkpoint dropped_dups           : {ck_dropped}")
print(f"  Actual dropped file rows          : {actual_drop}")
print(f"  Match?                            : {'YES' if ck_dropped == actual_drop else 'NO <<< MISMATCH'}")

print(f"\n  raw_rows - dropped                : {actual_raw - actual_drop}")
print(f"  Actual cleaned rows               : {actual_clean}")
rowcount_ok = (actual_raw - actual_drop) == actual_clean
print(f"  raw - dropped == cleaned?         : {'YES' if rowcount_ok else 'NO <<< MISMATCH'}")

has_loan_na_in_cleaned = cleaned['has_loan'].isna().sum() if 'has_loan' in cleaned.columns else 'N/A'
borr_plus_nonb = actual_borr + actual_nonb
borr_plus_nonb_plus_na = borr_plus_nonb + (int(has_loan_na_in_cleaned) if isinstance(has_loan_na_in_cleaned, (int, np.integer, float)) else 0)

print(f"\n  borrowers + nonborrowers          : {borr_plus_nonb}")
print(f"  has_loan NA in cleaned            : {has_loan_na_in_cleaned}")
print(f"  borrowers + nonborr + NA          : {borr_plus_nonb_plus_na}")
print(f"  Equals cleaned rows?              : {'YES' if borr_plus_nonb_plus_na == actual_clean else 'NO <<< MISMATCH'}")

# ============================================================
# CHECK 2: DUPLICATE REMOVAL VERIFICATION
# ============================================================
print("\n" + "=" * 70)
print("CHECK 2: DUPLICATE REMOVAL VERIFICATION")
print("=" * 70)

if actual_drop > 0:
    # The dropped file contains the DUPLICATE rows (2nd+ occurrences) from raw.
    # dropped file has same columns as raw (no row_id added yet at this point in R pipeline)
    drop_copy = dropped.copy()

    # Use all common columns for comparison
    common_cols = [c for c in drop_copy.columns if c in raw.columns]
    print(f"  Columns in dropped file    : {drop_copy.shape[1]}")
    print(f"  Columns matchable to raw   : {len(common_cols)}")

    # Strategy 1: count raw duplicates with keep='first' - should equal dropped count
    raw_dup_mask_first = raw.duplicated(keep='first')
    n_raw_dups_second = raw_dup_mask_first.sum()
    print(f"\n  Raw rows that are 2nd+ occurrences (keep='first') : {n_raw_dups_second}")
    print(f"  Dropped file row count                            : {actual_drop}")
    print(f"  Match?                                            : {'YES' if n_raw_dups_second == actual_drop else 'NO <<< MISMATCH'}")

    # Strategy 2: Verify dropped rows appear at least twice in raw
    # Use string representation for robust comparison
    raw_str = raw[common_cols].fillna('__NA__').astype(str)
    drop_str = drop_copy[common_cols].fillna('__NA__').astype(str)

    # Create a hashable key for each row
    raw_key_counts = raw_str.apply(lambda row: '|||'.join(row.values), axis=1).value_counts()

    verified_true_dup = 0
    not_verified = 0
    for idx, row in drop_str.iterrows():
        key = '|||'.join(row.values)
        count_in_raw = raw_key_counts.get(key, 0)
        if count_in_raw >= 2:
            verified_true_dup += 1
        else:
            not_verified += 1

    print(f"\n  Dropped rows verified as true duplicates (>=2 in raw): {verified_true_dup} / {actual_drop}")
    print(f"  Dropped rows NOT verifiable as duplicates             : {not_verified}")
    if not_verified > 0:
        print(f"  <<< WARNING: {not_verified} dropped row(s) could not be confirmed as raw duplicates.")
        print("  (This may occur if dropped file columns differ from raw)")
    else:
        print("  PASS: All dropped rows confirmed as exact duplicates in raw.")

    # Strategy 3: check internal duplicates within dropped file
    n_within_drop_dups = drop_copy[common_cols].duplicated().sum()
    print(f"\n  Duplicates WITHIN the dropped file itself: {n_within_drop_dups}")
    if n_within_drop_dups > 0:
        print("  (expected if same row appeared 3+ times)")
else:
    print("  No dropped duplicates in file (0 rows).")

# ============================================================
# CHECK 3: DUMMY (0/1) COLUMNS VERIFICATION
# ============================================================
print("\n" + "=" * 70)
print("CHECK 3: DUMMY (0/1) COLUMN VERIFICATION (excluding matrix cols)")
print("=" * 70)

# Identify slash-encoded columns in cleaned (same logic as R script)
# A real checkbox dummy: contains "?/" followed by non-? chars
clean_cols = list(cleaned.columns)
dummy_pattern = re.compile(r'\?/\s*[^?]+$')
matrix_pattern_1 = re.compile(r'^\d+-2\.1\.')
matrix_pattern_2 = re.compile(r'^\d+-3\.3')

all_slash_cols = [c for c in clean_cols if '/' in c and dummy_pattern.search(c)]
matrix_slash_cols = [c for c in all_slash_cols if matrix_pattern_1.match(c) or matrix_pattern_2.match(c)]
nonmatrix_slash_cols = [c for c in all_slash_cols if c not in matrix_slash_cols]

print(f"  Total slash-encoded columns in cleaned : {len(all_slash_cols)}")
print(f"  Matrix slash columns (excluded)        : {len(matrix_slash_cols)}")
print(f"  Non-matrix slash cols (should be 0/1)  : {len(nonmatrix_slash_cols)}")

# Check 0/1 compliance
issues_01 = []
for col in nonmatrix_slash_cols:
    series = cleaned[col].dropna()
    if len(series) == 0:
        continue
    non_binary = series[~series.isin([0, 1, 0.0, 1.0])]
    if len(non_binary) > 0:
        issues_01.append({
            'column': col,
            'n_non_binary': len(non_binary),
            'sample_vals': str(non_binary.unique()[:5].tolist())
        })

if len(issues_01) == 0:
    print(f"  Result: ALL {len(nonmatrix_slash_cols)} non-matrix slash cols are properly 0/1.")
else:
    print(f"  <<< WARNING: {len(issues_01)} columns have non-0/1 values:")
    for iss in issues_01[:10]:
        print(f"    - {iss['column'][:60]}: {iss['n_non_binary']} non-binary vals -> {iss['sample_vals']}")

# Check non-matrix slash cols are numeric
non_numeric_slash = [c for c in nonmatrix_slash_cols if not pd.api.types.is_numeric_dtype(cleaned[c])]
print(f"\n  Non-matrix slash cols that are non-numeric dtype: {len(non_numeric_slash)}")
if non_numeric_slash:
    for c in non_numeric_slash[:5]:
        print(f"    - {c[:70]}: dtype={cleaned[c].dtype}")

# ============================================================
# CHECK 4: HAS_LOAN DISTRIBUTION
# ============================================================
print("\n" + "=" * 70)
print("CHECK 4: HAS_LOAN FLAG DISTRIBUTION")
print("=" * 70)

if 'has_loan' in cleaned.columns:
    hl_dist = cleaned['has_loan'].value_counts(dropna=False).sort_index()
    print("  has_loan distribution in CLEANED master:")
    for val, cnt in hl_dist.items():
        pct = cnt / len(cleaned) * 100
        label = "NA" if pd.isna(val) else str(int(val))
        print(f"    has_loan = {label:>4} : {cnt:>6} rows ({pct:.1f}%)")
    print(f"\n  Checkpoint borrowers_n    : {ck_borrowers}  | Actual borrowers file : {actual_borr}")
    print(f"  Checkpoint nonborrowers_n : {ck_nonborr}  | Actual nonborr file   : {actual_nonb}")
    print(f"  Checkpoint has_loan_miss  : {ck_hl_miss}  | Actual NA in cleaned  : {cleaned['has_loan'].isna().sum()}")

    # Borrowers file should only have has_loan==1
    if 'has_loan' in borrowers.columns:
        borr_hl = borrowers['has_loan'].value_counts(dropna=False)
        not_one = borrowers[borrowers['has_loan'] != 1]
        print(f"\n  Borrowers file: has_loan values = {dict(borr_hl.items())}")
        print(f"  Rows in borrowers file with has_loan != 1: {len(not_one)}")
    if 'has_loan' in nonborr.columns:
        nb_hl = nonborr['has_loan'].value_counts(dropna=False)
        not_zero = nonborr[nonborr['has_loan'] != 0]
        print(f"  NonBorrowers file: has_loan values = {dict(nb_hl.items())}")
        print(f"  Rows in nonborrowers file with has_loan != 0: {len(not_zero)}")
else:
    print("  <<< has_loan column NOT FOUND in cleaned data!")

# ============================================================
# CHECK 5: SKIP-LOGIC ENFORCEMENT
# ============================================================
print("\n" + "=" * 70)
print("CHECK 5: SKIP-LOGIC ENFORCEMENT (non-borrowers with borrower-only fields)")
print("=" * 70)

BORROWER_ONLY_PREFIXES = [
    "2.4.1", "2.4.2", "2.4.3", "2.4.4",
    "2.5", "2.6", "2.7", "2.8", "2.9", "2.10",
    "3.4", "3.5", "3.6", "3.7", "3.8", "3.9",
    "3.10", "3.11", "3.12", "3.13", "3.14", "3.15", "3.16"
]

def starts_with_any(col_name, prefixes):
    for p in prefixes:
        if col_name.startswith(p):
            return True
    return False

borrower_only_cols = [c for c in clean_cols if starts_with_any(c, BORROWER_ONLY_PREFIXES)]
borrower_only_main = [c for c in borrower_only_cols if '/' not in c]
borrower_only_dummy = [c for c in borrower_only_cols if '/' in c]

print(f"  Borrower-only columns total        : {len(borrower_only_cols)}")
print(f"  Borrower-only main (non-slash)     : {len(borrower_only_main)}")
print(f"  Borrower-only dummy (slash)        : {len(borrower_only_dummy)}")

if 'has_loan' in cleaned.columns:
    non_borr_mask = cleaned['has_loan'] == 0

    # Check main (non-slash) cols: should be NA for non-borrowers
    if borrower_only_main:
        nb_main = cleaned.loc[non_borr_mask, borrower_only_main]
        n_non_null_main = nb_main.notna().sum().sum()
        cols_with_violations_main = (nb_main.notna().sum() > 0).sum()
        print(f"\n  Non-borrowers with non-null values in borrower-only MAIN cols:")
        print(f"    Total non-null cells     : {n_non_null_main}")
        print(f"    Columns with violations  : {cols_with_violations_main}")
        if n_non_null_main > 0:
            top_viol = nb_main.notna().sum().sort_values(ascending=False).head(10)
            print("    Top 10 columns with violations:")
            for col, cnt in top_viol[top_viol > 0].items():
                print(f"      - {col[:70]}: {cnt} non-null non-borrowers")
        else:
            print("    PASS: All non-borrowers have NA in all borrower-only main cols.")

    # Check dummy (slash) cols: should be 0 for non-borrowers
    if borrower_only_dummy:
        nb_dummy = cleaned.loc[non_borr_mask, borrower_only_dummy].copy()
        # Convert to numeric first (coerce non-numeric to NaN)
        nb_dummy_num = nb_dummy.apply(pd.to_numeric, errors='coerce')
        # Should be 0 (not NA, not 1)
        n_nonzero_dummy = (nb_dummy_num.fillna(0) > 0).sum().sum()
        n_na_dummy = nb_dummy_num.isna().sum().sum()
        print(f"\n  Non-borrowers with >0 in borrower-only DUMMY cols:")
        print(f"    Non-zero cells           : {n_nonzero_dummy}")
        print(f"    NA cells (dummy should be 0, not NA): {n_na_dummy}")
        if n_nonzero_dummy > 0:
            print("    <<< WARNING: Non-borrowers have non-zero dummy values!")
        else:
            print("    PASS: All non-borrowers have 0 (or NA) in borrower-only dummy cols.")
        if n_na_dummy > 0:
            print("    NOTE: Some borrower-only dummy cols are NA for non-borrowers (R sets to 0, not NA)")
            # Show which cols have NA for non-borrowers
            na_by_col = nb_dummy_num.isna().sum().sort_values(ascending=False)
            print("    Cols with NA in non-borrowers (first 5):")
            for col_n, cnt in na_by_col[na_by_col>0].head(5).items():
                print(f"      - {col_n[:60]}: {cnt} NAs")

    # Also check NA borrowers
    na_borr_mask = cleaned['has_loan'].isna()
    n_na = na_borr_mask.sum()
    if n_na > 0 and borrower_only_main:
        na_main = cleaned.loc[na_borr_mask, borrower_only_main]
        n_non_null_na = na_main.notna().sum().sum()
        print(f"\n  Rows with has_loan=NA ({n_na} rows): non-null borrower-only main cells: {n_non_null_na}")
else:
    print("  <<< Cannot check: has_loan column not in cleaned.")

# ============================================================
# CHECK 6: CARDINALITY VIOLATIONS - WERE THEY FIXED?
# ============================================================
print("\n" + "=" * 70)
print("CHECK 6: CARDINALITY VIOLATIONS - LOGGED OR FIXED?")
print("=" * 70)

CARDINALITY_RULES = [
    ("2.2.",  3, "Q2.2 max 3 selections"),
    ("2.1.",  3, "Q2.1 max 3 'appropriate' purposes"),
    ("3.10.", 2, "Q3.10 max 2 selections"),
]

print(f"  Cardinality violations file: {len(card_viol)} rows")
if len(card_viol) > 0:
    print(f"  Violation breakdown:")
    if 'rule_name' in card_viol.columns:
        print(card_viol.groupby('rule_name').size().to_string())

print(f"\n  Re-checking in CLEANED data:")
for hint, maxn, rule in CARDINALITY_RULES:
    # Non-matrix slash cols starting with hint
    block_cols = [c for c in clean_cols if c.startswith(hint) and '/' in c and c not in matrix_slash_cols]
    # Keep only numeric
    block_num = [c for c in block_cols if pd.api.types.is_numeric_dtype(cleaned[c])]
    if not block_num:
        print(f"  {rule}: No numeric slash cols found with prefix '{hint}'")
        continue
    row_sums = cleaned[block_num].fillna(0).sum(axis=1)
    violations = (row_sums > maxn).sum()
    print(f"  {rule}: {violations} rows still exceed max={maxn} in cleaned data")
    if violations > 0:
        print(f"    <<< Violations NOT fixed, only logged.")
    else:
        print(f"    PASS: No remaining violations.")

# Q2.1 appropriate check
q21_app_cols = [c for c in clean_cols
                if '2.1.' in c and '/' in c
                and re.search('мувофиқ', c.lower()) and 'эмас' not in c.lower()]
q21_app_num = [c for c in q21_app_cols if pd.api.types.is_numeric_dtype(cleaned[c])]
if q21_app_num:
    sums = cleaned[q21_app_num].fillna(0).sum(axis=1)
    q21_viol = (sums > 3).sum()
    print(f"  Q2.1 max 3 appropriate: {q21_viol} rows still exceed max=3 in cleaned")
else:
    print(f"  Q2.1 appropriate cols: none found to re-check")

# ============================================================
# CHECK 7: CONSTRAINT FIXES (NEGATIVE VALUES -> NA)
# ============================================================
print("\n" + "=" * 70)
print("CHECK 7: CONSTRAINT FIXES VERIFICATION")
print("=" * 70)

print(f"  Constraint fixes file: {len(con_fixes)} rows")
if len(con_fixes) > 0 and 'rule' in con_fixes.columns:
    print("  Fix breakdown by rule:")
    print(con_fixes['rule'].value_counts().to_string())

# Column exact names from R script
CHILDREN_COL  = "1.8. 18 ёшга етмаган фарзандларингиз сони?"
HH_SIZE_COL   = "1.6 Уй хўжалигингиз жами аъзолари сони нечта?"
HH_WORKERS_COL = "1.7. Уй хўжалигингизда неча киши даромадли меҳнат (иш) билан банд?"
AGE_COL       = "1.1. Ёшингиз:"

constraint_cols = {
    'children':   (CHILDREN_COL,   0, None,  "Must be >= 0"),
    'hh_workers': (HH_WORKERS_COL, 0, None,  "Must be >= 0"),
    'hh_size':    (HH_SIZE_COL,    1, None,  "Must be >= 1"),
    'age':        (AGE_COL,        18, 100,  "Must be 18-100"),
}

for name, (col, lo, hi, desc) in constraint_cols.items():
    if col not in clean_cols:
        print(f"  {name} ({col[:40]}...): NOT FOUND in cleaned")
        continue
    series = cleaned[col].dropna()
    n_below = (series < lo).sum() if lo is not None else 0
    n_above = (series > hi).sum() if hi is not None else 0
    print(f"\n  {name}: {desc}")
    print(f"    Col in cleaned?: YES, dtype={cleaned[col].dtype}, non-null={len(series)}")
    if lo is not None:
        print(f"    Values < {lo}: {n_below}", "<<< REMAINING VIOLATIONS!" if n_below > 0 else "  PASS")
    if hi is not None:
        print(f"    Values > {hi}: {n_above}", "<<< REMAINING VIOLATIONS!" if n_above > 0 else "  PASS")
    if len(series) > 0:
        print(f"    Range in cleaned: [{series.min()}, {series.max()}], mean={series.mean():.1f}")

# Verify fixes: negative values in raw should be NA in cleaned
print(f"\n  Verifying raw negatives were nulled in cleaned:")
for name, (col, lo, hi, desc) in list(constraint_cols.items())[:3]:  # only children/hh_workers/hh_size
    if col not in raw.columns or col not in clean_cols:
        continue
    raw_neg = raw[col].dropna()
    raw_neg_count = (raw_neg < lo).sum() if lo is not None else 0
    print(f"  {name}: raw had {raw_neg_count} values < {lo}; fix log has {len(con_fixes[con_fixes['column']==col]) if 'column' in con_fixes.columns else 'N/A'} entries")

# ============================================================
# CHECK 8: COLUMN COUNT INTEGRITY
# ============================================================
print("\n" + "=" * 70)
print("CHECK 8: COLUMN COUNT INTEGRITY")
print("=" * 70)

raw_cols_n  = raw.shape[1]
clean_cols_n = cleaned.shape[1]

# Fully empty cols in raw
raw_missing_pct = raw.isna().mean()
raw_fully_empty = (raw_missing_pct >= 1.0).sum()

# has_loan and row_id added
added_cols = 2  # row_id + has_loan
# age col dropped (from script logic - if both "age" and "1.1." exist)
age_artifact_drop = 1 if 'age' in raw.columns and AGE_COL in raw.columns else 0

expected_cols = raw_cols_n - raw_fully_empty + added_cols - age_artifact_drop
# q24_sum is also added temporarily but then kept (check if in cleaned)
q24_sum_present = 'q24_sum' in cleaned.columns

print(f"  Raw columns                  : {raw_cols_n}")
print(f"  Fully empty cols in raw      : {raw_fully_empty}")
print(f"  Added: row_id + has_loan     : +{added_cols}")
print(f"  Dropped: artifact 'age' col  : -{age_artifact_drop} {'(both age and 1.1. found in raw)' if age_artifact_drop else '(no age artifact in raw)'}")
print(f"  q24_sum in cleaned?          : {q24_sum_present}")
q24_sum_add = 1 if q24_sum_present else 0
expected_cols_adj = expected_cols + q24_sum_add
print(f"  Expected cols (approx)       : {expected_cols_adj}")
print(f"  Actual cleaned cols          : {clean_cols_n}")
print(f"  Difference                   : {clean_cols_n - expected_cols_adj}")

if abs(clean_cols_n - expected_cols_adj) <= 3:
    print("  Result: Column count CONSISTENT (within tolerance).")
else:
    print("  <<< Column count DISCREPANCY - investigate further.")

# Columns in cleaned but not raw (new cols)
raw_col_set   = set(raw.columns)
clean_col_set = set(cleaned.columns)
new_cols = clean_col_set - raw_col_set
dropped_cols = raw_col_set - clean_col_set
print(f"\n  Columns in cleaned but NOT in raw (added): {len(new_cols)}")
for c in sorted(new_cols):
    print(f"    + {c}")
print(f"\n  Columns in raw but NOT in cleaned (removed): {len(dropped_cols)}")
for c in sorted(dropped_cols):
    print(f"    - {c}")

# ============================================================
# CHECK 9: NEW NAs INAPPROPRIATELY INTRODUCED
# ============================================================
print("\n" + "=" * 70)
print("CHECK 9: MISSING RATE COMPARISON (RAW vs CLEANED)")
print("=" * 70)

# Common columns (excluding borrower-only and added cols)
common_cols_rc = [c for c in raw.columns if c in clean_cols
                  and not starts_with_any(c, BORROWER_ONLY_PREFIXES)]

if common_cols_rc:
    raw_miss  = raw[common_cols_rc].isna().mean()
    clean_miss = cleaned[common_cols_rc].isna().mean()

    diff = clean_miss - raw_miss
    # Large increases in missingness (>5%) not explained by skip logic
    large_increase = diff[diff > 0.05].sort_values(ascending=False)
    large_decrease = diff[diff < -0.05].sort_values()

    print(f"  Common non-borrower-only cols analysed: {len(common_cols_rc)}")
    print(f"  Average raw missing rate     : {raw_miss.mean()*100:.2f}%")
    print(f"  Average cleaned missing rate : {clean_miss.mean()*100:.2f}%")
    print(f"  Cols with >5% MORE missing in cleaned: {len(large_increase)}")
    if len(large_increase) > 0:
        print("  Top columns with large missing rate INCREASE:")
        for col, val in large_increase.head(10).items():
            print(f"    - {col[:70]}: raw={raw_miss[col]*100:.1f}% -> cleaned={clean_miss[col]*100:.1f}% (delta=+{val*100:.1f}%)")
    else:
        print("  PASS: No unexpected large increases in missing rates.")

    print(f"\n  Cols with >5% FEWER missing in cleaned: {len(large_decrease)}")
    if len(large_decrease) > 0:
        for col, val in large_decrease.head(5).items():
            print(f"    - {col[:70]}: delta={val*100:.1f}%")
else:
    print("  No common non-borrower-only columns found for comparison.")

# ============================================================
# CHECK 10: DATA TYPES
# ============================================================
print("\n" + "=" * 70)
print("CHECK 10: DATA TYPE VERIFICATION")
print("=" * 70)

# Columns that were numeric in raw should remain numeric in cleaned
common_all = [c for c in raw.columns if c in clean_cols]
dtype_issues = []
for col in common_all:
    raw_num = pd.api.types.is_numeric_dtype(raw[col])
    cln_num = pd.api.types.is_numeric_dtype(cleaned[col])
    if raw_num and not cln_num:
        dtype_issues.append({
            'column': col,
            'raw_dtype': str(raw[col].dtype),
            'cleaned_dtype': str(cleaned[col].dtype),
            'issue': 'Was numeric, became non-numeric'
        })
    # text -> numeric would also be suspicious
    raw_obj = raw[col].dtype == object
    cln_obj = cleaned[col].dtype == object
    if not raw_obj and not cln_obj and raw_num and not cln_num:
        dtype_issues.append({
            'column': col,
            'raw_dtype': str(raw[col].dtype),
            'cleaned_dtype': str(cleaned[col].dtype),
            'issue': 'Numeric became non-numeric'
        })

print(f"  Common columns checked: {len(common_all)}")
print(f"  Numeric->non-numeric regressions: {len(dtype_issues)}")
if dtype_issues:
    for iss in dtype_issues[:10]:
        print(f"    - {iss['column'][:60]}: {iss['raw_dtype']} -> {iss['cleaned_dtype']}")
else:
    print("  PASS: All numeric columns remain numeric in cleaned.")

# Check dummy cols are integer-like
dummy_type_ok = 0
dummy_type_bad = 0
for col in nonmatrix_slash_cols:
    if col in cleaned.columns:
        if pd.api.types.is_integer_dtype(cleaned[col]) or cleaned[col].dropna().isin([0.0, 1.0]).all():
            dummy_type_ok += 1
        else:
            dummy_type_bad += 1
print(f"\n  Dummy (non-matrix slash) cols with integer-like values: {dummy_type_ok}")
print(f"  Dummy cols with non-integer-like values              : {dummy_type_bad}")

# ============================================================
# CHECK 11: BORROWERS + NONBORROWERS = MASTER
# ============================================================
print("\n" + "=" * 70)
print("CHECK 11: BORROWERS + NONBORROWERS = MASTER")
print("=" * 70)

total_split = actual_borr + actual_nonb
has_loan_na = cleaned['has_loan'].isna().sum() if 'has_loan' in cleaned.columns else 0
total_accounted = total_split + has_loan_na

print(f"  Borrowers rows         : {actual_borr}")
print(f"  Non-borrowers rows     : {actual_nonb}")
print(f"  Sum (borr + non-borr)  : {total_split}")
print(f"  has_loan=NA in master  : {has_loan_na}")
print(f"  Total accounted        : {total_accounted}")
print(f"  Master (cleaned) rows  : {actual_clean}")

if total_accounted == actual_clean:
    print("  PASS: borrowers + nonborrowers + NA = master.")
else:
    print(f"  <<< MISMATCH by {total_accounted - actual_clean} rows!")

# Verify same columns
borr_extra = set(borrowers.columns) - set(cleaned.columns)
borr_miss  = set(cleaned.columns) - set(borrowers.columns)
nb_extra   = set(nonborr.columns) - set(cleaned.columns)
nb_miss    = set(cleaned.columns) - set(nonborr.columns)

print(f"\n  Columns in borrowers not in master   : {len(borr_extra)}")
print(f"  Columns in master not in borrowers   : {len(borr_miss)}")
print(f"  Columns in nonborrowers not in master: {len(nb_extra)}")
print(f"  Columns in master not in nonborrowers: {len(nb_miss)}")

# ============================================================
# CHECK 12: OUT-OF-RANGE VALUES
# ============================================================
print("\n" + "=" * 70)
print("CHECK 12: OUT-OF-RANGE VALUES IN CLEANED DATA")
print("=" * 70)

range_checks = [
    (AGE_COL,       18,   100,  "Age"),
    (CHILDREN_COL,   0,    20,  "Children count"),
    (HH_SIZE_COL,    1,    30,  "HH size"),
    (HH_WORKERS_COL, 0,    20,  "HH workers"),
]

for col, lo, hi, label in range_checks:
    if col not in cleaned.columns:
        print(f"  {label}: column NOT FOUND in cleaned")
        continue
    series = cleaned[col].dropna()
    n_total = len(series)
    n_below = (series < lo).sum()
    n_above = (series > hi).sum()
    pct_below = n_below / n_total * 100 if n_total > 0 else 0
    pct_above = n_above / n_total * 100 if n_total > 0 else 0
    status_lo = "<<< VIOLATION" if n_below > 0 else "ok"
    status_hi = "<<< SUSPICIOUS" if n_above > 0 else "ok"
    print(f"  {label:20}: n={n_total}, range=[{series.min():.0f},{series.max():.0f}], "
          f"<{lo}:{n_below} ({pct_below:.1f}%) {status_lo}, "
          f">{hi}:{n_above} ({pct_above:.1f}%) {status_hi}")

# Additional: negative values in any numeric column
print(f"\n  Checking for ANY negative values in cleaned numeric cols:")
numeric_clean_cols = [c for c in cleaned.columns if pd.api.types.is_numeric_dtype(cleaned[c])
                      and c not in nonmatrix_slash_cols]  # exclude dummy cols
neg_cols = []
for col in numeric_clean_cols:
    s = cleaned[col].dropna()
    n_neg = (s < 0).sum()
    if n_neg > 0:
        neg_cols.append((col, n_neg, s.min()))
if neg_cols:
    print(f"  <<< {len(neg_cols)} columns still have negative values:")
    for col, n, minv in neg_cols[:10]:
        print(f"    - {col[:70]}: {n} negatives, min={minv}")
else:
    print("  PASS: No negative values in numeric non-dummy cleaned columns.")

# ============================================================
# CHECKPOINT SUMMARY COMPARISON
# ============================================================
print("\n" + "=" * 70)
print("CHECKPOINT SUMMARY vs ACTUAL")
print("=" * 70)

print(f"  {'Metric':<40} {'Checkpoint':>12} {'Actual':>12} {'Match':>8}")
print(f"  {'-'*76}")

actual_vals = {
    'raw_rows':                    actual_raw,
    'raw_cols':                    raw.shape[1],
    'dropped_exact_duplicates':    actual_drop,
    'rows_after_drop':             actual_clean,
    'cols_after_drop_empty':       cleaned.shape[1],
    'dummy_cols_count':            len(all_slash_cols),
    'borrowers_n':                 actual_borr,
    'nonborrowers_n':              actual_nonb,
    'has_loan_missing_n':          int(cleaned['has_loan'].isna().sum()) if 'has_loan' in cleaned.columns else 0,
    'cardinality_violations_n':    len(card_viol),
    'constraint_fixes_n':          len(con_fixes),
}

for metric, ck_val in chk_dict.items():
    act_val = actual_vals.get(metric, 'N/A')
    try:
        match = 'YES' if int(float(ck_val)) == int(act_val) else 'NO <<<'
    except (ValueError, TypeError):
        match = 'N/A'
    print(f"  {metric:<40} {str(int(float(ck_val))):>12} {str(act_val):>12} {match:>8}")

# ============================================================
# COLUMN PROFILE SPOT CHECK
# ============================================================
print("\n" + "=" * 70)
print("COLUMN PROFILE SPOT CHECK")
print("=" * 70)

print(f"  Column profile rows: {len(col_prof)}")
print(f"  Cleaned actual cols: {cleaned.shape[1]}")
if len(col_prof) == cleaned.shape[1]:
    print("  PASS: Column profile covers all cleaned columns.")
else:
    print(f"  <<< Mismatch: {len(col_prof)} vs {cleaned.shape[1]}")

# Top missing cols
if 'missing_pct' in col_prof.columns:
    top_miss = col_prof.nlargest(10, 'missing_pct')[['column', 'missing_pct', 'dtype']]
    print("\n  Top 10 most-missing columns in profile:")
    print(top_miss.to_string(index=False))

# ============================================================
# AGE MISMATCH LOG
# ============================================================
print("\n" + "=" * 70)
print("AGE MISMATCH LOG SUMMARY")
print("=" * 70)
print(f"  Age mismatch rows logged: {len(age_mm)}")
if len(age_mm) > 0 and 'age' in age_mm.columns and 'age_q' in age_mm.columns:
    diff_vals = (age_mm['age'] - age_mm['age_q']).abs()
    print(f"  Mean absolute difference : {diff_vals.mean():.1f} years")
    print(f"  Max absolute difference  : {diff_vals.max():.0f} years")
    print(f"  Sample mismatches (first 5):")
    print(age_mm.head(5).to_string(index=False))

# ============================================================
# QC BY INTERVIEWER SUMMARY
# ============================================================
print("\n" + "=" * 70)
print("QC BY INTERVIEWER SUMMARY")
print("=" * 70)
print(f"  Unique interviewers: {len(qc_int)}")
if 'n_interviews' in qc_int.columns:
    print(f"  Min interviews per interviewer: {qc_int['n_interviews'].min()}")
    print(f"  Max interviews per interviewer: {qc_int['n_interviews'].max()}")
    print(f"  Mean interviews per interviewer: {qc_int['n_interviews'].mean():.1f}")
if 'avg_missing_pct_all_vars' in qc_int.columns:
    print(f"  Mean missing% across interviewers: {qc_int['avg_missing_pct_all_vars'].mean():.1f}%")
    high_miss = qc_int[qc_int['avg_missing_pct_all_vars'] > 20]
    print(f"  Interviewers with >20% missing rate: {len(high_miss)}")
    if len(high_miss) > 0:
        print(high_miss[['interviewer', 'n_interviews', 'avg_missing_pct_all_vars']].head(10).to_string(index=False))

# ============================================================
# EXTRA: CHECK QUESTION REGISTRY CONSISTENCY
# ============================================================
print("\n" + "=" * 70)
print("QUESTION REGISTRY CONSISTENCY")
print("=" * 70)

print(f"  Registry rows: {len(q_reg)}")
if 'type' in q_reg.columns:
    print(f"  Type breakdown:")
    print(q_reg['type'].value_counts().to_string())
if 'column' in q_reg.columns:
    reg_cols = set(q_reg['column'].dropna())
    clean_col_set_check = set(cleaned.columns)
    in_reg_not_clean = reg_cols - clean_col_set_check
    in_clean_not_reg = clean_col_set_check - reg_cols
    print(f"\n  Cols in registry but NOT in cleaned: {len(in_reg_not_clean)}")
    print(f"  Cols in cleaned but NOT in registry: {len(in_clean_not_reg)}")
    if in_clean_not_reg:
        for c in sorted(in_clean_not_reg)[:10]:
            print(f"    + {c}")

# ============================================================
# FINAL SUMMARY
# ============================================================
print("\n" + "=" * 70)
print("FINAL AUDIT SUMMARY")
print("=" * 70)

issues = []
if not rowcount_ok:
    issues.append("ROW COUNT: raw - dropped != cleaned")
if actual_borr + actual_nonb + (int(has_loan_na_in_cleaned) if isinstance(has_loan_na_in_cleaned, (int, np.integer)) else 0) != actual_clean:
    issues.append("ROW COUNT: borr + nonborr + NA != master")
if len(issues_01) > 0:
    issues.append(f"DUMMY COLS: {len(issues_01)} non-matrix slash cols not 0/1")
if len(non_numeric_slash) > 0:
    issues.append(f"DTYPE: {len(non_numeric_slash)} slash cols non-numeric")
if len(dtype_issues) > 0:
    issues.append(f"DTYPE: {len(dtype_issues)} numeric->non-numeric regressions")
if neg_cols:
    issues.append(f"RANGE: {len(neg_cols)} numeric cols still have negative values")

if issues:
    print(f"  ISSUES FOUND ({len(issues)}):")
    for iss in issues:
        print(f"    <<< {iss}")
else:
    print("  No critical issues detected.")

print("\n  Informational (non-critical):")
print(f"    Cardinality violations logged (not fixed): {len(card_viol)}")
print(f"    Constraint fixes applied                 : {len(con_fixes)}")
print(f"    Age mismatches logged                    : {len(age_mm)}")
print(f"    Skip-logic violations (log only)         : {chk_dict.get('skip_logic_violations_n', 'N/A')}")

print("\n" + "=" * 70)
print("AUDIT COMPLETE")
print("=" * 70)
