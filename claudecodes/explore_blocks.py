import pandas as pd
import numpy as np
import sys
sys.stdout.reconfigure(encoding='utf-8')

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
df = pd.read_excel(f"{base}/survey_master_cleaned_v1.xlsx", engine='openpyxl')
borrowers = df[df['has_loan']==1].copy()
borrowers['is_npl'] = borrowers['holat1'].apply(
    lambda x: 1 if pd.notna(x) and 'NPL' in str(x) else 0
)

print(f"Total respondents: {len(df)}")
print(f"Borrowers (has_loan=1): {len(borrowers)}")
print(f"NPL among borrowers: {borrowers['is_npl'].sum()}")
print(f"Non-NPL: {(borrowers['is_npl']==0).sum()}")

# Print ALL columns
print("\n" + "="*80)
print("ALL COLUMNS (index, name, non-null-all, non-null-borrowers)")
print("="*80)
for i, col in enumerate(df.columns):
    na = borrowers[col].notna().sum()
    na2 = df[col].notna().sum()
    print(f"[{i:3d}] {col:55s} all={na2:5d}  borr={na:5d}")

# ============================================================
# BLOCK B: NPL causes - scan range 125-160
# ============================================================
print("\n" + "="*80)
print("BLOCK B: NPL CAUSES (cols 125-160)")
print("="*80)
npl_b = borrowers[borrowers['is_npl']==1]
for i in range(125, min(160, len(df.columns))):
    col = df.columns[i]
    nn = npl_b[col].notna().sum()
    if nn > 0:
        print(f"\nCol[{i}]: {col}  (NPL non-null={nn})")
        vc = npl_b[col].value_counts()
        for v, c in vc.head(8).items():
            print(f"  {v}: {c}")

# ============================================================
# BLOCK A: Decision factors - scan range 65-95
# ============================================================
print("\n" + "="*80)
print("BLOCK A: DECISION FACTORS (cols 65-95)")
print("="*80)
for i in range(65, min(95, len(df.columns))):
    col = df.columns[i]
    nn = borrowers[col].notna().sum()
    if nn > 0:
        print(f"\nCol[{i}]: {col}  (borr non-null={nn})")
        vc = borrowers[col].value_counts()
        for v, c in vc.head(8).items():
            print(f"  {v}: {c}")

# ============================================================
# BLOCK C: Credit rating - scan range 180-210
# ============================================================
print("\n" + "="*80)
print("BLOCK C: CREDIT RATING (cols 180-210)")
print("="*80)
for i in range(180, min(210, len(df.columns))):
    col = df.columns[i]
    nn_all = df[col].notna().sum()
    nn_b = borrowers[col].notna().sum()
    if nn_all > 0:
        print(f"\nCol[{i}]: {col}  (all={nn_all}, borr={nn_b})")
        vc = df[col].value_counts()
        for v, c in vc.head(8).items():
            print(f"  {v}: {c}")

# ============================================================
# BLOCK D: Credit purpose - scan range 55-75
# ============================================================
print("\n" + "="*80)
print("BLOCK D: CREDIT PURPOSE (cols 55-75)")
print("="*80)
for i in range(55, min(75, len(df.columns))):
    col = df.columns[i]
    nn = borrowers[col].notna().sum()
    if nn > 0:
        print(f"\nCol[{i}]: {col}  (borr non-null={nn})")
        vc = borrowers[col].value_counts()
        for v, c in vc.head(10).items():
            print(f"  {v}: {c}")

# ============================================================
# BLOCK E: Payment priorities - scan range 150-180
# ============================================================
print("\n" + "="*80)
print("BLOCK E: PAYMENT PRIORITIES (cols 150-180)")
print("="*80)
for i in range(150, min(180, len(df.columns))):
    col = df.columns[i]
    nn = borrowers[col].notna().sum()
    if nn > 0:
        print(f"\nCol[{i}]: {col}  (borr non-null={nn})")
        vc = borrowers[col].value_counts()
        for v, c in vc.head(10).items():
            print(f"  {v}: {c}")

print("\n\nDONE.")
