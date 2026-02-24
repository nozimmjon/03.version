import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os

plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.size'] = 12
colors = plt.cm.Set2.colors

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
out = os.path.join(base, "claude")
df = pd.read_excel(os.path.join(base, 'survey_master_cleaned_v1.xlsx'), engine='openpyxl')
borrowers = df[df['has_loan']==1]
N = len(df)

npl = df[df['holat1'].str.contains('NPL', na=False)]
ontime = df[df['holat1'].str.contains('вақтида', na=False)]
delayed = df[df['holat1'].str.contains('1-3', na=False)]

print('='*80)
print('BLOCK 3: PROBLEM LOAN DRIVERS')
print('='*80)

# === 2.4.1 Number of active loans ===
loan_count_col = df.columns[104]  # 2.4.1
print(f'\n### NUMBER OF ACTIVE LOANS (borrowers)')
print(borrowers[loan_count_col].value_counts().sort_index().to_string())
# By status
print('\nMultiple loans by status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    multi = (grp[loan_count_col] > 0).sum()
    print(f'  {label}: multiple loans = {multi} ({multi/len(grp)*100:.1f}%)')

# === 2.4.2 Debt-to-income ratio ===
dti_col = df.columns[105]  # 2.4.2
print(f'\n### DEBT-TO-INCOME RATIO (borrowers, N={borrowers[dti_col].notna().sum()})')
print(borrowers[dti_col].value_counts().to_string())
print('\nDTI by repayment status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    print(f'\n  {label} (N={len(grp)}):')
    vc = grp[dti_col].value_counts()
    for val, cnt in vc.items():
        print(f'    {val}: {cnt} ({cnt/len(grp)*100:.1f}%)')

# === 2.4.3 How many months can pay without income ===
reserve_col = df.columns[106]  # 2.4.3
print(f'\n### FINANCIAL RESERVES (months without income)')
print(borrowers[reserve_col].value_counts().sort_index().to_string())

# === 2.4.4 Income volatility ===
vol_col = df.columns[107]  # 2.4.4
print(f'\n### INCOME VOLATILITY (borrowers, N={borrowers[vol_col].notna().sum()})')
print(borrowers[vol_col].value_counts().to_string())
print('\nIncome volatility by repayment status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    print(f'\n  {label}:')
    vc = grp[vol_col].value_counts(normalize=True)*100
    for val, pct in vc.items():
        print(f'    {val}: {pct:.1f}%')

# === 3.7 Reasons for non-payment (cols 196-212) ===
print(f'\n### REASONS FOR NON-PAYMENT (borrowers, multiple response)')
reason_labels = [
    'Daromad kamaydi (Dokhod snizilsya)',
    'Ish haqi kechiktirildi (Zarplata zaderzhana)',
    'Boshqa qarzlarga (Na dr. dolgi)',
    'Daromad mavsumiy (Sezonnyy dokhod)',
    'Imkoniyat notogri baholangan (Pereotsenka)',
    'Boshqalar ham tolamaydi (Drugie ne platyat)',
    'Oqibati yoq deb oylagan (Net posledstviy)',
    'Imkoniyat bolganda tolash (Kogda budet vozmozh.)',
    'Kutilmagan xarajat (Neozhid. raskhody)',
    'Bank xatoligi (Oshibka banka)',
    'Yashirin shartlar (Skrytye usloviya)',
    'Ruxsatsiz kredit (Nesanktsion. kredit)',
    'Adolatsizlik (Nespravedlivost)',
    'Umuman kreditim yoq (Net kredita)',
    'Har doim tolayapman (Vsegda plachu)',
    'Javob qiyin (Trudno otvetit)',
    'Boshqa (Drugoe)',
]
for i, label in enumerate(reason_labels):
    col = df.columns[196+i]
    n_npl = npl[col].sum()
    n_del = delayed[col].sum()
    n_on = ontime[col].sum()
    n_all = borrowers[col].sum()
    print(f'  {label}: Total={n_all} | NPL={n_npl} ({n_npl/len(npl)*100:.1f}%) | 1-3m={n_del} ({n_del/len(delayed)*100:.1f}%) | On-time={n_on}')

# === CHARTS ===

# CHART 17: DTI by repayment status
fig, ax = plt.subplots(figsize=(12, 6))
dti_order = ['10% gacha', '10% dan – 20% gача', '20% dan – 35% gача', '35% dan – 50% гача', '50%дан кўп']
# Get actual values
dti_vals = borrowers[dti_col].value_counts()
dti_actual = list(dti_vals.index)
x = np.arange(len(dti_actual))
w = 0.25
npl_pcts = [npl[dti_col].value_counts(normalize=True).get(v, 0)*100 for v in dti_actual]
del_pcts = [delayed[dti_col].value_counts(normalize=True).get(v, 0)*100 for v in dti_actual]
on_pcts = [ontime[dti_col].value_counts(normalize=True).get(v, 0)*100 for v in dti_actual]
ax.bar(x-w, npl_pcts, w, label=f'NPL (N={len(npl)})', color='#e74c3c')
ax.bar(x, del_pcts, w, label=f'1-3 oy (N={len(delayed)})', color='#f39c12')
ax.bar(x+w, on_pcts, w, label=f'Oz vaqtida (N={len(ontime)})', color='#27ae60')
ax.set_xticks(x)
short_dti = [str(v)[:15] for v in dti_actual]
ax.set_xticklabels(short_dti, fontsize=9, rotation=15)
ax.set_ylabel('%')
ax.set_title('17-rasm. Qarz/daromad nisbati tolov holati boyicha')
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_17_dti_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 17 saved')

# CHART 18: Income volatility by status
fig, ax = plt.subplots(figsize=(10, 6))
vol_vals = borrowers[vol_col].value_counts().index.tolist()
x = np.arange(len(vol_vals))
w = 0.25
npl_pcts = [npl[vol_col].value_counts(normalize=True).get(v, 0)*100 for v in vol_vals]
del_pcts = [delayed[vol_col].value_counts(normalize=True).get(v, 0)*100 for v in vol_vals]
on_pcts = [ontime[vol_col].value_counts(normalize=True).get(v, 0)*100 for v in vol_vals]
ax.bar(x-w, npl_pcts, w, label=f'NPL (N={len(npl)})', color='#e74c3c')
ax.bar(x, del_pcts, w, label=f'1-3 oy (N={len(delayed)})', color='#f39c12')
ax.bar(x+w, on_pcts, w, label=f'Oz vaqtida (N={len(ontime)})', color='#27ae60')
ax.set_xticks(x)
short_vol = [str(v)[:20] for v in vol_vals]
ax.set_xticklabels(short_vol, fontsize=9)
ax.set_ylabel('%')
ax.set_title('18-rasm. Daromad ozgaruvchanligi tolov holati boyicha')
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_18_volatility_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 18 saved')

# CHART 19: Reasons for non-payment (top 10)
fig, ax = plt.subplots(figsize=(12, 7))
all_reasons = []
for i, label in enumerate(reason_labels):
    col = df.columns[196+i]
    n = npl[col].sum()
    all_reasons.append((label, n, n/len(npl)*100))
all_reasons.sort(key=lambda x: x[1], reverse=True)
top10 = [r for r in all_reasons if r[2] > 2][:10]
r_labels = [r[0] for r in top10]
r_pcts = [r[2] for r in top10]
r_ns = [r[1] for r in top10]
y = range(len(r_labels))
bars = ax.barh(y, r_pcts, color='#e74c3c')
ax.set_yticks(y)
ax.set_yticklabels(r_labels, fontsize=9)
for i, (n, p) in enumerate(zip(r_ns, r_pcts)):
    ax.text(p + 0.3, i, f'{p:.1f}% (N={n})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'19-rasm. NPL sabablari (N={len(npl)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_19_npl_reasons.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 19 saved')

print('\n=== BLOCK 3 COMPLETE ===')
