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

# Family entrepreneurship borrowers
fam = df[df['kredit_turi'] == 'oilaviy'].copy()
non_fam_borrowers = df[(df['has_loan']==1) & (df['kredit_turi'] != 'oilaviy')].copy()

npl_fam = fam[fam['holat1'].str.contains('NPL', na=False)]
ontime_fam = fam[fam['holat1'].str.contains('вақтида', na=False)]

print('='*80)
print('BLOCK 7: FAMILY ENTREPRENEURSHIP PROGRAM')
print('='*80)

print(f'\n### OVERVIEW')
print(f'Family entrepreneurship borrowers: N={len(fam)}')
print(f'Other borrowers: N={len(non_fam_borrowers)}')
print(f'\nRepayment status (family):')
print(fam['holat1'].value_counts().to_string())
npl_rate = fam['holat1'].str.contains('NPL', na=False).mean()*100
print(f'NPL rate: {npl_rate:.1f}%')

# Compare demographics
age_col = '1.1. Ёшингиз:'
inc_col = df.columns[40]
edu_col = '1.4. Маълумотингиз:'
gender_col = '1.2. Респондент жинси'

print(f'\n### DEMOGRAPHIC COMPARISON')
print(f'Mean age: Family={fam[age_col].mean():.1f}, Other={non_fam_borrowers[age_col].mean():.1f}')
print(f'Male %: Family={fam[gender_col].eq("Эркак").mean()*100:.1f}%, Other={non_fam_borrowers[gender_col].eq("Эркак").mean()*100:.1f}%')
print(f'Higher edu %: Family={fam[edu_col].str.contains("Олий", na=False).mean()*100:.1f}%, Other={non_fam_borrowers[edu_col].str.contains("Олий", na=False).mean()*100:.1f}%')

print(f'\nIncome distribution (family):')
print(fam[inc_col].value_counts().to_string())

# === 2.7a Why family entrepreneurship credit (cols 135-140) ===
print(f'\n### 2.7a WHY FAMILY ENTREPRENEURSHIP CREDIT (N={len(fam)})')
fam_reason_labels = [
    'Biznes goyani amalga oshirish',
    'Foizlari past',
    'Yaqinlar tajribasi',
    'Bosim va tasir ostida',
    'Birovning maslahatiga ishonib',
    'Boshqa',
]
for i, label in enumerate(fam_reason_labels):
    col = df.columns[135+i]
    n = fam[col].sum()
    pct = n/len(fam)*100
    # NPL vs on-time
    n_npl = npl_fam[col].sum()
    n_on = ontime_fam[col].sum() if len(ontime_fam) > 0 else 0
    print(f'  {label}: N={n} ({pct:.1f}%) | NPL={n_npl/len(npl_fam)*100:.1f}%')

# === 2.7b NPL reasons for family credit (cols 143-148) ===
print(f'\n### 2.7b NPL REASONS FOR FAMILY CREDIT (N={len(fam)})')
npl_reason_labels = [
    'Daromad kutilganidan past (Dokhod nizhe ozhid.)',
    'Biznes yurmagan (Biznes ne poshyol)',
    'Davlat krediti qaytarish shart emas (Gos. kredit ne nado)',
    'Istemolga yonaltirilgan (Na potrebleniye)',
    'Shartlar notogri talqin (Nepr. interpretatsiya)',
    'Boshqa sabab (Drugaya prichina)',
]
for i, label in enumerate(npl_reason_labels):
    col = df.columns[143+i]
    n = fam[col].sum()
    pct = n/len(fam)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 2.7v Support measures (cols 151-157) ===
print(f'\n### 2.7v SUPPORT MEASURES NEEDED (N={len(fam)})')
support_labels = [
    'Biznes konikma treningi',
    'Tolov muddatini moslash',
    'Kreditni bosqichma-bosqich',
    'Biznes reja yordam',
    'Nazoratni kuchaytirish',
    'Naqd pul va plastik',
    'Boshqa',
]
for i, label in enumerate(support_labels):
    col = df.columns[151+i]
    n = fam[col].sum()
    pct = n/len(fam)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 2.8 Targeted use ===
target_col = df.columns[159]
print(f'\n### TARGETED USE OF CREDIT (family)')
print(fam[target_col].value_counts().to_string())

# === DTI for family ===
dti_col = df.columns[105]
print(f'\n### DTI RATIO (family)')
print(fam[dti_col].value_counts().to_string())

# === Income volatility for family ===
vol_col = df.columns[107]
print(f'\n### INCOME VOLATILITY (family)')
print(fam[vol_col].value_counts().to_string())

# Regional distribution
print(f'\n### REGIONAL DISTRIBUTION (family)')
reg_fam = fam['region'].value_counts()
for r, cnt in reg_fam.items():
    npl_cnt = npl_fam[npl_fam['region']==r].shape[0]
    npl_pct = npl_cnt/cnt*100 if cnt > 0 else 0
    print(f'  {r}: N={cnt} | NPL={npl_cnt} ({npl_pct:.1f}%)')

# === CHARTS ===

# CHART 27: Family credit NPL reasons
fig, ax = plt.subplots(figsize=(10, 6))
n_vals = [fam.iloc[:, 143+i].sum() for i in range(6)]
n_pcts = [v/len(fam)*100 for v in n_vals]
sorted_idx = np.argsort(n_pcts)[::-1]
s_labels = [npl_reason_labels[i] for i in sorted_idx]
s_pcts = [n_pcts[i] for i in sorted_idx]
s_ns = [n_vals[i] for i in sorted_idx]
y = range(len(s_labels))
bars = ax.barh(y, s_pcts, color='#e74c3c')
ax.set_yticks(y)
ax.set_yticklabels([l[:35] for l in s_labels], fontsize=9)
for i, (n, p) in enumerate(zip(s_ns, s_pcts)):
    ax.text(p+0.3, i, f'{p:.1f}% (N={n})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'27-rasm. Oilaviy kredit NPL sabablari (N={len(fam)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_27_family_npl_reasons.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 27 saved')

# CHART 28: Support measures needed
fig, ax = plt.subplots(figsize=(10, 6))
s_vals = [fam.iloc[:, 151+i].sum() for i in range(7)]
s_pcts_m = [v/len(fam)*100 for v in s_vals]
sorted_idx = np.argsort(s_pcts_m)[::-1]
s_labels2 = [support_labels[i] for i in sorted_idx]
s_pcts2 = [s_pcts_m[i] for i in sorted_idx]
s_ns2 = [s_vals[i] for i in sorted_idx]
y = range(len(s_labels2))
bars = ax.barh(y, s_pcts2, color=colors[2])
ax.set_yticks(y)
ax.set_yticklabels(s_labels2)
for i, (n, p) in enumerate(zip(s_ns2, s_pcts2)):
    ax.text(p+0.3, i, f'{p:.1f}% (N={n})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'28-rasm. Oilaviy kredit qollab-quvvatlash choralari (N={len(fam)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_28_family_support.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 28 saved')

# CHART 29: Family vs Other - NPL rate comparison
fig, ax = plt.subplots(figsize=(10, 6))
credit_types = df['kredit_turi'].value_counts().index.tolist()
npl_rates = []
counts = []
for ct in credit_types:
    grp = df[df['kredit_turi']==ct]
    npl_r = grp['holat1'].str.contains('NPL', na=False).mean()*100
    npl_rates.append(npl_r)
    counts.append(len(grp))
sorted_idx = np.argsort(npl_rates)[::-1]
s_types = [credit_types[i] for i in sorted_idx]
s_rates = [npl_rates[i] for i in sorted_idx]
s_counts = [counts[i] for i in sorted_idx]
y = range(len(s_types))
bar_colors = ['#e74c3c' if r > 50 else '#f39c12' if r > 30 else '#27ae60' for r in s_rates]
bars = ax.barh(y, s_rates, color=bar_colors)
ax.set_yticks(y)
ax.set_yticklabels(s_types)
for i, (r, n) in enumerate(zip(s_rates, s_counts)):
    ax.text(r+0.5, i, f'{r:.1f}% (N={n})', va='center', fontsize=9)
ax.set_xlabel('NPL rate (%)')
ax.set_title('29-rasm. NPL darajasi kredit turi boyicha')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_29_npl_by_credit_type.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 29 saved')

# CHART 30: Why took family credit
fig, ax = plt.subplots(figsize=(10, 6))
r_vals = [fam.iloc[:, 135+i].sum() for i in range(6)]
r_pcts = [v/len(fam)*100 for v in r_vals]
sorted_idx = np.argsort(r_pcts)[::-1]
s_labels3 = [fam_reason_labels[i] for i in sorted_idx]
s_pcts3 = [r_pcts[i] for i in sorted_idx]
s_ns3 = [r_vals[i] for i in sorted_idx]
y = range(len(s_labels3))
bars = ax.barh(y, s_pcts3, color=colors[0])
ax.set_yticks(y)
ax.set_yticklabels(s_labels3)
for i, (n, p) in enumerate(zip(s_ns3, s_pcts3)):
    ax.text(p+0.3, i, f'{p:.1f}% (N={n})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'30-rasm. Oilaviy kredit olish sabablari (N={len(fam)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_30_why_family_credit.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 30 saved')

print('\n=== BLOCK 7 COMPLETE ===')
