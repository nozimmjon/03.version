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

npl = df[df['holat1'].str.contains('NPL', na=False)]
ontime = df[df['holat1'].str.contains('вақтида', na=False)]
delayed = df[df['holat1'].str.contains('1-3', na=False)]

print('='*80)
print('BLOCK 4: FINANCIAL LITERACY')
print('='*80)

# === 3.1 Perception of others paying on time ===
perc_col = df.columns[172]  # 3.1
print(f'\n### 3.1 DO PEOPLE AROUND YOU PAY ON TIME? (N={df[perc_col].notna().sum()})')
print(df[perc_col].value_counts().to_string())
print('\nBy borrower status:')
for label, grp in [('Borrowers', borrowers), ('Non-borrowers', df[df['has_loan']==0])]:
    print(f'\n  {label}:')
    vc = grp[perc_col].value_counts(normalize=True)*100
    for val, pct in vc.items():
        print(f'    {val}: {pct:.1f}%')

# === 3.4 Contract awareness ===
contract_col = df.columns[192]  # 3.4
print(f'\n### 3.4 CONTRACT AWARENESS (borrowers, N={borrowers[contract_col].notna().sum()})')
print(borrowers[contract_col].value_counts().to_string())
print('\nBy repayment status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    print(f'\n  {label}:')
    vc = grp[contract_col].value_counts(normalize=True)*100
    for val, pct in vc.items():
        print(f'    {val}: {pct:.1f}%')

# === 3.5 Understanding of terms ===
terms_col = df.columns[193]  # 3.5
print(f'\n### 3.5 UNDERSTANDING OF CREDIT TERMS (borrowers, N={borrowers[terms_col].notna().sum()})')
print(borrowers[terms_col].value_counts().to_string())
print('\nBy repayment status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    print(f'\n  {label}:')
    vc = grp[terms_col].value_counts(normalize=True)*100
    for val, pct in vc.items():
        print(f'    {val}: {pct:.1f}%')

# === 3.8 Acceptable delay period ===
delay_col = df.columns[214]  # 3.8
print(f'\n### 3.8 ACCEPTABLE DELAY PERIOD (borrowers, N={borrowers[delay_col].notna().sum()})')
print(borrowers[delay_col].value_counts().to_string())

# === 3.12 Credit rating motivation ===
rating_col = df.columns[244]  # 3.12
print(f'\n### 3.12 CREDIT RATING MOTIVATION (borrowers, N={borrowers[rating_col].notna().sum()})')
print(borrowers[rating_col].value_counts().to_string())

# === 3.13 Knowledge of delay impact ===
impact_col = df.columns[245]  # 3.13
print(f'\n### 3.13 KNOWLEDGE OF DELAY IMPACT ON CREDIT HISTORY (N={borrowers[impact_col].notna().sum()})')
print(borrowers[impact_col].value_counts().to_string())
print('\nBy repayment status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    print(f'\n  {label}:')
    vc = grp[impact_col].value_counts(normalize=True)*100
    for val, pct in vc.items():
        print(f'    {val}: {pct:.1f}%')

# === 3.14 Credit regret ===
regret_col = df.columns[246]  # 3.14
print(f'\n### 3.14 CREDIT REGRET (borrowers, N={borrowers[regret_col].notna().sum()})')
print(borrowers[regret_col].value_counts().to_string())
print('\nBy repayment status:')
for label, grp in [('NPL', npl), ('1-3m', delayed), ('On-time', ontime)]:
    pct = grp[regret_col].value_counts(normalize=True).get('Ҳа', 0)*100
    print(f'  {label}: Regret = {pct:.1f}%')

# === 2.9 Online vs offline and contract awareness cross ===
form_col = df.columns[161]  # 2.9
print(f'\n### ONLINE vs OFFLINE: CONTRACT AWARENESS')
online = borrowers[borrowers[form_col] == 'Онлайн']
offline = borrowers[borrowers[form_col] != 'Онлайн']
print(f'Online borrowers: N={len(online)}')
print(online[contract_col].value_counts(normalize=True).apply(lambda x: f'{x*100:.1f}%').to_string())
print(f'\nOffline borrowers: N={len(offline)}')
print(offline[contract_col].value_counts(normalize=True).apply(lambda x: f'{x*100:.1f}%').to_string())

# === CHARTS ===

# CHART 20: Contract awareness by status
fig, ax = plt.subplots(figsize=(12, 6))
c_vals = borrowers[contract_col].value_counts().index.tolist()
x = np.arange(len(c_vals))
w = 0.25
npl_p = [npl[contract_col].value_counts(normalize=True).get(v,0)*100 for v in c_vals]
del_p = [delayed[contract_col].value_counts(normalize=True).get(v,0)*100 for v in c_vals]
on_p = [ontime[contract_col].value_counts(normalize=True).get(v,0)*100 for v in c_vals]
ax.bar(x-w, npl_p, w, label=f'NPL (N={len(npl)})', color='#e74c3c')
ax.bar(x, del_p, w, label=f'1-3 oy (N={len(delayed)})', color='#f39c12')
ax.bar(x+w, on_p, w, label=f'Oz vaqtida (N={len(ontime)})', color='#27ae60')
ax.set_xticks(x)
short_c = [str(v)[:25] for v in c_vals]
ax.set_xticklabels(short_c, fontsize=9)
ax.set_ylabel('%')
ax.set_title('20-rasm. Shartnoma bilan tanishish tolov holati boyicha')
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_20_contract_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 20 saved')

# CHART 21: Understanding of terms by status
fig, ax = plt.subplots(figsize=(12, 6))
t_vals = borrowers[terms_col].value_counts().index.tolist()
x = np.arange(len(t_vals))
w = 0.25
npl_p = [npl[terms_col].value_counts(normalize=True).get(v,0)*100 for v in t_vals]
del_p = [delayed[terms_col].value_counts(normalize=True).get(v,0)*100 for v in t_vals]
on_p = [ontime[terms_col].value_counts(normalize=True).get(v,0)*100 for v in t_vals]
ax.bar(x-w, npl_p, w, label=f'NPL', color='#e74c3c')
ax.bar(x, del_p, w, label=f'1-3 oy', color='#f39c12')
ax.bar(x+w, on_p, w, label=f'Oz vaqtida', color='#27ae60')
ax.set_xticks(x)
short_t = [str(v)[:30] for v in t_vals]
ax.set_xticklabels(short_t, fontsize=8)
ax.set_ylabel('%')
ax.set_title('21-rasm. Kredit shartlarini tushunish tolov holati boyicha')
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_21_terms_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 21 saved')

# CHART 22: Credit regret by status
fig, ax = plt.subplots(figsize=(8, 5))
statuses = ['NPL 3+ oy', '1-3 oy', 'Oz vaqtida']
regret_pcts = []
for grp in [npl, delayed, ontime]:
    pct = grp[regret_col].value_counts(normalize=True).get('Ҳа', 0)*100
    regret_pcts.append(pct)
bar_colors = ['#e74c3c', '#f39c12', '#27ae60']
bars = ax.bar(statuses, regret_pcts, color=bar_colors, width=0.5)
for bar, p in zip(bars, regret_pcts):
    ax.text(bar.get_x()+bar.get_width()/2, bar.get_height()+1, f'{p:.1f}%', ha='center', fontsize=12)
ax.set_ylabel('%')
ax.set_title('22-rasm. Kredit olganidan afsuslanish')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_22_regret_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 22 saved')

# CHART 23: Online vs offline contract awareness
fig, axes = plt.subplots(1, 2, figsize=(14, 5))
for ax_i, (grp, title, n) in enumerate([(online, 'Online', len(online)), (offline, 'Offline', len(offline))]):
    vc = grp[contract_col].value_counts()
    short = [str(v)[:20] for v in vc.index]
    axes[ax_i].pie(vc.values, labels=short, autopct='%1.1f%%', colors=colors[:len(vc)], textprops={'fontsize':9})
    axes[ax_i].set_title(f'{title} (N={n})')
fig.suptitle('23-rasm. Shartnoma bilan tanishish: Online vs Offline', fontsize=13)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_23_online_vs_offline.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 23 saved')

print('\n=== BLOCK 4 COMPLETE ===')
