import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
import os

plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.size'] = 12

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
out = os.path.join(base, "claude")
df = pd.read_excel(os.path.join(base, 'survey_master_cleaned_v1.xlsx'), engine='openpyxl')
borrowers = df[df['has_loan']==1]
nonborrowers = df[df['has_loan']==0]
N = len(df)
colors = plt.cm.Set2.colors

# CHART 9: First source when extra funds needed
fig, ax = plt.subplots(figsize=(10, 6))
src_col = df.columns[51]
src = df[src_col].value_counts()
short_labels = {
    'Банк ташкилотларига': 'Bank',
    'Оила аъзолари, дўстлар ёки танишларга': 'Oila/dostlar',
    'Қарз, кредит, насия олмасдан бошқа ечим қидирар эдим': 'Qarz olmasman',
    'Расмий насия хизматларига ("Техномарт", "Узум", "Ишонч" в.б.)': 'Rasmiy nasiya',
}
top5 = src.head(5)
labels = [short_labels.get(k, k[:20]) for k in top5.index]
vals = top5.values
pcts = vals / N * 100
bars = ax.barh(range(len(labels)), vals, color=colors[:len(labels)])
ax.set_yticks(range(len(labels)))
ax.set_yticklabels(labels)
for i, (v, p) in enumerate(zip(vals, pcts)):
    ax.text(v + 10, i, f'{v} ({p:.1f}%)', va='center', fontsize=10)
ax.set_xlabel('Respondentlar soni')
ax.set_title(f'9-rasm. Qoshimcha mablag zarur bolganda birinchi manba (N={N})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_09_first_source.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 9 saved')

# CHART 10: First source by region (heatmap-style stacked bar)
fig, ax = plt.subplots(figsize=(14, 8))
def simplify_source(val):
    if pd.isna(val): return 'Boshqa'
    val = str(val)
    if 'Банк' in val: return 'Bank'
    if 'Оила' in val or 'танишларга' in val: return 'Oila/dostlar'
    if 'олмасдан' in val: return 'Qarz olmasman'
    return 'Boshqa'
df['src_simple'] = df[src_col].apply(simplify_source)
ct = pd.crosstab(df['region'], df['src_simple'], normalize='index') * 100
ct = ct[['Bank', 'Oila/dostlar', 'Qarz olmasman', 'Boshqa']]
ct_sorted = ct.sort_values('Bank', ascending=True)
ct_sorted.plot(kind='barh', stacked=True, ax=ax, color=[colors[0], colors[1], colors[2], colors[3]])
ax.set_xlabel('Ulushi (%)')
ax.set_title('10-rasm. Birinchi manba hudud boyicha (N=2,131)')
ax.legend(title='Manba', loc='lower right')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_10_source_by_region.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 10 saved')

# CHART 11: Why informal borrowing
fig, ax = plt.subplots(figsize=(10, 6))
informal_mask = df[df.columns[54:61]].sum(axis=1) > 0
informal_n = informal_mask.sum()
inf_labels = ['Foizsiz (Bez %)', 'Oson va tez', 'Bank stavka yuqori', 'Rasmiy daromad yoq', 'Diniy sabab', 'Kredit tarix yomon', 'Boshqa']
inf_cols = [55, 54, 59, 56, 57, 58, 60]
inf_vals = [df.loc[informal_mask, df.columns[c]].sum() for c in inf_cols]
inf_pcts = [v/informal_n*100 for v in inf_vals]
y = range(len(inf_labels))
bars = ax.barh(y, inf_pcts, color=colors[1])
ax.set_yticks(y)
ax.set_yticklabels(inf_labels)
for i, (v, p) in enumerate(zip(inf_vals, inf_pcts)):
    ax.text(p + 0.5, i, f'{p:.1f}% (N={v})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'11-rasm. Norasmiy qarz olish sabablari (N={informal_n})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_11_why_informal.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 11 saved')

# CHART 12: Why commercial banks
fig, ax = plt.subplots(figsize=(10, 6))
bank_mask = df[df.columns[63:71]].sum(axis=1) > 0
bank_n = bank_mask.sum()
bk_labels = ['Shartlar aniq (Chetkiye)', 'Qulay/online', 'Uzoq muddat', 'Foizi kamroq', 'KI shakllanadi', 'Yirik summa', 'Tanish bor', 'Boshqa']
bk_cols = [63, 68, 69, 65, 64, 67, 66, 70]
bk_vals = [df.loc[bank_mask, df.columns[c]].sum() for c in bk_cols]
bk_pcts = [v/bank_n*100 for v in bk_vals]
y = range(len(bk_labels))
bars = ax.barh(y, bk_pcts, color=colors[0])
ax.set_yticks(y)
ax.set_yticklabels(bk_labels)
for i, (v, p) in enumerate(zip(bk_vals, bk_pcts)):
    ax.text(p + 0.5, i, f'{p:.1f}% (N={v})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'12-rasm. Bankdan qarz olish sabablari (N={bank_n})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_12_why_banks.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 12 saved')

# CHART 13: Credit purpose (borrowers)
fig, ax = plt.subplots(figsize=(12, 7))
purpose_labels = [
    'Biznes', 'Oilaviy tadbirkorlik', 'Kundalik xarajat',
    'Avtomobil', 'Maishiy texnika', 'Sogliq', 'Uy-joy/ipoteka',
    'Talim', 'Uy tamirlash', 'Toy/marosim', 'Boshqa qarz tolash',
    'Yaqinlarga yordam', 'Boshqa', 'Chetga ketish'
]
purpose_cols = [129, 130, 119, 122, 121, 120, 125, 123, 126, 124, 127, 131, 132, 128]
p_vals = [borrowers.iloc[:, c].sum() for c in purpose_cols]
p_pcts = [v/len(borrowers)*100 for v in p_vals]
y = range(len(purpose_labels))
bars = ax.barh(y, p_pcts, color=colors[2])
ax.set_yticks(y)
ax.set_yticklabels(purpose_labels)
for i, (v, p) in enumerate(zip(p_vals, p_pcts)):
    ax.text(p + 0.3, i, f'{p:.1f}% (N={v})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'13-rasm. Kredit maqsadlari (qarzdorlar, N={len(borrowers)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_13_credit_purpose.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 13 saved')

# CHART 14: Borrowing reason (life improvement vs crisis)
fig, ax = plt.subplots(figsize=(10, 5))
reason_col = df.columns[117]
reason = borrowers[reason_col].value_counts()
r_labels = ['Hayot tarzini yaxshilash\n(Uluchshenie zhizni)', 'Vaziyatdan chiqish\n(Vykhod iz situatsii)']
r_vals = reason.values
r_pcts = r_vals / r_vals.sum() * 100
bars = ax.bar(r_labels, r_vals, color=[colors[0], colors[4]], width=0.5)
for bar, v, p in zip(bars, r_vals, r_pcts):
    ax.text(bar.get_x()+bar.get_width()/2, bar.get_height()+15, f'{v}\n({p:.1f}%)', ha='center', fontsize=12)
ax.set_ylabel('Respondentlar soni')
ax.set_title(f'14-rasm. Qarz olish sababi (N={r_vals.sum()})')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_14_borrowing_reason.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 14 saved')

# CHART 15: Credit purpose by repayment status
fig, ax = plt.subplots(figsize=(14, 8))
npl = df[df['holat1'].str.contains('NPL', na=False)]
ontime = df[df['holat1'].str.contains('вақтида', na=False)]
delayed = df[df['holat1'].str.contains('1-3', na=False)]
top_purposes = ['Biznes', 'Oilaviy tadb.', 'Kundalik', 'Avtomobil', 'Maishiy tex.', 'Sogliq', 'Ipoteka', 'Talim']
top_cols = [129, 130, 119, 122, 121, 120, 125, 123]
npl_pcts = [npl.iloc[:, c].mean()*100 for c in top_cols]
del_pcts = [delayed.iloc[:, c].mean()*100 for c in top_cols]
on_pcts = [ontime.iloc[:, c].mean()*100 for c in top_cols]
x = np.arange(len(top_purposes))
w = 0.25
bars1 = ax.bar(x - w, npl_pcts, w, label=f'NPL 3+ oy (N={len(npl)})', color='#e74c3c')
bars2 = ax.bar(x, del_pcts, w, label=f'1-3 oy (N={len(delayed)})', color='#f39c12')
bars3 = ax.bar(x + w, on_pcts, w, label=f'Oz vaqtida (N={len(ontime)})', color='#27ae60')
ax.set_xticks(x)
ax.set_xticklabels(top_purposes, fontsize=10)
ax.set_ylabel('%')
ax.set_title('15-rasm. Kredit maqsadi tolov holati boyicha')
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_15_purpose_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 15 saved')

# CHART 16: Decision factors
fig, ax = plt.subplots(figsize=(10, 6))
dec_labels = ['Zaruriyat', 'Imtiyoz/lgota', 'Tanishlar tavsiyasi', 'Hokimiyat tavsiyasi', 'Aldanib qolgan', 'Boshqa', 'Inflyatsiya', 'KI yaxshilanadi']
dec_cols = [163, 164, 165, 166, 167, 170, 169, 168]
d_vals = [borrowers.iloc[:, c].sum() for c in dec_cols]
d_pcts = [v/len(borrowers)*100 for v in d_vals]
y = range(len(dec_labels))
bars = ax.barh(y, d_pcts, color=colors[5])
ax.set_yticks(y)
ax.set_yticklabels(dec_labels)
for i, (v, p) in enumerate(zip(d_vals, d_pcts)):
    ax.text(p + 0.3, i, f'{p:.1f}% (N={v})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'16-rasm. Qarz olish qaroriga tasir etgan omillar (N={len(borrowers)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_16_decision_factors.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 16 saved')

print('\n=== ALL BLOCK 2 CHARTS SAVED ===')
