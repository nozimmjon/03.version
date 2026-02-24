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

# CHART 1: Regional distribution
fig, ax = plt.subplots(figsize=(12, 7))
reg = df['region'].value_counts().sort_values()
reg_b = borrowers['region'].value_counts()
reg_nb = nonborrowers['region'].value_counts()
regions = reg.index
y = np.arange(len(regions))
b_vals = [reg_b.get(r, 0) for r in regions]
nb_vals = [reg_nb.get(r, 0) for r in regions]
ax.barh(y, b_vals, 0.7, label='Qarzdorlar', color=colors[0])
ax.barh(y, nb_vals, 0.7, left=b_vals, label='Qarzdor emaslar', color=colors[1])
ax.set_yticks(y)
ax.set_yticklabels(regions, fontsize=10)
ax.set_xlabel('Respondentlar soni')
ax.set_title('1-rasm. Respondentlarning hududiy taqsimoti (N=2,131)')
ax.legend(loc='lower right')
for i, (b, nb) in enumerate(zip(b_vals, nb_vals)):
    ax.text(b+nb+5, i, str(b+nb), va='center', fontsize=9)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_01_regional_distribution.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 1 saved: Regional distribution')

# CHART 2: Age distribution
fig, ax = plt.subplots(figsize=(10, 6))
age_col = '1.1. Ёшингиз:'
bins = [0, 25, 35, 45, 55, 100]
labels = ['18-25', '26-35', '36-45', '46-55', '56+']
df['age_group'] = pd.cut(df[age_col], bins=bins, labels=labels, right=True)
b_age = borrowers[age_col].apply(lambda x: pd.cut([x], bins=bins, labels=labels)[0]).value_counts().sort_index()
nb_age = nonborrowers[age_col].apply(lambda x: pd.cut([x], bins=bins, labels=labels)[0]).value_counts().sort_index()
x = np.arange(len(labels))
w = 0.35
bars1 = ax.bar(x - w/2, [b_age.get(l,0) for l in labels], w, label='Qarzdorlar', color=colors[0])
bars2 = ax.bar(x + w/2, [nb_age.get(l,0) for l in labels], w, label='Qarzdor emaslar', color=colors[1])
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.set_xlabel('Yosh guruhlari')
ax.set_ylabel('Respondentlar soni')
ax.set_title('2-rasm. Yosh guruhlari boyicha taqsimot (N=2,131)')
ax.legend()
ax.bar_label(bars1, fontsize=9)
ax.bar_label(bars2, fontsize=9)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_02_age_distribution.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 2 saved: Age distribution')

# CHART 3: Gender
fig, axes = plt.subplots(1, 2, figsize=(12, 5))
gender = df['1.2. Респондент жинси'].value_counts()
axes[0].pie(gender.values, labels=['Erkak (Muzhchiny)', 'Ayol (Zhenshchiny)'],
            autopct='%1.1f%%', colors=[colors[2], colors[3]], startangle=90, textprops={'fontsize':12})
axes[0].set_title(f'Umumiy (N={N})')
gen_b = borrowers['1.2. Респондент жинси'].value_counts()
gen_nb = nonborrowers['1.2. Респондент жинси'].value_counts()
categories = ['Erkak', 'Ayol']
cat_uz = ['Эркак', 'Аёл']
b_pcts = [gen_b.get(c,0)/len(borrowers)*100 for c in cat_uz]
nb_pcts = [gen_nb.get(c,0)/len(nonborrowers)*100 for c in cat_uz]
x = np.arange(len(categories))
w = 0.35
axes[1].bar(x-w/2, b_pcts, w, label='Qarzdorlar', color=colors[0])
axes[1].bar(x+w/2, nb_pcts, w, label='Qarzdor emaslar', color=colors[1])
axes[1].set_xticks(x)
axes[1].set_xticklabels(categories)
axes[1].set_ylabel('%')
axes[1].set_title('Jins boyicha status')
axes[1].legend()
fig.suptitle('3-rasm. Jins boyicha taqsimot', fontsize=14, y=1.02)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_03_gender.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 3 saved: Gender')

# CHART 4: Income distribution
fig, ax = plt.subplots(figsize=(10, 6))
inc_col = '1.10. Уй хўжалигингиз аъзоларининг жами ойлик даромади?'
inc_order = ['5 млн сўмгача', '5-10 млн сўм', '10 - 20 млн сўм', '20-50 млн сўм', '50 млн сўмдан юқори']
inc_b = borrowers[inc_col].value_counts()
inc_nb = nonborrowers[inc_col].value_counts()
x = np.arange(len(inc_order))
w = 0.35
b_vals = [inc_b.get(i,0)/len(borrowers)*100 for i in inc_order]
nb_vals = [inc_nb.get(i,0)/len(nonborrowers)*100 for i in inc_order]
bars1 = ax.bar(x-w/2, b_vals, w, label='Qarzdorlar', color=colors[0])
bars2 = ax.bar(x+w/2, nb_vals, w, label='Qarzdor emaslar', color=colors[1])
ax.set_xticks(x)
ax.set_xticklabels(['< 5 mln', '5-10 mln', '10-20 mln', '20-50 mln', '> 50 mln'], fontsize=10)
ax.set_xlabel('Oylik daromad (sum)')
ax.set_ylabel('Respondentlar ulushi (%)')
ax.set_title('4-rasm. Oylik daromad boyicha taqsimot (N=2,131)')
ax.legend()
for bar in bars1:
    ax.text(bar.get_x()+bar.get_width()/2, bar.get_height()+0.5, f'{bar.get_height():.1f}%', ha='center', fontsize=8)
for bar in bars2:
    ax.text(bar.get_x()+bar.get_width()/2, bar.get_height()+0.5, f'{bar.get_height():.1f}%', ha='center', fontsize=8)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_04_income.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 4 saved: Income distribution')

# CHART 5: Credit type pie
fig, ax = plt.subplots(figsize=(10, 7))
ct = df['kredit_turi'].value_counts()
ct_labels = {
    'oilaviy': 'Oilaviy tadbirkorlik',
    'mikroqarz': 'Mikroqarz',
    'ipoteka': 'Ipoteka',
    'avtokredit': 'Avtokredit',
    'kredit_karta': 'Kredit karta',
    'talim_krediti': 'Talim krediti',
    'istemol_kredit': 'Istemol kredit'
}
labels_list = [ct_labels.get(k, k) for k in ct.index]
explode = [0.05]*len(ct)
ax.pie(ct.values, labels=labels_list, autopct='%1.1f%%', colors=colors[:len(ct)],
       startangle=90, explode=explode, textprops={'fontsize':10})
ax.set_title(f'5-rasm. Kredit turlari boyicha taqsimot (N={len(borrowers)})', fontsize=13)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_05_credit_type.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 5 saved: Credit type')

# CHART 6: Repayment status
fig, ax = plt.subplots(figsize=(10, 6))
holat = df['holat1'].value_counts()
holat_labels = {
    'Қарздорликни ўз вақтида тўлаётган мижозлар': 'Oz vaqtida\n(On-time)',
    'Қарздорликни 1-3 ой кечикиш билан тўлаётган мижозлар': '1-3 oy kechikish\n(1-3 month delay)',
    'Қарздорликни 3 ой ва ундан кўп муддат (NPL) тўламаётган мижозлар': 'NPL (3+ oy)\n(NPL 3+ months)'
}
labels_h = [holat_labels.get(k,k) for k in holat.index]
bar_colors = ['#e74c3c', '#f39c12', '#27ae60']
bars = ax.bar(labels_h, holat.values, color=bar_colors)
ax.set_ylabel('Respondentlar soni')
ax.set_title(f'6-rasm. Tolov holati boyicha taqsimot (N={holat.sum()})')
for bar, val in zip(bars, holat.values):
    pct = val/holat.sum()*100
    ax.text(bar.get_x()+bar.get_width()/2, bar.get_height()+10, f'{val}\n({pct:.1f}%)', ha='center', fontsize=11)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_06_repayment_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 6 saved: Repayment status')

# CHART 7: Income sources comparison
fig, ax = plt.subplots(figsize=(12, 7))
income_cols_idx = {
    'Ish haqi (Zarplata)': 29,
    'Tadbirkorlik (Predprinim.)': 30,
    'Mavsumiy (Sezonnye)': 36,
    'Doimiy yoq (Net dokhoda)': 37,
    'Pensiya': 32,
    'Q.x. (S/kh)': 35,
    'Pul otkazma (Perevody)': 31,
}
inc_labels = list(income_cols_idx.keys())
b_pcts = [borrowers.iloc[:, v].mean()*100 for v in income_cols_idx.values()]
nb_pcts = [nonborrowers.iloc[:, v].mean()*100 for v in income_cols_idx.values()]
y = np.arange(len(inc_labels))
h = 0.35
ax.barh(y-h/2, b_pcts, h, label='Qarzdorlar', color=colors[0])
ax.barh(y+h/2, nb_pcts, h, label='Qarzdor emaslar', color=colors[1])
ax.set_yticks(y)
ax.set_yticklabels(inc_labels)
ax.set_xlabel('Respondentlar ulushi (%)')
ax.set_title('7-rasm. Daromad manbalari (kop javob, N=2,131)')
ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_07_income_sources.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 7 saved: Income sources')

# CHART 8: Education
fig, ax = plt.subplots(figsize=(10, 6))
edu_col = '1.4. Маълумотингиз:'
edu_short = {
    'Ўрта таълим (мактаб, лицей)': 'Orta talim\n(Srednee)',
    'Ўрта махсус, касб-ҳунар таълими (техникум, коллеж)': 'Orta maxsus\n(Srednee spec.)',
    'Олий таълим (бакалавр, магистр в.б.)': 'Oliy talim\n(Vysshee)'
}
edu_order = list(edu_short.keys())
b_pcts = [borrowers[edu_col].value_counts(normalize=True).get(e,0)*100 for e in edu_order]
nb_pcts = [nonborrowers[edu_col].value_counts(normalize=True).get(e,0)*100 for e in edu_order]
x = np.arange(len(edu_order))
w = 0.35
bars1 = ax.bar(x-w/2, b_pcts, w, label='Qarzdorlar', color=colors[0])
bars2 = ax.bar(x+w/2, nb_pcts, w, label='Qarzdor emaslar', color=colors[1])
ax.set_xticks(x)
ax.set_xticklabels([edu_short[e] for e in edu_order])
ax.set_ylabel('%')
ax.set_title('8-rasm. Malumot darajasi boyicha taqsimot (N=2,131)')
ax.legend()
ax.bar_label(bars1, fmt='%.1f%%', fontsize=9)
ax.bar_label(bars2, fmt='%.1f%%', fontsize=9)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_08_education.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 8 saved: Education')

print('\n=== ALL 8 CHARTS SAVED ===')
