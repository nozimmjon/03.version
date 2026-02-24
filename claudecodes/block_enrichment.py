"""
Block enrichment analysis: 5 new blocks for report v5
Generates charts and computes statistics for:
  Block B: Detailed NPL causes (Q3.7)
  Block A: Credit decision factors (Q2.10)
  Block C: Credit rating awareness (Q3.12-3.13)
  Block D: Credit purpose × NPL
  Block E: Payment norms and priorities (Q3.8-3.9)
"""
import sys
sys.stdout.reconfigure(encoding='utf-8')

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import warnings
warnings.filterwarnings('ignore')

# Fix Uzbek text rendering
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['DejaVu Sans', 'Arial', 'Calibri']
plt.rcParams['axes.unicode_minus'] = False

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
out = f"{base}/claude"

df = pd.read_excel(f"{base}/survey_master_cleaned_v1.xlsx", engine='openpyxl')
borrowers = df[df['has_loan']==1].copy()
borrowers['is_npl'] = borrowers['holat1'].apply(
    lambda x: 1 if pd.notna(x) and 'NPL' in str(x) else 0
)
npl_b = borrowers[borrowers['is_npl']==1]
non_npl_b = borrowers[borrowers['is_npl']==0]

print(f"Borrowers: {len(borrowers)}, NPL: {len(npl_b)}, Non-NPL: {len(non_npl_b)}")

colors_main = ['#2196F3', '#FF5722', '#4CAF50', '#FFC107', '#9C27B0', '#00BCD4', '#795548', '#607D8B']

# ============================================================
# CHART 33: Block B — Detailed NPL causes (Q3.7)
# ============================================================
print("\n--- Block B: NPL Causes (Q3.7) ---")
npl_causes = {
    'Даромадларим камайди\n(Снижение доходов)': 196,
    'Кутилмаган харажатлар\n(Непредв. расходы)': 204,
    'Иш ҳақи кечиктирилди\n(Задержка зарплаты)': 197,
    'Мавсумий даромад\n(Сезонный доход)': 199,
    'Имкониятларни нотўғри\nбаҳоладим (Переоценка)': 200,
    'Бошқа қарзларга\nйўналтирилди (Др. долги)': 198,
    'Номимга рухсатимсиз\n(Мошенничество)': 207,
    'Ёширин шартлар\n(Скрытые условия)': 206,
    'Адолатсизлик\n(Несправедливость)': 208,
    'Тўламаса оқибати йўқ\n(Безнаказанность)': 202,
    'Бошқалар тўламайди\n(Подражание)': 201,
    'Имконияти бўлганда\n(При возможности)': 203,
    'Банк хатолиги\n(Ошибка банка)': 205,
}

# Build data from borrowers (all, not just NPL) - these are binary columns
cause_data = []
for label, col_idx in npl_causes.items():
    col = df.columns[col_idx]
    # Count among all borrowers who selected this reason
    total_yes = (borrowers[col]==1).sum()
    npl_yes = (npl_b[col]==1).sum()
    non_npl_yes = (non_npl_b[col]==1).sum()
    cause_data.append({
        'label': label,
        'total': total_yes,
        'npl': npl_yes,
        'non_npl': non_npl_yes,
        'pct_total': total_yes / len(borrowers) * 100,
        'pct_npl': npl_yes / len(npl_b) * 100 if len(npl_b) > 0 else 0,
    })

cause_df = pd.DataFrame(cause_data).sort_values('total', ascending=True)

# Group causes into categories
economic = ['Даромадларим камайди\n(Снижение доходов)', 'Кутилмаган харажатлар\n(Непредв. расходы)',
            'Иш ҳақи кечиктирилди\n(Задержка зарплаты)', 'Мавсумий даромад\n(Сезонный доход)']
behavioral = ['Имкониятларни нотўғри\nбаҳоладим (Переоценка)', 'Бошқа қарзларга\nйўналтирилди (Др. долги)',
              'Имконияти бўлганда\n(При возможности)']
institutional = ['Номимга рухсатимсиз\n(Мошенничество)', 'Ёширин шартлар\n(Скрытые условия)',
                 'Адолатсизлик\n(Несправедливость)', 'Банк хатолиги\n(Ошибка банка)']
moral_hazard = ['Тўламаса оқибати йўқ\n(Безнаказанность)', 'Бошқалар тўламайди\n(Подражание)']

fig, ax = plt.subplots(figsize=(12, 8))
bar_colors = []
for label in cause_df['label']:
    if label in economic:
        bar_colors.append('#2196F3')
    elif label in behavioral:
        bar_colors.append('#FF9800')
    elif label in institutional:
        bar_colors.append('#F44336')
    else:
        bar_colors.append('#9C27B0')

bars = ax.barh(range(len(cause_df)), cause_df['pct_npl'].values, color=bar_colors, height=0.7)
ax.set_yticks(range(len(cause_df)))
ax.set_yticklabels(cause_df['label'].values, fontsize=8)
ax.set_xlabel('NPL гуруҳида улуши (%)', fontsize=11)
ax.set_title('Расм 26. NPL сабаблари: батафсил таҳлил (N=1 525)', fontsize=13, fontweight='bold')

# Add value labels
for i, (val, total) in enumerate(zip(cause_df['pct_npl'].values, cause_df['npl'].values)):
    ax.text(val + 0.5, i, f'{val:.1f}% (n={total})', va='center', fontsize=8)

# Legend
from matplotlib.patches import Patch
legend_elements = [
    Patch(facecolor='#2196F3', label='Иқтисодий (Экономические)'),
    Patch(facecolor='#FF9800', label='Хулқий (Поведенческие)'),
    Patch(facecolor='#F44336', label='Институционал (Институцион.)'),
    Patch(facecolor='#9C27B0', label='Маънавий хавф (Мор. риск)'),
]
ax.legend(handles=legend_elements, loc='lower right', fontsize=9)
plt.tight_layout()
plt.savefig(f'{out}/chart_33_npl_causes_detailed.png', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved chart_33_npl_causes_detailed.png")

# Print summary stats for report
print("\n  NPL causes summary (sorted by frequency among NPL group):")
for _, row in cause_df.sort_values('npl', ascending=False).iterrows():
    print(f"    {row['label'].replace(chr(10), ' ')}: N={row['npl']}, {row['pct_npl']:.1f}%")

# ============================================================
# CHART 34: Block A — Decision factors (Q2.10)
# ============================================================
print("\n--- Block A: Decision Factors (Q2.10) ---")
decision_factors = {
    'Зарурият туфайли\n(Необходимость)': 163,
    'Имтиёз мавжудлиги\n(Наличие льготы)': 164,
    'Танишлар тавсияси\n(Рекомендация знакомых)': 165,
    'Ҳокимият тавсияси\n(Рекомендация власти)': 166,
    'Алданиб олган\n(Был обманут)': 167,
    'Кредит тарихи яхшиланади\n(Улучшение КИ)': 168,
    'Инфляциядан хавотир\n(Страх инфляции)': 169,
    'Бошқа\n(Другое)': 170,
}

factor_data = []
for label, col_idx in decision_factors.items():
    col = df.columns[col_idx]
    total = (borrowers[col]==1).sum()
    npl_yes = (npl_b[col]==1).sum()
    non_npl_yes = (non_npl_b[col]==1).sum()
    npl_rate = npl_yes / total * 100 if total > 0 else 0
    factor_data.append({
        'label': label,
        'total': total,
        'pct': total / len(borrowers) * 100,
        'npl': npl_yes,
        'non_npl': non_npl_yes,
        'npl_rate': npl_rate,
    })

factor_df = pd.DataFrame(factor_data).sort_values('total', ascending=True)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# Left: frequency
bars1 = ax1.barh(range(len(factor_df)), factor_df['pct'].values, color='#2196F3', height=0.6)
ax1.set_yticks(range(len(factor_df)))
ax1.set_yticklabels(factor_df['label'].values, fontsize=9)
ax1.set_xlabel('Қарз олувчилар орасида улуши (%)', fontsize=10)
ax1.set_title('a) Ҳар бир омил частотаси', fontsize=11, fontweight='bold')
for i, (val, n) in enumerate(zip(factor_df['pct'].values, factor_df['total'].values)):
    ax1.text(val + 0.5, i, f'{val:.1f}% (n={n})', va='center', fontsize=8)

# Right: NPL rate by factor
colors_npl = ['#F44336' if r > 60 else '#FF9800' if r > 52 else '#4CAF50' for r in factor_df['npl_rate'].values]
bars2 = ax2.barh(range(len(factor_df)), factor_df['npl_rate'].values, color=colors_npl, height=0.6)
ax2.set_yticks(range(len(factor_df)))
ax2.set_yticklabels(factor_df['label'].values, fontsize=9)
ax2.set_xlabel('NPL даражаси (%)', fontsize=10)
ax2.set_title('б) NPL даражаси ҳар бир омил бўйича', fontsize=11, fontweight='bold')
ax2.axvline(x=51.9, color='gray', linestyle='--', alpha=0.5, label=f'Умумий NPL={51.9:.1f}%')
for i, val in enumerate(factor_df['npl_rate'].values):
    ax2.text(val + 0.5, i, f'{val:.1f}%', va='center', fontsize=8)
ax2.legend(fontsize=8)

plt.suptitle('Расм 27. Кредит олиш қарорига таъсир кўрсатган омиллар (N=1 525)', fontsize=13, fontweight='bold')
plt.tight_layout()
plt.savefig(f'{out}/chart_34_decision_factors.png', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved chart_34_decision_factors.png")

for _, r in factor_df.sort_values('total', ascending=False).iterrows():
    print(f"    {r['label'].replace(chr(10), ' ')}: N={r['total']} ({r['pct']:.1f}%), NPL rate={r['npl_rate']:.1f}%")

# ============================================================
# CHART 35: Block C — Credit Rating Awareness
# ============================================================
print("\n--- Block C: Credit Rating Awareness ---")

# Q3.12: Does knowing credit rating improvement motivate discipline?
q312 = borrowers[df.columns[244]]
q312_data = {
    'Ҳа, кучли\nрағбатлантиради\n(Сильно мотивирует)': 'Ҳа, кучли рағбатлантиради',
    'Ҳа, қисман\nрағбатлантиради\n(Частично мотивирует)': 'Ҳа, қисман рағбатлантиради',
    'Йўқ\n(Нет)': 'Йўқ',
    'Маълумотга эга\nэмасман\n(Не осведомлён)': 'Кредит рейтинги ҳақида маълумотга эга эмасман',
    'Иккиланаман\n(Сомневаюсь)': 'Иккиланаман',
}

# Q3.13: Knowledge of late payment consequences
q313 = borrowers[df.columns[245]]
q313_data = {
    'Ҳа, аниқ биламан\n(Знаю точно)': 'Ҳа, аниқ биламан',
    'Қисман биламан\n(Знаю частично)': 'Қисман биламан',
    'Йўқ, билмайман\n(Не знаю)': 'Йўқ, билмайман',
    'Жавоб бериш қийин\n(Затрудняюсь)': 'Жавоб бериш қийин (ўқилмасин)',
}

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 7))

# Left: Q3.12 with NPL rates
labels_312 = []
npl_rates_312 = []
non_npl_rates_312 = []
sizes_312 = []
for label, value in q312_data.items():
    mask = q312 == value
    n = mask.sum()
    npl_n = (borrowers.loc[mask.values, 'is_npl'] == 1).sum() if n > 0 else 0
    npl_rate = npl_n / n * 100 if n > 0 else 0
    labels_312.append(label)
    npl_rates_312.append(npl_rate)
    sizes_312.append(n)

bar_colors_312 = ['#4CAF50', '#8BC34A', '#FF9800', '#F44336', '#9E9E9E']
y_pos = range(len(labels_312))
bars = ax1.barh(y_pos, npl_rates_312, color=bar_colors_312, height=0.6)
ax1.set_yticks(y_pos)
ax1.set_yticklabels(labels_312, fontsize=8)
ax1.set_xlabel('NPL даражаси (%)', fontsize=10)
ax1.set_title('а) Кредит рейтинги билими ва NPL', fontsize=11, fontweight='bold')
ax1.axvline(x=51.9, color='gray', linestyle='--', alpha=0.5)
for i, (val, n) in enumerate(zip(npl_rates_312, sizes_312)):
    ax1.text(val + 0.5, i, f'{val:.1f}% (n={n})', va='center', fontsize=8)

# Right: Q3.13 with NPL rates
labels_313 = []
npl_rates_313 = []
sizes_313 = []
for label, value in q313_data.items():
    mask = q313 == value
    n = mask.sum()
    npl_n = (borrowers.loc[mask.values, 'is_npl'] == 1).sum() if n > 0 else 0
    npl_rate = npl_n / n * 100 if n > 0 else 0
    labels_313.append(label)
    npl_rates_313.append(npl_rate)
    sizes_313.append(n)

bar_colors_313 = ['#4CAF50', '#FF9800', '#F44336', '#9E9E9E']
y_pos = range(len(labels_313))
bars = ax2.barh(y_pos, npl_rates_313, color=bar_colors_313, height=0.6)
ax2.set_yticks(y_pos)
ax2.set_yticklabels(labels_313, fontsize=8)
ax2.set_xlabel('NPL даражаси (%)', fontsize=10)
ax2.set_title('б) Кечикиш оқибатларини билиш ва NPL', fontsize=11, fontweight='bold')
ax2.axvline(x=51.9, color='gray', linestyle='--', alpha=0.5)
for i, (val, n) in enumerate(zip(npl_rates_313, sizes_313)):
    ax2.text(val + 0.5, i, f'{val:.1f}% (n={n})', va='center', fontsize=8)

plt.suptitle('Расм 28. Кредит рейтинги ва кечикиш оқибатлари ҳақида хабардорлик (N=1 525)', fontsize=13, fontweight='bold')
plt.tight_layout()
plt.savefig(f'{out}/chart_35_credit_rating_awareness.png', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved chart_35_credit_rating_awareness.png")

for l, r, n in zip(labels_312, npl_rates_312, sizes_312):
    print(f"    Q3.12 {l.replace(chr(10), ' ')}: N={n}, NPL={r:.1f}%")
for l, r, n in zip(labels_313, npl_rates_313, sizes_313):
    print(f"    Q3.13 {l.replace(chr(10), ' ')}: N={n}, NPL={r:.1f}%")

# ============================================================
# CHART 36: Block D — Credit Purpose × NPL
# ============================================================
print("\n--- Block D: Credit Purpose × NPL ---")

purposes = {
    'Кундалик харажатлар\n(Повседн. расходы)': 119,
    'Соғлиқ\n(Здоровье)': 120,
    'Маиший техника\n(Бытовая техн.)': 121,
    'Автомобил\n(Автомобиль)': 122,
    'Таълим\n(Образование)': 123,
    'Тўй/маросим\n(Свадьба/торж.)': 124,
    'Ипотека\n(Ипотека)': 125,
    'Уй таъмирлаш\n(Ремонт жилья)': 126,
    'Қарз тўлаш\n(Погашение долга)': 127,
    'Бизнес\n(Бизнес)': 129,
    'Оилавий тадбиркорлик\n(Сем. предприн.)': 130,
    'Яқинларга ёрдам\n(Помощь близким)': 131,
}

purpose_data = []
for label, col_idx in purposes.items():
    col = df.columns[col_idx]
    mask = borrowers[col] == 1
    total = mask.sum()
    if total >= 20:
        npl_n = (borrowers.loc[mask, 'is_npl'] == 1).sum()
        npl_rate = npl_n / total * 100
        purpose_data.append({
            'label': label,
            'total': total,
            'npl': npl_n,
            'npl_rate': npl_rate,
        })

purpose_df = pd.DataFrame(purpose_data).sort_values('npl_rate', ascending=True)

fig, ax = plt.subplots(figsize=(12, 7))
colors_purpose = ['#F44336' if r > 55 else '#FF9800' if r > 50 else '#4CAF50' for r in purpose_df['npl_rate'].values]
bars = ax.barh(range(len(purpose_df)), purpose_df['npl_rate'].values, color=colors_purpose, height=0.6)
ax.set_yticks(range(len(purpose_df)))
ax.set_yticklabels(purpose_df['label'].values, fontsize=9)
ax.set_xlabel('NPL даражаси (%)', fontsize=11)
ax.set_title('Расм 29. NPL даражаси кредит мақсади бўйича (N=1 525)', fontsize=13, fontweight='bold')
ax.axvline(x=51.9, color='gray', linestyle='--', alpha=0.7, label=f'Умумий NPL = 51.9%')

for i, (val, n) in enumerate(zip(purpose_df['npl_rate'].values, purpose_df['total'].values)):
    ax.text(val + 0.5, i, f'{val:.1f}% (n={n})', va='center', fontsize=9)

ax.legend(fontsize=10)
plt.tight_layout()
plt.savefig(f'{out}/chart_36_purpose_npl.png', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved chart_36_purpose_npl.png")

for _, r in purpose_df.sort_values('npl_rate', ascending=False).iterrows():
    print(f"    {r['label'].replace(chr(10), ' ')}: N={r['total']}, NPL={r['npl_rate']:.1f}%")

# Targeted use (Q2.8)
col_targeted = df.columns[159]
ct = pd.crosstab(borrowers[col_targeted], borrowers['is_npl'])
print(f"\n  Targeted use cross-tab:")
for val in ct.index:
    row = ct.loc[val]
    total = row.sum()
    npl_pct = row[1] / total * 100 if total > 0 else 0
    print(f"    {val}: N={total}, NPL={npl_pct:.1f}%")

# ============================================================
# CHART 37: Block E — Payment Norms
# ============================================================
print("\n--- Block E: Payment Norms (Q3.8) ---")

col_norms = df.columns[214]
norms_ct = pd.crosstab(borrowers[col_norms], borrowers['is_npl'])
norm_order = ['Ҳеч қандай кечикиш мумкин (нормал) эмас', '1-7 кун', '8-30 кун', '30 кундан ортиқ', 'Тўламаса ҳам бўлади']
norm_labels = ['Кечикиш мумкин эмас\n(Нельзя)', '1-7 кун\n(1-7 дней)', '8-30 кун\n(8-30 дней)', '30+ кун\n(30+ дней)', 'Тўламаса ҳам\nбўлади (Можно)']

norm_data = []
for val, label in zip(norm_order, norm_labels):
    if val in norms_ct.index:
        row = norms_ct.loc[val]
        total = row.sum()
        npl_n = row[1] if 1 in row.index else 0
        npl_rate = npl_n / total * 100 if total > 0 else 0
        norm_data.append({'label': label, 'value': val, 'total': total, 'npl': npl_n, 'npl_rate': npl_rate})

norm_df = pd.DataFrame(norm_data)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# Left: distribution
ax1.bar(range(len(norm_df)), norm_df['total'].values, color='#2196F3')
ax1.set_xticks(range(len(norm_df)))
ax1.set_xticklabels(norm_df['label'].values, fontsize=8)
ax1.set_ylabel('Респондентлар сони', fontsize=10)
ax1.set_title('а) Қабул қилинган кечикиш меъёри', fontsize=11, fontweight='bold')
for i, (val, n) in enumerate(zip(norm_df['total'].values, norm_df['total'].values)):
    ax1.text(i, val + 10, f'n={n}', ha='center', fontsize=9)

# Right: NPL rate
colors_norm = ['#4CAF50', '#8BC34A', '#FF9800', '#F44336', '#B71C1C']
ax2.bar(range(len(norm_df)), norm_df['npl_rate'].values, color=colors_norm)
ax2.set_xticks(range(len(norm_df)))
ax2.set_xticklabels(norm_df['label'].values, fontsize=8)
ax2.set_ylabel('NPL даражаси (%)', fontsize=10)
ax2.set_title('б) NPL даражаси кечикиш меъёри бўйича', fontsize=11, fontweight='bold')
ax2.axhline(y=51.9, color='gray', linestyle='--', alpha=0.7, label='Умумий NPL')
for i, val in enumerate(norm_df['npl_rate'].values):
    ax2.text(i, val + 1, f'{val:.1f}%', ha='center', fontsize=9)
ax2.legend(fontsize=9)

plt.suptitle('Расм 30. Тўлов кечикиши меъёрлари ва NPL (N=1 525)', fontsize=13, fontweight='bold')
plt.tight_layout()
plt.savefig(f'{out}/chart_37_payment_norms.png', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved chart_37_payment_norms.png")

for _, r in norm_df.iterrows():
    print(f"    {r['label'].replace(chr(10), ' ')}: N={r['total']}, NPL={r['npl_rate']:.1f}%")

# Payment priorities (Q3.9 cols 216-221)
print("\n--- Block E: Payment Priorities (Q3.9) ---")
priorities = {
    'Банк ташкилотлари\n(Банки)': 216,
    'Яқинлардан олинган\nқарз (Долг близким)': 217,
    'Норасмий шахслар\n(Неформ. кредиторы)': 218,
    'Расмий насия\n(Формальная рассрочка)': 219,
    'Норасмий насия\n(Неформ. рассрочка)': 220,
    'Микромолия\n(Микрофинансы)': 221,
}

prio_data = []
for label, col_idx in priorities.items():
    col = df.columns[col_idx]
    n = (borrowers[col]==1).sum()
    prio_data.append({'label': label, 'n': n, 'pct': n / len(borrowers) * 100})

prio_df = pd.DataFrame(prio_data).sort_values('n', ascending=True)

fig, ax = plt.subplots(figsize=(10, 5))
bars = ax.barh(range(len(prio_df)), prio_df['pct'].values, color='#2196F3', height=0.6)
ax.set_yticks(range(len(prio_df)))
ax.set_yticklabels(prio_df['label'].values, fontsize=9)
ax.set_xlabel('Респондентлар улуши (%)', fontsize=10)
ax.set_title('Расм 31. Биринчи навбатда тўланиши керак бўлган қарзлар (N=1 525)', fontsize=12, fontweight='bold')
for i, (val, n) in enumerate(zip(prio_df['pct'].values, prio_df['n'].values)):
    ax.text(val + 0.3, i, f'{val:.1f}% (n={n})', va='center', fontsize=9)
plt.tight_layout()
plt.savefig(f'{out}/chart_38_payment_priorities.png', dpi=300, bbox_inches='tight')
plt.close()
print("  Saved chart_38_payment_priorities.png")

for _, r in prio_df.sort_values('n', ascending=False).iterrows():
    print(f"    {r['label'].replace(chr(10), ' ')}: N={r['n']} ({r['pct']:.1f}%)")

print("\n=== ALL CHARTS GENERATED SUCCESSFULLY ===")
