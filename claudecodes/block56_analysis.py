import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os
import warnings
warnings.filterwarnings('ignore')

plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.size'] = 12
colors = plt.cm.Set2.colors

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
out = os.path.join(base, "claude")
df = pd.read_excel(os.path.join(base, 'survey_master_cleaned_v1.xlsx'), engine='openpyxl')
borrowers = df[df['has_loan']==1].copy()
N = len(df)

npl = df[df['holat1'].str.contains('NPL', na=False)]
ontime = df[df['holat1'].str.contains('вақтида', na=False)]
delayed = df[df['holat1'].str.contains('1-3', na=False)]

print('='*80)
print('BLOCK 5: ECONOMETRIC MODEL PREP')
print('='*80)

# Build model dataset for borrowers only
age_col = '1.1. Ёшингиз:'
gender_col = '1.2. Респондент жинси'
marital_col = '1.3. Оилавий ҳолатингиз:'
edu_col = '1.4. Маълумотингиз:'
emp_col = '1.5. Қуйидагилардан қайси бири сизнинг бандлик ҳолатингизни ифодалайди?'
hh_col = df.columns[25]  # 1.6 household size
employed_col = df.columns[26]  # 1.7 employed members
children_col = df.columns[27]  # 1.8 children
inc_col = df.columns[40]  # 1.10 income
dti_col = df.columns[105]  # 2.4.2
vol_col = df.columns[107]  # 2.4.4
contract_col = df.columns[192]  # 3.4
terms_col = df.columns[193]  # 3.5

# Create binary target: 1 = NPL, 0 = not NPL (on-time or 1-3m)
borrowers['is_npl'] = (df.loc[borrowers.index, 'holat1'].str.contains('NPL', na=False)).astype(int)
borrowers['is_male'] = (borrowers[gender_col] == 'Эркак').astype(int)
borrowers['is_married'] = (borrowers[marital_col] == 'Турмуш қурган').astype(int)
borrowers['has_higher_edu'] = (borrowers[edu_col].str.contains('Олий', na=False)).astype(int)
borrowers['age'] = borrowers[age_col]

# Income encoding
inc_map = {
    '5 млн сўмгача': 1,
    '5-10 млн сўм': 2,
    '10 - 20 млн сўм': 3,
    '20-50 млн сўм': 4,
    '50 млн сўмдан юқори': 5,
}
borrowers['income_level'] = borrowers[inc_col].map(inc_map)

# DTI encoding
dti_map = {
    '10% гача': 1,
    '10% дан – 20% гача': 2,
    '20% дан – 35% гача': 3,
    '35% дан – 50% гача': 4,
    '50%дан кўп': 5,
}
borrowers['dti_level'] = borrowers[dti_col].map(dti_map)

# Volatility encoding
vol_map = {
    'Барқарор, деярли ўзгармайди': 1,
    'Бироз ўзгариб туради': 2,
    'Жуда ўзгарувчан/мавсумий': 3,
}
borrowers['volatility'] = borrowers[vol_col].map(vol_map)

# Contract awareness
contract_map = {
    'Ҳа, тўлиқ танишганман': 1,
    'Ҳа, қисман танишганман': 0,
    'Танишмаганман': 0,
}
borrowers['read_contract'] = borrowers[contract_col].map(contract_map)

# Income source flags
borrowers['has_wage'] = df.loc[borrowers.index, df.columns[29]]
borrowers['has_business'] = df.loc[borrowers.index, df.columns[30]]
borrowers['has_seasonal'] = df.loc[borrowers.index, df.columns[36]]
borrowers['no_stable_income'] = df.loc[borrowers.index, df.columns[37]]
borrowers['household_size'] = borrowers[hh_col]
borrowers['employed_members'] = borrowers[employed_col]
borrowers['children'] = borrowers[children_col]

# Model variables
model_vars = ['is_npl', 'age', 'is_male', 'is_married', 'has_higher_edu', 'income_level',
              'dti_level', 'volatility', 'read_contract', 'has_wage', 'has_business',
              'has_seasonal', 'no_stable_income', 'household_size', 'employed_members', 'children']

model_df = borrowers[model_vars].dropna()
print(f'\nModel dataset: N={len(model_df)} (after dropping NaN)')
print('\nDescriptive statistics:')
print(model_df.describe().round(2).to_string())

print(f'\nNPL rate in model sample: {model_df["is_npl"].mean()*100:.1f}%')

# Correlation with NPL
print('\nCorrelation with NPL:')
corr = model_df.corr()['is_npl'].drop('is_npl').sort_values()
for var, r in corr.items():
    print(f'  {var}: r = {r:.3f}')

# Try logistic regression
try:
    from sklearn.linear_model import LogisticRegression
    from sklearn.preprocessing import StandardScaler

    X = model_df.drop('is_npl', axis=1)
    y = model_df['is_npl']

    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)

    model = LogisticRegression(max_iter=1000, random_state=42)
    model.fit(X_scaled, y)

    print(f'\nLogistic Regression Results:')
    print(f'Accuracy: {model.score(X_scaled, y)*100:.1f}%')

    coef_df = pd.DataFrame({
        'Variable': X.columns,
        'Coefficient': model.coef_[0],
        'Odds_Ratio': np.exp(model.coef_[0])
    }).sort_values('Coefficient')
    print('\nCoefficients and Odds Ratios:')
    print(coef_df.to_string(index=False))

    # CHART 24: Coefficient plot
    fig, ax = plt.subplots(figsize=(10, 7))
    coef_sorted = coef_df.sort_values('Coefficient')
    bar_colors = ['#27ae60' if c < 0 else '#e74c3c' for c in coef_sorted['Coefficient']]
    ax.barh(range(len(coef_sorted)), coef_sorted['Coefficient'], color=bar_colors)
    ax.set_yticks(range(len(coef_sorted)))
    ax.set_yticklabels(coef_sorted['Variable'], fontsize=9)
    ax.axvline(x=0, color='black', linewidth=0.5)
    ax.set_xlabel('Coefficient (standardized)')
    ax.set_title(f'24-rasm. NPL risk omillari - logistik regressiya (N={len(model_df)})')
    plt.tight_layout()
    plt.savefig(os.path.join(out, 'chart_24_logistic_coef.png'), dpi=300, bbox_inches='tight')
    plt.close()
    print('Chart 24 saved')

except ImportError:
    print('\nsklearn not installed - skipping logistic regression')
    print('Install with: python -m pip install scikit-learn')

print('\n' + '='*80)
print('BLOCK 6: DEBT COLLECTION')
print('='*80)

# === 3.2 Acceptable delay reasons (cols 174-183) ===
print(f'\n### 3.2 ACCEPTABLE DELAY REASONS (all respondents, N={N})')
delay_labels = [
    'Ish haqi kechiktirilishi (Zaderzhka zarplaty)',
    'Tanishlar nomiga olgan (Za drugikh)',
    'Firibgarlar (Moshenniki)',
    'Daromad yoqolishi (Poterya dokhoda)',
    'Huquqiy nizolar (Yurid. spory)',
    'Kutilmagan xarajat (Neozhid. raskhody)',
    'Boshqa qarzlar (Drugie dolgi)',
    'Esdan chiqib qolishi (Zabyil)',
    'Qaytarish shart emas (Ne nuzhno vozvrashchat)',
    'Boshqa (Drugoe)',
]
for i, label in enumerate(delay_labels):
    col = df.columns[174+i]
    n = df[col].sum()
    pct = n/N*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 3.3 Who to notify (cols 185-191) ===
print(f'\n### 3.3 WHO SHOULD BANK NOTIFY ON DELAY (all, N={N})')
notify_labels = ['Kafil (Poruchitel)', 'Turmush ortog (Suprug/a)', 'Ota-ona (Roditeli)',
                 'Voyaga etgan farzand (Vzroslyy rebenok)', 'Boshqa oila (Drugiye chlen semyi)',
                 'Ish beruvchi (Rabotodatel)', 'Yaqin dost (Blizkiy drug)']
for i, label in enumerate(notify_labels):
    col = df.columns[185+i]
    n = df[col].sum()
    pct = n/N*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 3.9 Repayment priority (cols 216-221) ===
print(f'\n### 3.9 REPAYMENT PRIORITY (borrowers, N={len(borrowers)})')
priority_labels = ['Bank', 'Yaqinlardan qarz', 'Norasmiy shaxslar', 'Rasmiy nasiya', 'Norasmiy nasiya', 'MFO']
for i, label in enumerate(priority_labels):
    col = df.columns[216+i]
    n = borrowers[col].sum()
    pct = n/len(borrowers)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 3.10 Credit in relatives names ===
rel_col = df.columns[222]  # 3.10
print(f'\n### 3.10 CREDIT REGISTERED IN RELATIVES NAMES')
print(borrowers[rel_col].value_counts().to_string())

# === 3.11 Reminder frequency ===
rem_col = df.columns[223]  # 3.11
print(f'\n### 3.11 REMINDER FREQUENCY (borrowers)')
print(borrowers[rem_col].value_counts().to_string())

# === 3.11.1 Collection methods applied (cols 225-233) ===
print(f'\n### 3.11.1 COLLECTION METHODS APPLIED (borrowers)')
coll_labels = ['SMS eslatma', 'Telefon qongiroq', 'Bank xodimi kelishi', 'MIB xodimi',
               'Kafil bilan boglanish', 'Rasmiy ogolantirish', 'Sud jarayoni',
               'Garov choralari', 'Umuman qollanilmagan']
for i, label in enumerate(coll_labels):
    col = df.columns[225+i]
    n = borrowers[col].sum()
    pct = n/len(borrowers)*100
    # By status
    n_npl = npl[col].sum()
    n_on = ontime[col].sum()
    print(f'  {label}: N={n} ({pct:.1f}%) | NPL={n_npl/len(npl)*100:.1f}% | On-time={n_on/len(ontime)*100:.1f}%')

# === 3.11.2 Most effective reminders (cols 235-242) ===
print(f'\n### 3.11.2 MOST EFFECTIVE REMINDERS (borrowers)')
eff_labels = ['SMS', 'Telefon', 'Bank ilovasi', 'Messenger', 'Kafil qongiroq',
              'Shaxsan tushuntirish', 'Boshqa', 'Samarasi yoq']
for i, label in enumerate(eff_labels):
    col = df.columns[235+i]
    n = borrowers[col].sum()
    pct = n/len(borrowers)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# CHART 25: Collection methods by status
fig, ax = plt.subplots(figsize=(12, 7))
methods = coll_labels[:8]  # exclude "not applied"
m_cols = list(range(225, 233))
npl_pcts = [npl.iloc[:, c].mean()*100 for c in m_cols]
on_pcts = [ontime.iloc[:, c].mean()*100 for c in m_cols]
y = np.arange(len(methods))
h = 0.35
ax.barh(y-h/2, npl_pcts, h, label=f'NPL (N={len(npl)})', color='#e74c3c')
ax.barh(y+h/2, on_pcts, h, label=f'Oz vaqtida (N={len(ontime)})', color='#27ae60')
ax.set_yticks(y)
ax.set_yticklabels(methods, fontsize=9)
ax.set_xlabel('%')
ax.set_title('25-rasm. Undirish usullari tolov holati boyicha')
ax.legend()
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_25_collection_by_status.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 25 saved')

# CHART 26: Effective reminders
fig, ax = plt.subplots(figsize=(10, 6))
e_vals = [borrowers.iloc[:, 235+i].sum() for i in range(8)]
e_pcts = [v/len(borrowers)*100 for v in e_vals]
sorted_idx = np.argsort(e_pcts)[::-1]
s_labels = [eff_labels[i] for i in sorted_idx]
s_pcts = [e_pcts[i] for i in sorted_idx]
s_vals = [e_vals[i] for i in sorted_idx]
y = range(len(s_labels))
bars = ax.barh(y, s_pcts, color=colors[4])
ax.set_yticks(y)
ax.set_yticklabels(s_labels)
for i, (v, p) in enumerate(zip(s_vals, s_pcts)):
    ax.text(p+0.3, i, f'{p:.1f}% (N={v})', va='center', fontsize=9)
ax.set_xlabel('%')
ax.set_title(f'26-rasm. Eng samarali eslatma vositalari (N={len(borrowers)})')
ax.invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_26_effective_reminders.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 26 saved')

print('\n=== BLOCKS 5-6 COMPLETE ===')
