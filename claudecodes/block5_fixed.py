"""
Block 5 FIXED: Logistic regression with corrected mapping
Fixes:
1. DTI: '10% гача' -> '10% дан – кам'
2. DTI: added '0% (тўлов қилмайман)' -> 0
3. Volatility: added 'Жавоб бериш қийин' -> median impute or exclude
4. Income: added 'Жавоб беришга қийналаман' -> exclude
"""

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

print('='*80)
print('BLOCK 5 FIXED: LOGISTIC REGRESSION WITH CORRECTED MAPPING')
print('='*80)
print(f'\nTotal borrowers: N={len(borrowers)}')

# Column references
age_col = '1.1. Ёшингиз:'
gender_col = '1.2. Респондент жинси'
marital_col = '1.3. Оилавий ҳолатингиз:'
edu_col = '1.4. Маълумотингиз:'
inc_col = df.columns[40]
dti_col = df.columns[105]
vol_col = df.columns[107]
contract_col = df.columns[192]

# === TARGET ===
borrowers['is_npl'] = (df.loc[borrowers.index, 'holat1'].str.contains('NPL', na=False)).astype(int)

# === DEMOGRAPHICS (no NaN) ===
borrowers['is_male'] = (borrowers[gender_col] == 'Эркак').astype(int)
borrowers['is_married'] = (borrowers[marital_col] == 'Турмуш қурган').astype(int)
borrowers['has_higher_edu'] = (borrowers[edu_col].str.contains('Олий', na=False)).astype(int)
borrowers['age'] = borrowers[age_col]

# === INCOME LEVEL — FIXED ===
inc_map = {
    '5 млн сўмгача': 1,
    '5-10 млн сўм': 2,
    '10 - 20 млн сўм': 3,
    '20-50 млн сўм': 4,
    '50 млн сўмдан юқори': 5,
    # 'Жавоб беришга қийналаман' -> NaN (exclude, only 62 cases)
}
borrowers['income_level'] = borrowers[inc_col].map(inc_map)

# === DTI LEVEL — FIXED ===
dti_map = {
    '0% (тўлов қилмайман)': 0,           # FIX 2: Added (was missing)
    '10% дан – кам': 1,                   # FIX 1: Corrected key (was '10% гача')
    '10% дан – 20% гача': 2,
    '20% дан – 35% гача': 3,
    '35% дан – 50%гача': 4,
    '50%дан кўп': 5,
    # 'Жавоб бериш қийин' -> NaN (exclude, only 53 cases)
}
borrowers['dti_level'] = borrowers[dti_col].map(dti_map)

# === VOLATILITY — FIXED ===
vol_map = {
    'Барқарор, деярли ўзгармайди': 1,
    'Бироз ўзгариб туради': 2,
    'Жуда ўзгарувчан/мавсумий': 3,
    # 'Жавоб бериш қийин' -> NaN (exclude, only 89 cases)
}
borrowers['volatility'] = borrowers[vol_col].map(vol_map)

# === CONTRACT ===
contract_map = {
    'Ҳа, тўлиқ танишганман': 1,
    'Ҳа, қисман танишганман': 0,
    'Танишмаганман': 0,
}
borrowers['read_contract'] = borrowers[contract_col].map(contract_map)

# === INCOME SOURCE FLAGS (no NaN) ===
borrowers['has_wage'] = df.loc[borrowers.index, df.columns[29]]
borrowers['has_business'] = df.loc[borrowers.index, df.columns[30]]
borrowers['has_seasonal'] = df.loc[borrowers.index, df.columns[36]]
borrowers['no_stable_income'] = df.loc[borrowers.index, df.columns[37]]

# === HOUSEHOLD (no NaN) ===
borrowers['household_size'] = borrowers[df.columns[25]]
borrowers['employed_members'] = borrowers[df.columns[26]]
borrowers['children'] = borrowers[df.columns[27]]

# === BUILD MODEL DATASET ===
model_vars = ['is_npl', 'age', 'is_male', 'is_married', 'has_higher_edu', 'income_level',
              'dti_level', 'volatility', 'read_contract', 'has_wage', 'has_business',
              'has_seasonal', 'no_stable_income', 'household_size', 'employed_members', 'children']

model_df_before = borrowers[model_vars]

print('\n=== MISSING VALUES AFTER FIX ===')
print(f"{'Variable':<25} {'Non-null':>10} {'Missing':>10} {'Missing%':>10}")
print('-'*60)
for col in model_vars:
    non_null = model_df_before[col].notna().sum()
    missing = model_df_before[col].isna().sum()
    pct = missing/len(model_df_before)*100
    flag = ' <<<' if missing > 0 else ''
    print(f'{col:<25} {non_null:>10} {missing:>10} {pct:>9.1f}%{flag}')

model_df = model_df_before.dropna()
print(f'\n=== SAMPLE SIZE ===')
print(f'Before dropna: {len(model_df_before)}')
print(f'After dropna:  {len(model_df)}')
print(f'Lost:          {len(model_df_before) - len(model_df)} ({(len(model_df_before)-len(model_df))/len(model_df_before)*100:.1f}%)')
print(f'NPL rate:      {model_df["is_npl"].mean()*100:.1f}%')

# === COMPARE OLD vs NEW ===
print(f'\n=== COMPARISON: OLD vs NEW ===')
print(f'OLD sample: N=823')
print(f'NEW sample: N={len(model_df)}')
print(f'GAIN:       +{len(model_df)-823} observations ({(len(model_df)-823)/823*100:.1f}% more)')

# === DESCRIPTIVE STATISTICS ===
print('\n=== DESCRIPTIVE STATISTICS ===')
print(model_df.describe().round(3).to_string())

# === CORRELATION WITH NPL ===
print('\n=== CORRELATION WITH NPL ===')
corr = model_df.corr()['is_npl'].drop('is_npl').sort_values()
for var, r in corr.items():
    sig = '***' if abs(r) > 0.1 else '**' if abs(r) > 0.05 else ''
    print(f'  {var:<25} r = {r:+.4f} {sig}')

# === LOGISTIC REGRESSION ===
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import cross_val_score
from sklearn.metrics import classification_report, confusion_matrix

X = model_df.drop('is_npl', axis=1)
y = model_df['is_npl']

scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

model = LogisticRegression(max_iter=1000, random_state=42)
model.fit(X_scaled, y)

# Cross-validation
cv_scores = cross_val_score(model, X_scaled, y, cv=5, scoring='accuracy')

print(f'\n=== LOGISTIC REGRESSION RESULTS ===')
print(f'Training accuracy: {model.score(X_scaled, y)*100:.1f}%')
print(f'5-fold CV accuracy: {cv_scores.mean()*100:.1f}% (+/- {cv_scores.std()*100:.1f}%)')

# Classification report
y_pred = model.predict(X_scaled)
print(f'\nClassification Report:')
print(classification_report(y, y_pred, target_names=['Non-NPL', 'NPL']))

# Confusion matrix
cm = confusion_matrix(y, y_pred)
print(f'Confusion Matrix:')
print(f'                Predicted Non-NPL  Predicted NPL')
print(f'  Actual Non-NPL    {cm[0][0]:>10}    {cm[0][1]:>10}')
print(f'  Actual NPL        {cm[1][0]:>10}    {cm[1][1]:>10}')

# Coefficients and Odds Ratios
coef_df = pd.DataFrame({
    'Variable': X.columns,
    'Coefficient': model.coef_[0],
    'Odds_Ratio': np.exp(model.coef_[0]),
    'Direction': ['Risk factor (+NPL)' if c > 0 else 'Protective (-NPL)' for c in model.coef_[0]]
}).sort_values('Coefficient')

print(f'\n=== COEFFICIENTS AND ODDS RATIOS ===')
print(coef_df.to_string(index=False))

# === MARGINAL EFFECTS (approximate) ===
# For logistic regression, marginal effect at mean ~ beta * p * (1-p)
p_mean = model_df['is_npl'].mean()
coef_df['Marginal_Effect_pct'] = (coef_df['Coefficient'] * p_mean * (1 - p_mean) * 100).round(2)
print(f'\n=== APPROXIMATE MARGINAL EFFECTS (at mean, percentage points) ===')
for _, row in coef_df.iterrows():
    print(f"  {row['Variable']:<25} {row['Marginal_Effect_pct']:+.2f} p.p.")

# === CHART 24 FIXED: Coefficient plot ===
fig, ax = plt.subplots(figsize=(11, 7))
coef_sorted = coef_df.sort_values('Coefficient')
bar_colors = ['#27ae60' if c < 0 else '#e74c3c' for c in coef_sorted['Coefficient']]
bars = ax.barh(range(len(coef_sorted)), coef_sorted['Coefficient'], color=bar_colors)
ax.set_yticks(range(len(coef_sorted)))
ax.set_yticklabels(coef_sorted['Variable'], fontsize=10)
ax.axvline(x=0, color='black', linewidth=0.8)
for i, (coef, odds) in enumerate(zip(coef_sorted['Coefficient'], coef_sorted['Odds_Ratio'])):
    label = f'OR={odds:.2f}'
    ax.text(coef + (0.02 if coef >= 0 else -0.02), i, label,
            va='center', ha='left' if coef >= 0 else 'right', fontsize=9)
ax.set_xlabel('Coefficient (standardized)')
ax.set_title(f'24-rasm (FIXED). NPL risk omillari - logistik regressiya\n'
             f'N={len(model_df)}, Accuracy={model.score(X_scaled, y)*100:.1f}%, '
             f'CV={cv_scores.mean()*100:.1f}%')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_24_logistic_coef_FIXED.png'), dpi=300, bbox_inches='tight')
plt.close()
print('\nChart 24 FIXED saved')

# === CHART 31: Odds Ratio forest plot ===
fig, ax = plt.subplots(figsize=(11, 7))
coef_sorted_or = coef_df.sort_values('Odds_Ratio')
bar_colors = ['#27ae60' if or_val < 1 else '#e74c3c' for or_val in coef_sorted_or['Odds_Ratio']]
bars = ax.barh(range(len(coef_sorted_or)), coef_sorted_or['Odds_Ratio'], color=bar_colors)
ax.set_yticks(range(len(coef_sorted_or)))
ax.set_yticklabels(coef_sorted_or['Variable'], fontsize=10)
ax.axvline(x=1.0, color='black', linewidth=1.2, linestyle='--')
for i, (var, or_val) in enumerate(zip(coef_sorted_or['Variable'], coef_sorted_or['Odds_Ratio'])):
    ax.text(or_val + 0.01, i, f'{or_val:.2f}', va='center', fontsize=9)
ax.set_xlabel('Odds Ratio (OR)')
ax.set_title(f'31-rasm. NPL Odds Ratios (N={len(model_df)})\n'
             f'OR < 1 = himoya omili (yashil) | OR > 1 = xavf omili (qizil)')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_31_odds_ratios.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 31 saved: Odds Ratio forest plot')

print('\n=== BLOCK 5 FIXED COMPLETE ===')
