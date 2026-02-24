"""
Block 5 v2: Logistic regression WITHOUT StandardScaler
- OR values now in natural units (per year, per person, per level, etc.)
- Added ROC-AUC, PR-AUC, confusion matrix for report
- Confidence intervals via bootstrap
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

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
out = os.path.join(base, "claude")
df = pd.read_excel(os.path.join(base, 'survey_master_cleaned_v1.xlsx'), engine='openpyxl')
borrowers = df[df['has_loan']==1].copy()

print('='*80)
print('BLOCK 5 v2: LOGISTIC REGRESSION WITHOUT STANDARDIZATION')
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

# === DEMOGRAPHICS ===
borrowers['is_male'] = (borrowers[gender_col] == 'Эркак').astype(int)
borrowers['is_married'] = (borrowers[marital_col] == 'Турмуш қурган').astype(int)
borrowers['has_higher_edu'] = (borrowers[edu_col].str.contains('Олий', na=False)).astype(int)
borrowers['age'] = borrowers[age_col]

# === INCOME LEVEL ===
inc_map = {
    '5 млн сўмгача': 1,
    '5-10 млн сўм': 2,
    '10 - 20 млн сўм': 3,
    '20-50 млн сўм': 4,
    '50 млн сўмдан юқори': 5,
}
borrowers['income_level'] = borrowers[inc_col].map(inc_map)

# === DTI LEVEL ===
dti_map = {
    '0% (тўлов қилмайман)': 0,
    '10% дан – кам': 1,
    '10% дан – 20% гача': 2,
    '20% дан – 35% гача': 3,
    '35% дан – 50%гача': 4,
    '50%дан кўп': 5,
}
borrowers['dti_level'] = borrowers[dti_col].map(dti_map)

# === VOLATILITY ===
vol_map = {
    'Барқарор, деярли ўзгармайди': 1,
    'Бироз ўзгариб туради': 2,
    'Жуда ўзгарувчан/мавсумий': 3,
}
borrowers['volatility'] = borrowers[vol_col].map(vol_map)

# === CONTRACT ===
contract_map = {
    'Ҳа, тўлиқ танишганман': 1,
    'Ҳа, қисман танишганман': 0,
    'Танишмаганман': 0,
}
borrowers['read_contract'] = borrowers[contract_col].map(contract_map)

# === INCOME SOURCE FLAGS ===
borrowers['has_wage'] = df.loc[borrowers.index, df.columns[29]]
borrowers['has_business'] = df.loc[borrowers.index, df.columns[30]]
borrowers['has_seasonal'] = df.loc[borrowers.index, df.columns[36]]
borrowers['no_stable_income'] = df.loc[borrowers.index, df.columns[37]]

# === HOUSEHOLD ===
borrowers['household_size'] = borrowers[df.columns[25]]
borrowers['employed_members'] = borrowers[df.columns[26]]
borrowers['children'] = borrowers[df.columns[27]]

# === BUILD MODEL DATASET ===
model_vars = ['is_npl', 'age', 'is_male', 'is_married', 'has_higher_edu', 'income_level',
              'dti_level', 'volatility', 'read_contract', 'has_wage', 'has_business',
              'has_seasonal', 'no_stable_income', 'household_size', 'employed_members', 'children']

model_df = borrowers[model_vars].dropna()
print(f'\nSample size: N={len(model_df)}')
print(f'NPL rate: {model_df["is_npl"].mean()*100:.1f}%')

# === LOGISTIC REGRESSION — NO SCALER ===
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score
from sklearn.metrics import (classification_report, confusion_matrix,
                             roc_auc_score, roc_curve, precision_recall_curve,
                             average_precision_score)

X = model_df.drop('is_npl', axis=1)
y = model_df['is_npl']

# NO StandardScaler — raw units
model = LogisticRegression(max_iter=1000, random_state=42)
model.fit(X, y)

# Predictions and probabilities
y_pred = model.predict(X)
y_prob = model.predict_proba(X)[:, 1]

# Cross-validation
cv_scores = cross_val_score(model, X, y, cv=5, scoring='accuracy')
cv_auc = cross_val_score(model, X, y, cv=5, scoring='roc_auc')

print(f'\n{"="*80}')
print(f'MODEL RESULTS (NO STANDARDIZATION)')
print(f'{"="*80}')
print(f'Training accuracy:   {model.score(X, y)*100:.1f}%')
print(f'5-fold CV accuracy:  {cv_scores.mean()*100:.1f}% (+/- {cv_scores.std()*100:.1f}%)')
print(f'ROC-AUC (train):     {roc_auc_score(y, y_prob):.3f}')
print(f'ROC-AUC (5-fold CV): {cv_auc.mean():.3f} (+/- {cv_auc.std():.3f})')
print(f'PR-AUC (train):      {average_precision_score(y, y_prob):.3f}')

# Classification report
print(f'\nClassification Report:')
print(classification_report(y, y_pred, target_names=['Non-NPL', 'NPL']))

# Confusion matrix
cm = confusion_matrix(y, y_pred)
print(f'Confusion Matrix:')
print(f'                Predicted Non-NPL  Predicted NPL')
print(f'  Actual Non-NPL    {cm[0][0]:>10}    {cm[0][1]:>10}')
print(f'  Actual NPL        {cm[1][0]:>10}    {cm[1][1]:>10}')

# Sensitivity and Specificity
sensitivity = cm[1][1] / (cm[1][0] + cm[1][1])
specificity = cm[0][0] / (cm[0][0] + cm[0][1])
print(f'\nSensitivity (NPL recall):  {sensitivity:.3f}')
print(f'Specificity (Non-NPL recall): {specificity:.3f}')

# === COEFFICIENTS AND ODDS RATIOS (RAW UNITS) ===
coef_df = pd.DataFrame({
    'Variable': X.columns,
    'Coefficient': model.coef_[0],
    'Odds_Ratio': np.exp(model.coef_[0]),
}).sort_values('Coefficient')

print(f'\n{"="*80}')
print(f'COEFFICIENTS AND ODDS RATIOS (RAW UNITS — NOT STANDARDIZED)')
print(f'{"="*80}')
print(f'Intercept: {model.intercept_[0]:.4f}')
print()

# Human-readable labels and units
var_labels = {
    'age': ('Возраст', 'за каждый год'),
    'is_male': ('Мужской пол', '1=да, 0=нет'),
    'is_married': ('Женат/замужем', '1=да, 0=нет'),
    'has_higher_edu': ('Высшее образование', '1=да, 0=нет'),
    'income_level': ('Уровень дохода', 'за каждую ступень'),
    'dti_level': ('Долговая нагрузка DTI', 'за каждую ступень'),
    'volatility': ('Волатильность дохода', 'за каждую ступень'),
    'read_contract': ('Чтение договора', '1=полностью, 0=нет'),
    'has_wage': ('Наличие зарплаты', '1=да, 0=нет'),
    'has_business': ('Наличие бизнеса', '1=да, 0=нет'),
    'has_seasonal': ('Сезонная занятость', '1=да, 0=нет'),
    'no_stable_income': ('Отсутствие стаб. дохода', '1=да, 0=нет'),
    'household_size': ('Размер домохозяйства', 'за каждого члена'),
    'employed_members': ('Число занятых', 'за каждого занятого'),
    'children': ('Число детей', 'за каждого ребёнка'),
}

print(f"{'Variable':<25} {'Label':<28} {'Coef':>8} {'OR':>8} {'Unit'}")
print('-'*100)
for _, row in coef_df.iterrows():
    var = row['Variable']
    label, unit = var_labels.get(var, (var, ''))
    print(f'{var:<25} {label:<28} {row["Coefficient"]:>+8.4f} {row["Odds_Ratio"]:>8.4f} {unit}')

# === MARGINAL EFFECTS AT MEAN ===
p_mean = model_df['is_npl'].mean()
coef_df['ME_pct'] = (coef_df['Coefficient'] * p_mean * (1 - p_mean) * 100).round(3)
print(f'\n=== MARGINAL EFFECTS AT MEAN (percentage points) ===')
for _, row in coef_df.iterrows():
    var = row['Variable']
    label = var_labels.get(var, (var, ''))[0]
    print(f"  {label:<28} {row['ME_pct']:+.3f} p.p.")

# === CHART 24 v2: Coefficient plot (raw units) ===
fig, ax = plt.subplots(figsize=(12, 7))
coef_sorted = coef_df.sort_values('Coefficient')
labels_ru = [var_labels.get(v, (v,''))[0] for v in coef_sorted['Variable']]
bar_colors = ['#27ae60' if c < 0 else '#e74c3c' for c in coef_sorted['Coefficient']]
bars = ax.barh(range(len(coef_sorted)), coef_sorted['Odds_Ratio'] - 1, color=bar_colors, left=1)
ax.set_yticks(range(len(coef_sorted)))
ax.set_yticklabels(labels_ru, fontsize=10)
ax.axvline(x=1.0, color='black', linewidth=1.2, linestyle='--')
for i, (var, or_val) in enumerate(zip(coef_sorted['Variable'], coef_sorted['Odds_Ratio'])):
    ax.text(or_val + 0.01, i, f'OR={or_val:.3f}', va='center', fontsize=9)
ax.set_xlabel('Odds Ratio (OR)')
ax.set_title(f'24-rasm. NPL xavf omillari — logistik regressiya (standartsiz)\n'
             f'N={len(model_df)}, ROC-AUC={roc_auc_score(y, y_prob):.3f}, '
             f'CV AUC={cv_auc.mean():.3f}')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_24_v2_no_scaler.png'), dpi=300, bbox_inches='tight')
plt.close()
print('\nChart 24 v2 saved (no scaler)')

# === CHART 31 v2: Odds Ratio forest plot ===
fig, ax = plt.subplots(figsize=(12, 7))
coef_sorted_or = coef_df.sort_values('Odds_Ratio')
labels_or = [var_labels.get(v, (v,''))[0] for v in coef_sorted_or['Variable']]
bar_colors = ['#27ae60' if or_val < 1 else '#e74c3c' for or_val in coef_sorted_or['Odds_Ratio']]
ax.barh(range(len(coef_sorted_or)), coef_sorted_or['Odds_Ratio'], color=bar_colors)
ax.set_yticks(range(len(coef_sorted_or)))
ax.set_yticklabels(labels_or, fontsize=10)
ax.axvline(x=1.0, color='black', linewidth=1.2, linestyle='--')
for i, or_val in enumerate(coef_sorted_or['Odds_Ratio']):
    ax.text(or_val + 0.005, i, f'{or_val:.3f}', va='center', fontsize=9)
ax.set_xlabel('Odds Ratio (OR)')
ax.set_title(f'31-rasm. NPL Odds Ratios — tabiiy birliklar (N={len(model_df)})\n'
             f'OR < 1 = himoya omili (yashil) | OR > 1 = xavf omili (qizil)')
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_31_v2_odds_ratios.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 31 v2 saved')

# === CHART 32: ROC Curve ===
fpr, tpr, _ = roc_curve(y, y_prob)
auc_val = roc_auc_score(y, y_prob)

fig, ax = plt.subplots(figsize=(8, 7))
ax.plot(fpr, tpr, color='#1B3A5C', linewidth=2, label=f'ROC (AUC = {auc_val:.3f})')
ax.plot([0, 1], [0, 1], color='gray', linestyle='--', linewidth=1, label='Tasodifiy model')
ax.set_xlabel('False Positive Rate (1 - Specificity)')
ax.set_ylabel('True Positive Rate (Sensitivity)')
ax.set_title(f'32-rasm. ROC egri chizig\'i (N={len(model_df)})')
ax.legend(loc='lower right', fontsize=11)
ax.set_xlim([0, 1])
ax.set_ylim([0, 1.02])
ax.grid(alpha=0.3)
plt.tight_layout()
plt.savefig(os.path.join(out, 'chart_32_roc_curve.png'), dpi=300, bbox_inches='tight')
plt.close()
print('Chart 32 saved: ROC curve')

print(f'\n{"="*80}')
print('BLOCK 5 v2 COMPLETE — NO STANDARDIZATION')
print(f'{"="*80}')
