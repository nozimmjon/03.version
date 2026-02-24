import pandas as pd
import numpy as np
import os

base = r"D:\old\Desktop\Cerr_old\Cerr\2025\58. Марказий банк тадқиқоти\умумий репорт"
df = pd.read_excel(os.path.join(base, 'survey_master_cleaned_v1.xlsx'), engine='openpyxl')
N = len(df)
borrowers = df[df['has_loan']==1]
nonborrowers = df[df['has_loan']==0]

print('='*80)
print('BLOCK 2: CREDIT ATTITUDES & SOURCES ANALYSIS')
print('='*80)

# === 2.1 What purposes justify borrowing? (Q2.1, cols 41-50) ===
print(f'\n### 2.1 PURPOSES THAT JUSTIFY BORROWING (all respondents, N={N})')
purpose_labels = [
    'Kundalik xarajatlar (Povsednevnye)',
    'Sogliq/Talim (Zdorovye/Obrazovanie)',
    'Telefon',
    'Ipoteka (Ipoteka/remont)',
    'Avtomobil',
    'Toy/marosim (Svadba)',
    'Maishiy texnika (Bytovaya tekh.)',
    'Boshqa qarzni tolash (Pogash. dr. dolga)',
    'Biznes',
    'Boshqa (Drugoe)',
]
for i, label in enumerate(purpose_labels):
    col = df.columns[41+i]
    total = df[col].sum()
    b_pct = borrowers[col].mean()*100
    nb_pct = nonborrowers[col].mean()*100
    print(f'  {label}: N={total} ({total/N*100:.1f}%) | B: {b_pct:.1f}% | NB: {nb_pct:.1f}%')

# === 2.2 First source when extra funds needed ===
src_col = df.columns[51]
print(f'\n### 2.2 FIRST SOURCE WHEN EXTRA FUNDS NEEDED (N={N})')
src = df[src_col].value_counts()
for val, cnt in src.items():
    b_cnt = borrowers[src_col].value_counts().get(val, 0)
    nb_cnt = nonborrowers[src_col].value_counts().get(val, 0)
    print(f'  {val}: N={cnt} ({cnt/N*100:.1f}%) | B:{b_cnt} | NB:{nb_cnt}')

# === First source by REGION ===
print(f'\n### 2.2a FIRST SOURCE BY REGION')
def simplify_source(val):
    if pd.isna(val): return 'Other'
    val = str(val)
    if 'Банк' in val: return 'Bank'
    if 'Оила' in val or 'танишларга' in val: return 'Family/Friends'
    if 'олмасдан' in val: return 'Would not borrow'
    return 'Other'

df['source_simple'] = df[src_col].apply(simplify_source)
ct_reg = pd.crosstab(df['region'], df['source_simple'], normalize='index')*100
print(ct_reg.round(1).to_string())

# === 2.2a Why informal borrowing (cols 54-60) ===
print(f'\n### 2.2a WHY INFORMAL BORROWING')
informal_labels = [
    'Oson va tez (Legko i bystro)',
    'Ustamasi yoq (Bez protsentov)',
    'Rasmiy daromad shart emas (Net form. dokhoda)',
    'Diniy sabab (Relig. prichiny)',
    'Kredit tarix yomon (Plokhaya KI)',
    'Bank stavkasi yuqori (Vys. stavki bankov)',
    'Boshqa (Drugoe)',
]
informal_mask = df[df.columns[54:61]].sum(axis=1) > 0
informal_n = informal_mask.sum()
print(f'  Informal borrowers subsample: N={informal_n}')
for i, label in enumerate(informal_labels):
    col = df.columns[54+i]
    n = df.loc[informal_mask, col].sum()
    pct = n/informal_n*100 if informal_n > 0 else 0
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 2.2b Why commercial banks (cols 63-70) ===
print(f'\n### 2.2b WHY COMMERCIAL BANKS')
bank_labels = [
    'Shartlar aniq va qonuniy (Chetkiye usloviya)',
    'Kredit tarix shakllanadi (Formir. KI)',
    'Foizi kamroq (Nizkiye %)',
    'Bankda tanish bor (Znakomyy v banke)',
    'Yirik summa norasmiy qiyin (Krupnuyu neformal. slozhno)',
    'Qulay va oson/online (Udobno/onlayn)',
    'Uzoq muddat (Dlit. sroki)',
    'Boshqa (Drugoe)',
]
bank_mask = df[df.columns[63:71]].sum(axis=1) > 0
bank_n = bank_mask.sum()
print(f'  Bank borrowers subsample: N={bank_n}')
for i, label in enumerate(bank_labels):
    col = df.columns[63+i]
    n = df.loc[bank_mask, col].sum()
    pct = n/bank_n*100 if bank_n > 0 else 0
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 2.2g Why prefer NOT to borrow (cols 80-89) ===
print(f'\n### 2.2g WHY PREFER NOT TO BORROW')
noborrow_labels = [
    'Ishonmayman (Ne doveryayu fin.org.)',
    'Kredit tuzogidan qorqaman (Boyus kred. lovushki)',
    'Daromad barqaror emas (Nestab. dokhod)',
    'Jamgarmalarim yetarli (Nakopleniya dostat.)',
    'Hujjatlar qiyin (Slozhnye dokumenty)',
    'Atrofdagilar yomon tajriba (Plokhoy opyt)',
    'Shartlarni tushunmayman (Ne ponimayu usloviya)',
    'Etiqod (Relig. ubezhdeniya)',
    'Oila/yaqinlar qarshi (Semya protiv)',
    'Boshqa (Drugoe)',
]
noborrow_mask = df[df.columns[80:90]].sum(axis=1) > 0
noborrow_n = noborrow_mask.sum()
print(f'  Non-borrowing preference subsample: N={noborrow_n}')
for i, label in enumerate(noborrow_labels):
    col = df.columns[80+i]
    n = df.loc[noborrow_mask, col].sum()
    pct = n/noborrow_n*100 if noborrow_n > 0 else 0
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 2.5 Credit source for borrowers (cols 109-115) ===
print(f'\n### 2.5 CREDIT SOURCES FOR BORROWERS (N={len(borrowers)})')
source_labels = [
    'Tijoriy bankdan (Kommerch. bank)',
    'Rasmiy nasiya (Formalnaya rassrochka)',
    'Mikromoliya (MFO)',
    'Lombarddan (Lombard)',
    'Norasmiy (Neform. kreditory)',
    'Oila/dostlar (Semya/druzya)',
    'Boshqa (Drugoe)',
]
for i, label in enumerate(source_labels):
    col = df.columns[109+i]
    n = borrowers[col].sum()
    pct = n/len(borrowers)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === 2.6 Reason for borrowing ===
reason_col = df.columns[117]
print(f'\n### 2.6 REASON FOR BORROWING')
print(borrowers[reason_col].value_counts().to_string())

# === 2.7 Credit purpose (cols 119-132) ===
print(f'\n### 2.7 CREDIT PURPOSE (borrowers, multiple response, N={len(borrowers)})')
purpose_borrow_labels = [
    'Kundalik xarajat (Povsednevnye)',
    'Sogliq (Zdorovye)',
    'Maishiy texnika (Byt. tekhnika)',
    'Avtomobil',
    'Talim (Obrazovanie)',
    'Toy/marosim (Svadba)',
    'Uy-joy/ipoteka (Ipoteka)',
    'Uy tamirlash (Remont)',
    'Boshqa qarz tolash (Pogash. dr. dolga)',
    'Chetga ketish (Vyezd za rubezh)',
    'Biznes',
    'Oilaviy tadbirkorlik (Sem. predprinim.)',
    'Yaqinlarga yordam (Pomoshch rodstv.)',
    'Boshqa (Drugoe)',
]
for i, label in enumerate(purpose_borrow_labels):
    col = df.columns[119+i]
    n = borrowers[col].sum()
    pct = n/len(borrowers)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === CROSS-TAB: First source x Income ===
print(f'\n### CROSS-TAB: First source x Income')
inc_col = df.columns[40]
ct_inc = pd.crosstab(df[inc_col], df['source_simple'], normalize='index')*100
print(ct_inc.round(1).to_string())

# === CROSS-TAB: First source x Gender ===
print(f'\n### CROSS-TAB: First source x Gender')
gender_col = df.columns[20]
ct_gen = pd.crosstab(df[gender_col], df['source_simple'], normalize='index')*100
print(ct_gen.round(1).to_string())

# === 2.8 Targeted use of credit ===
target_col = df.columns[159]
print(f'\n### 2.8 WAS CREDIT USED FOR INTENDED PURPOSE? (N={borrowers[target_col].notna().sum()})')
print(borrowers[target_col].value_counts().to_string())

# === 2.9 Credit form (online vs offline) ===
form_col = df.columns[161]
print(f'\n### 2.9 CREDIT FORM (online vs offline, N={borrowers[form_col].notna().sum()})')
print(borrowers[form_col].value_counts().to_string())

# === 2.10 Decision factors (cols 163-170) ===
print(f'\n### 2.10 WHAT INFLUENCED BORROWING DECISION (N={len(borrowers)})')
decision_labels = [
    'Zaruriyat (Neobkhodimost)',
    'Imtiyoz (Lgoty)',
    'Tanishlar tavsiyasi (Rek. znakomykh)',
    'Hokimiyat tavsiyasi (Rek. gos. organov)',
    'Aldanib qolgan (Byl obmanut)',
    'Kredit tarix yaxshilanadi (Uluchsheniye KI)',
    'Inflyatsiya',
    'Boshqa (Drugoe)',
]
for i, label in enumerate(decision_labels):
    col = df.columns[163+i]
    n = borrowers[col].sum()
    pct = n/len(borrowers)*100
    print(f'  {label}: N={n} ({pct:.1f}%)')

# === Credit purpose by repayment status (NPL vs on-time) ===
print(f'\n### CREDIT PURPOSE BY REPAYMENT STATUS')
npl = df[df['holat1'].str.contains('NPL', na=False)]
ontime = df[df['holat1'].str.contains('вақтида', na=False)]
delayed = df[df['holat1'].str.contains('1-3', na=False)]
print(f'NPL: N={len(npl)}, On-time: N={len(ontime)}, 1-3 month: N={len(delayed)}')
for i, label in enumerate(purpose_borrow_labels):
    col = df.columns[119+i]
    npl_pct = npl[col].mean()*100
    ontime_pct = ontime[col].mean()*100
    delayed_pct = delayed[col].mean()*100
    print(f'  {label}: NPL={npl_pct:.1f}% | 1-3m={delayed_pct:.1f}% | On-time={ontime_pct:.1f}%')

print('\n=== BLOCK 2 ANALYSIS COMPLETE ===')
