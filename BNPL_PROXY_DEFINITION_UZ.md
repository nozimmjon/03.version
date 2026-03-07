---
editor: 
  markdown: 
    wrap: 72
---

# BNPL proxy definition (Uzbek)

## Maqsad

Ushbu loyiha doirasida `BNPL / nasiya` segmentini so'rovnoma
ma'lumotlarida to'g'ridan-to'g'ri emas, balki mavjud savollar orqali
**proksi** sifatida belgilash kerak. Eng muhim tamoyil: **formal** va
**informal** nasiyani aralashtirmaslik, lekin zarur joyda ular uchun
**birlashtirilgan keng proksi** ham berish.

## Asosiy xulosa

Ma'lumotlar tarkibiga ko'ra quyidagi uch darajali yondashuv tavsiya
etiladi:

1.  `Formal BNPL proxy`
2.  `Informal installment proxy`
3.  `Combined installment / BNPL proxy`

Bunda har bir proksi uchun ishlatiladigan maydon va cheklov alohida
ko'rsatiladi.

------------------------------------------------------------------------

## 1. Formal BNPL proxy

### 1A. Formal BNPL - primary preference proxy

**O'zgaruvchi:** `credit_source_primary`

**Formal BNPL qiymati:** -
`Расмий насия хизматларига (“Техномарт”, “Узум”, “Ишонч” в.б.)`

**Talqin:** - Respondent zarurat tug'ilganda birinchi navbatda formal
nasiya xizmatini tanlashini bildiradi. - Bu **foydalanish emas**, balki
**birlamchi afzallik** yoki tanlov niyatidir.

**Qayerda ishlatish mumkin:** - bozorning potentsial talabi, - formal
nasiya jozibadorligi, - bank / oila / informal manbalar bilan
taqqoslash.

**Qayerda ishlatmaslik kerak:** - actual user profile, - NPL
comparison, - repayment burden analysis.

### 1B. Formal BNPL - actual use proxy

**O'zgaruvchi:** `credit_source_actual__formal_installment`

**Formal BNPL user = 1 agar:** - respondent actual foydalanilgan
manbalar ichida formal nasiyani belgilagan bo'lsa.

**Talqin:** - Bu formal nasiya xizmatidan amalda foydalangan
qarzdorlarni ifodalaydi. - BNPL bo'yicha eng toza **actual-use** proksi
shu hisoblanadi.

**Qayerda ishlatish mumkin:** - NPL bilan taqqoslash, - multi-debt
layering, - regret, - repayment behaviour, - desired ticket-size bilan
bog'lash.

**Cheklov:** - formal nasiya foydalanishi bank, oila yoki boshqa
manbalar bilan birga bo'lishi mumkin; bu eksklyuziv foydalanish emas.

------------------------------------------------------------------------

## 2. Informal installment proxy

### 2A. Informal installment - primary preference proxy

**O'zgaruvchi:** `credit_source_primary`

**Informal installment qiymati:** -
`Норасмий насия хизматига (бозорлар, дўконлар в.б.)`

**Talqin:** - respondent informal nasiya xizmatini birinchi tanlov
sifatida ko'rsatgan. - Bu ham **actual use emas**, balki **birlamchi
tanlov / preference** proksisi.

**Qayerda ishlatish mumkin:** - formal vs informal nasiya tanlovi, -
source architecture, - Q2.3 desired amount comparison.

**Qayerda ishlatmaslik kerak:** - actual user NPL profile, - actual
multi-obligation profile, - actual repayment distress profile.

### 2B. Informal installment - repayment-priority proxy

**O'zgaruvchi:** `repayment_priority__informal_installment`

**Talqin:** - respondent qarzlarni qaytarishda informal nasiyani ustuvor
to'lov sifatida ko'rishini bildiradi. - Bu **foydalanish** emas, balki
**repayment priority / salience** proksisi.

**Qayerda ishlatish mumkin:** - instalment obligations qaysi darajada
prioritet ekanini ko'rsatish, - behavioural salience discussion.

**Qayerda ishlatmaslik kerak:** - informal nasiyadan foydalanuvchilar
ulushi deb, - informal BNPL NPL rate deb.

### Muhim cheklov

Datasetda `credit_source_actual__informal_installment` degan alohida
actual-use dummysi yo'q. Shuning uchun informal nasiya uchun formal
nasiya darajasidagi toza `actual-use` proksi mavjud emas.

------------------------------------------------------------------------

## 3. Combined installment / BNPL proxy

### 3A. Combined preference proxy

**Tarkibi:** `credit_source_primary` ichida quyidagi ikki qiymat
birlashtiriladi: -
`Расмий насия хизматларига (“Техномарт”, “Узум”, “Ишонч” в.б.)` -
`Норасмий насия хизматига (бозорлар, дўконлар в.б.)`

**Talqin:** - deferred-payment / instalment ecosystem'ga bo'lgan umumiy
qiziqish yoki birinchi tanlov.

**Qayerda ishlatish mumkin:** - broad BNPL preference footprint, -
source comparison, - average desired amount analysis (Q2.3).

### 3B. Combined behavioural salience proxy

**Tarkibi:** - `repayment_priority__formal_installment` -
`repayment_priority__informal_installment`

**Talqin:** - instalment majburiyatlarining to'lov prioritetidagi o'rni.

**Qayerda ishlatish mumkin:** - repayment hierarchy, - behavioural
salience.

### 3C. Combined actual-use proxy

**Tavsiya:** ehtiyotkorlik bilan ishlatilsin.

**Sabab:** - formal nasiya uchun actual-use mavjud, - informal nasiya
uchun actual-use mavjud emas, - shu sababli formal + informal combined
actual user ko'rsatkichi datasetda to'g'ridan-to'g'ri qurilmaydi.

**Xulosa:** - combined actual-use proxy qurmaslik yoki faqat alohida
izoh bilan qurish kerak. - asosiy matnda bundan qochish tavsiya etiladi.

------------------------------------------------------------------------

## 4. Tavsiya etiladigan amaliy qo'llanish

### Report main text uchun

1.  `Formal BNPL actual use proxy`
    -   `credit_source_actual__formal_installment`
2.  `Combined BNPL preference proxy`
    -   `credit_source_primary` dagi formal + informal nasiya
3.  `Combined instalment priority proxy`
    -   `repayment_priority__formal_installment` +
        `repayment_priority__informal_installment`

### Appendix / methodological note uchun

-   informal nasiya uchun actual-use proxysi yo'qligi albatta yozilsin.
-   formal va informal nasiyani bitta actual-user segment sifatida
    talqin qilmaslik kerak.

------------------------------------------------------------------------

## 5. Hozirgi ma'lumotlar asosida kuchliroq qo'shimcha natijalar

Quyidagilarni aynan shu proksi mantiqi bilan ishlatish mumkin:

### Formal BNPL actual users

**O'zgaruvchi:** `credit_source_actual__formal_installment`

Mavjud natijalar: - qarzdorlar orasida actual formal installment use:
`5.7%` (`n=87`) - formal installment users orasida multi-debt: `86.2%` -
non-formal-installment borrowers orasida multi-debt: `28.2%` - formal
installment users orasida regret: `44.8%` - boshqa borrowers orasida
regret: `35.8%` - formal installment users orasida NPL: `57.5%` - boshqa
borrowers orasida NPL: `51.6%`

**Talqin:** - eng kuchli signal: debt layering / hidden obligation
concentration. - NPL va regret farqlari descriptivedir; causal emas.

### Combined BNPL preference

**O'zgaruvchi:** `credit_source_primary`

Mavjud categories: - formal installment first choice - informal
installment first choice

**Talqin:** - actual use emas, lekin instalment channel'ga bo'lgan
orientation'ni ko'rsatadi.

### Q2.3 desired amount by source

**Muhim qoida:** nol qiymatlar chiqarib tashlangan holda hisoblash
tavsiya etiladi.

Sabab: - respondent ma'lum manbadan umuman qarz olishni ko'rib chiqmasa,
`0` qiymat average'ni sun'iy pasaytiradi. - source-specific desired
ticket size uchun `positive / non-zero responses` mantiqi to'g'riroq.

------------------------------------------------------------------------

## 6. Report-safe wording

Tavsiya etilgan metod izohi:

`Mazkur bo'limda BNPL segmenti so'rovnoma ma'lumotlarida proksi ko'rsatkichlar orqali baholandi. Formal nasiya actual-use proksisi sifatida credit_source_actual__formal_installment, informal nasiya esa primary preference va repayment-priority ko'rsatkichlari orqali baholandi. Datasetda informal nasiya uchun formal nasiyadagi kabi alohida actual-use dummysi mavjud emasligi sababli, combined BNPL actual-user ko'rsatkichi qurilmadi.`

------------------------------------------------------------------------

## 7. Qisqa qaror

**Ishlatish kerak:** - formal BNPL actual use, - formal + informal
preference, - formal + informal repayment priority.

**Ishlatmaslik kerak:** - formal + informal combined actual-use share, -
informal BNPL NPL rate (to'g'ridan-to'g'ri), - BNPL bozor ulushi
sifatida sample ulushini milliy ko'rsatkichga aylantirish.
