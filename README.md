# Project Title : Longitudinal Data Analysis for AMPATH Multiple Myeloma Patients Data

# Project Description
This project centers on analyzing longitudinal data from patients diagnosed with Multiple Myeloma (MM) who receive care, treatment, and follow-up at Moi Teaching and Referral Hospital (MTRH) under the AMPATH Multiple Myeloma Program, which has been operational since 2009. The AMPATH (Academic Model Providing Access to Healthcare) MM program's extensive dataset provides a unique opportunity to track MM progression, treatment patterns, and survival outcomes over a prolonged period, offering valuable insights for clinical and research purposes.

Multiple Myeloma is a chronic and progressive hematological cancer with complex treatment needs. Patients in the AMPATH program have undergone numerous clinical encounters, each documenting critical information on demographics, diagnostics, treatment protocols, and symptom evolution. 

This analysis aims to: -
1.  extract actionable insights on survival rates,
2.  highlight key clinical indicators, and
3.  Visualize trends to support treatment planning and informed clinical decision-making.

The project includes:-
1.  comprehensive data cleaning,
2.  feature engineering,
3.  and survival analysis, with a particular focus on CRAB criteria-based indicators (elevated calcium, renal dysfunction, anemia, and bone involvement).
4.  Additionally, a dynamic dashboard will present key findings, facilitating an intuitive review of MM trends, symptom progression, and survival metrics. 

By examining these longitudinal data patterns, this project aims to support clinicians, researchers, and stakeholders in optimizing MM care strategies based on real-world patient outcomes.

# Data/Statistical  Analysis Objectives
1. Data Cleaning and Transformation: Clean and format raw longitudinal patient data, addressing inconsistencies, missing values, and duplicate records.
2. escriptive Analysis: Describe patient demographics, diagnostic dates, and treatment encounters to characterize the patient cohort.
3. Feature Engineering: Create meaningful variables to capture symptoms, treatment details, and disease progression.
4. CRAB Criteria Assessment: Develop specific variables for CRAB (Calcium, Renal, Anemia, Bone) diagnostic features.
5. Survival Analysis: Examine survival rates based on clinical and diagnostic indicators.
6. Dashboard Creation: Build a dynamic dashboard to visualize patient statistics, key symptoms, and treatment patterns over time.

# Analysis Plan
1. Data Import: Import raw Multiple Myeloma (MM) data in CSV format.
2. Data Cleaning: Correct date formats, handle missing values, and ensure uniformity in categorical fields.
3. Exploratory Data Analysis (EDA): Conduct initial analysis to understand patient demographics, diagnostic distribution, and encounter patterns.
4. Feature Engineering: Create new variables, such as survival status, symptom presence, and CRAB features.
5. Descriptive Statistics: Summarize the cohort's clinical indicators, including symptoms, age distribution, CRAB feature prevalence, and survival rates.

# Key Variables
a) Patient Demographics: Date of birth, age, gender, and hospital ID.
b) Encounter Data: Encounter dates, diagnosis date, encounter type (initial/return).
c) CRAB Diagnostic Features: Indicators for calcium level, renal function (creatinine), anemia (hemoglobin), and bone involvement.
d) Survival Status: Alive, lost to follow-up (LTFU), or deceased.
e) Chief Complaints and Symptoms: Various symptoms categorized by pain site, bleeding site, and swelling site.

# Expected Outcomes
- Survival rates
- gender ratio
- Remission %

# Exploratory Data Analysis (EDA) Procedures
1. Date Formatting and Validation: Convert various date fields to date format and check for inconsistencies.
2. Missing Data Handling: Replace missing values in dates and categorical fields, including diagnosis and encounter dates.
3. Duplicate Checking: Identify and address duplicate encounters within patients.
4. Symptom and Pain Site Categorization: Recode symptoms and complaints into standardized categories (e.g., bleeding site, pain location).

# Statistical Analyses
1. Descriptive Statistics: Summarize patient ages, encounter counts, and CRAB feature prevalence.
2. Frequency Analysis: For categorical variables like chief complaints, pain sites, and symptoms.
3. Survival Analysis: Conduct survival analysis based on key clinical indicators and demographics.
