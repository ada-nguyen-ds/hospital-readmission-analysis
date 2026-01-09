# Hospital Readmission Analysis

## Overview
This project analyzes hospital Excess Readmission Ratios (ERR) using linear mixed-effects models to evaluate how hospital characteristics and county-level socioeconomic factors influence readmission performance.

The analysis focuses on identifying structural and socioeconomic drivers of readmissions rather than hospital-level volume effects alone.

## Data
- CMS Hospital Readmission Measures
- Area Health Resources Files (AHRF)

> Raw data are not publicly shared due to usage and privacy restrictions.  
> All analysis code is fully reproducible given access to the original data sources.

## Methods
- Data cleaning and multi-source joins (ZIP → county FIPS)
- Feature engineering and standardization of socioeconomic variables
- Linear mixed-effects models with hospital-level random intercepts
- Marginal effect visualization using standardized predictors (`ggpredict`)

## Key Findings
- Higher county-level poverty and unemployment rates are associated with **higher adjusted ERR**, even after controlling for hospital characteristics.
- **Physician-owned hospitals** exhibit lower adjusted ERR compared to other ownership types.
- Socioeconomic context explains a meaningful portion of between-hospital variation in readmission performance.


## Outputs
- 🌐 **Live HTML Report (recommended)**  
  👉 https://ada-nguyen-ds.github.io/hospital-readmission-analysis/

- 📄 **HTML Report (repository file)**  
  👉 [Open HTML Report](https://github.com/ada-nguyen-ds/hospital-readmission-analysis/blob/main/readmissions%20report.html)

- 📄 **PDF Report**  
  👉 [[Download PDF](https://github.com/ada-nguyen-ds/hospital-readmission-analysis/blob/main/readmissions%20report.pdf)


## Tools
R, tidyverse, lme4, emmeans, ggeffects
