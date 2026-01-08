# Hospital Readmission Analysis

## Overview
This project analyzes hospital Excess Readmission Ratios (ERR) using
linear mixed-effects models to assess the impact of hospital
characteristics and county-level socioeconomic factors.

## Data
- CMS Hospital Readmission Measures
- Area Health Resources Files (AHRF)

> Raw data are not publicly shared due to usage restrictions.

## Methods
- Data cleaning and multi-source joins (ZIP → county FIPS)
- Linear mixed-effects models with hospital-level random intercepts
- Effect plots using standardized predictors (ggpredict)

## Key Findings
- Higher county-level poverty and unemployment rates are associated
  with higher ERR after adjustment.
- Physician-owned hospitals show lower adjusted ERR.

## Outputs
- 📄 **HTML Report (live on GitHub Pages)**  
  👉 [View HTML report](https://ada-nguyen-ds.github.io/hospital-readmission-analysis/)

- 📄 **HTML Report (file in repo)**  
  👉 [Open HTML file](Hospital%20Readmission%20Report.html)

- 📄 **PDF Report**  
  👉 [Download PDF report](Hospital%20Readmission%20Report.pdf)

## Tools
R, tidyverse, lme4, emmeans, ggeffects
