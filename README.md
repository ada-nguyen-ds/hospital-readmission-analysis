# Hospital Readmission Analysis

## Project question

How do hospital characteristics and county socioeconomic conditions relate to CMS excess readmission ratios?

## Portfolio highlights

- **7,944** hospital-condition observations
- **2,444** hospitals across **1,197** counties
- **55%** of analyzed observations had an excess readmission ratio at or above 1
- Approximately **23%** of residual variation occurred between hospitals

## Why this project matters

Hospital readmission performance may reflect both hospital characteristics and the communities hospitals serve. This project combines public hospital performance data with county socioeconomic indicators and accounts for repeated condition-level observations within each hospital.

## Data sources

- CMS Hospital Readmissions Reduction Program data
- CMS hospital characteristics
- ZIP and county FIPS crosswalk data
- Area Health Resources Files

Raw source extracts are excluded from the repository to keep it lightweight and avoid redistributing large source files. The README and analysis workflow identify the data needed to reproduce the project.

## Data preparation

The workflow:

1. Cleans hospital identifiers, measure names, ZIP codes, and county FIPS values.
2. Filters invalid or incomplete readmission records.
3. Joins hospital information to county socioeconomic indicators.
4. Corrects a many-to-many ZIP-to-county join that created duplicated hospital-measure rows.
5. Standardizes socioeconomic predictors for comparable model interpretation.
6. Exports analysis-ready and Tableau-ready results.

## Statistical method

The project uses a **linear mixed-effects model** fitted in R with `nlme`.

- **Outcome:** Excess readmission ratio
- **Fixed effects:** Readmission measure, ownership, emergency services, poverty, unemployment, uninsured rate, income, age 65+, and log population
- **Random effect:** Hospital-level intercept
- **Model checks:** Likelihood-ratio tests, confidence intervals, AIC, BIC, and intraclass correlation

The random intercept accounts for multiple condition-level observations recorded for the same hospital.

## Key findings

After adjustment, higher county poverty, unemployment, and population size remained associated with higher excess readmission ratios.

- Poverty: approximately **+0.0082** ERR per 1 SD
- Unemployment: approximately **+0.0069** ERR per 1 SD
- Log population: approximately **+0.0060** ERR per 1 SD
- Hospital-level intraclass correlation: approximately **23%**

These results describe adjusted associations and do not establish causation.

## Reports

- [Live HTML report](https://ada-nguyen-ds.github.io/hospital-readmission-analysis/)
- [Download the PDF report](https://github.com/ada-nguyen-ds/hospital-readmission-analysis/blob/main/readmissions%20report.pdf)

## Tools

R, dplyr, readr, stringr, ggplot2, nlme, Excel, and Tableau-ready exports

## Next improvements

- Add a concise data dictionary and source-download instructions
- Organize analysis scripts and outputs into a clear project structure
- Add a dashboard screenshot and direct Tableau link
- Add reproducibility instructions and package versions
