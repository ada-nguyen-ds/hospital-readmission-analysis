# U.S. Hospital Readmissions

[Open the live R Shiny dashboard](https://01a1e25e-f806-5dfe-ea5b-43da37bf67d3.share.connect.posit.cloud/)

An interactive companion to the [hospital readmission analysis](../README.md), combining CMS hospital performance measures with county socioeconomic context from HRSA AHRF.

## Explore the app

- **Overview:** cohort counts, three dynamic highlights, a state map, condition/procedure comparisons, ERR distribution, and original adjusted findings.
- **Geography:** a State × Condition/Procedure heatmap with sample sizes on hover.
- **Hospitals:** ownership-group distributions and a selected hospital's condition-level profile.
- **Community:** county income, poverty, unemployment, uninsured population under 65, and population age 65+ compared with hospital ERR. Charts display Pearson correlation and an unadjusted linear trend when the data support calculation.
- **Data & methods:** searchable records, downloadable filtered CSV, data sources, and interpretation notes.

Choose a condition/procedure before comparing geography or ownership. Click a state to filter. The map stays nationwide and responds to the condition and ownership filters; the other charts also respond to the selected state. Reset all returns to the default view. The app works best on a laptop or desktop, with responsive stacking on smaller screens.

## Run locally

Clone or download the repository. In RStudio, open `shiny-app/hospital-readmission-shiny.Rproj`, then run:

```r
install.packages(c(
  "shiny", "bslib", "readr", "dplyr", "ggplot2", "plotly", "scales"
))
shiny::runApp(".")
```

Alternatively, from the repository root:

```r
shiny::runApp("shiny-app")
```

The bundled `.csv.gz` is read directly by `readr`; no manual extraction, account, API key, or rerun of the analysis pipeline is needed.

## Files

```text
shiny-app/
  app.R                              # UI, reactive filters, statistics, and charts
  hospital-readmission-shiny.Rproj    # RStudio project
  data/
    README.md                        # Snapshot definition and limitations
    tableau_model_complete.csv.gz    # Compressed public-data analytical snapshot
  www/
    style.css                        # Dashboard styling
    usa_110m.json                     # Bundled U.S. map topology for Plotly
```

## Analytical snapshot

The included model-complete snapshot has **7,944 hospital-condition records**, **2,444 hospitals**, and **1,197 counties**. Overall unweighted mean ERR is approximately **1.009**, and **54.8%** of records have ERR > 1. A record represents a hospital and readmission condition/procedure, not a unique patient.

ERR is the predicted readmission rate divided by the expected rate for similar patients. For example, ERR 1.05 means a predicted rate 5% above the expected rate, not a five-percentage-point increase in readmissions.

- Descriptive means across hospitals and conditions are unweighted. State means are not CMS risk-standardized state scores, and condition mix can affect pooled comparisons.
- Map and heatmap colors use a fixed ERR range of 0.90–1.10 with endpoint colors outside that range. Hover values remain exact; pale colors near 1 do not indicate statistical significance.
- Scatter correlations and trends use finite hospital-condition records after filtering. Hospitals and counties can repeat. These calculations do not adjust for that dependence or imply causation; the app does not display p-values or confidence bands for those trends.
- County socioeconomic and age variables describe communities, not individual patients. The original age-65+ variable uses a 2022 numerator and a 2023 population denominator, retained here to match the analytical extract.
- Adjusted findings are the saved results of the original mixed-effects analysis. They remain fixed when filters change; the dashboard does not refit the mixed-effects model.

## Data sources

- [CMS Hospital Readmissions Reduction Program](https://data.cms.gov/provider-data/dataset/9n3s-kdb3)
- [CMS HRRP definitions and context](https://www.cms.gov/medicare/quality/value-based-programs/hospital-readmissions)
- [HRSA Area Health Resources Files](https://data.hrsa.gov/data/download?data=AHRF)
- [Original analysis pipeline](../R/hospital_readmission_full.R) and [source-file requirements](../data/README.md)

## Deployment

The public instance runs on Posit Connect Cloud. To publish your own copy, publish this `shiny-app/` directory as a Shiny application and include `app.R`, `data/`, and `www/`. RStudio can detect package dependencies when publishing. Local `rsconnect/`, `.Rproj.user/`, session histories, and credentials are intentionally excluded from version control. Keep the deployment records on your own computer to update the same hosted app later.

## Author

Khanh Nguyen. Independent analysis using public data; no affiliation with CMS or HRSA is implied. 
