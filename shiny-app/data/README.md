# Dashboard analytical snapshot

`tableau_model_complete.csv.gz` is a gzip-compressed copy of the analytical extract used by the public Shiny dashboard. It contains public hospital-level CMS measures joined to county-level socioeconomic indicators, not patient-level records.

- Rows: 7,944 distinct hospital-condition/procedure pairs
- Hospitals: 2,444
- Counties: 1,197
- Key: `provider_id` and `measure`
- Outcome: `err`, the CMS excess readmission ratio
- Identifiers: provider and county FIPS codes are read as text to retain leading zeros

The snapshot is included so the dashboard can run without downloading and processing the raw source files. It is a derived model-complete subset and does not cover all U.S. hospitals. Incomplete model records and the Other/Tribal ownership group were excluded in the original analysis. Source periods vary; consult the variable names and app methods rather than treating every indicator as contemporaneous.

The original 2022 age-65+ numerator divided by the 2023 total county population is retained. Repeated county values across hospital-condition rows must not be interpreted as independent county observations.

`readr::read_csv()` reads this compressed file directly. The compressed content decompresses byte-for-byte to the CSV used for the public app. Raw inputs remain outside version control; their requirements are documented in [the repository data guide](../../data/README.md).
