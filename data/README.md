# Data

The analysis uses public data from the following sources:

- CMS Hospital Readmissions Reduction Program data
- CMS hospital characteristics
- ZIP-to-county FIPS crosswalk data
- Area Health Resources Files

The raw CSV files are not stored in this repository because the combined extracts are large. Place the required files in this folder before running the analysis:

```text
data/
├── readmission.csv
├── hospitals.csv
├── zip_fips.csv
└── ahrf.csv
```

Run the project from the repository root so the `here` package can resolve the folders correctly. The main workflow checks for all four files and stops with a clear error if any file is missing.

Hospital and geographic identifiers are imported as text to preserve leading zeroes.
