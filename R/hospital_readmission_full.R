# Hospital Readmission Analysis: complete R pipeline
# Raw data -> cleaning and validated joins -> mixed-effects models -> exports

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(nlme)
  library(here)
  library(openxlsx)
})

# Expected project structure:
# data/readmission.csv, data/hospitals.csv, data/zip_fips.csv, data/ahrf.csv
# R/hospital_readmission_full.R, output/, visuals/, report/
raw_dir <- here("data")
out_dir <- here("output")
visual_dir <- here("visuals")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(visual_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- file.path(
  raw_dir,
  c("readmission.csv", "hospitals.csv", "zip_fips.csv", "ahrf.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing raw file(s): ", paste(basename(missing_files), collapse = ", "))
}

clean_number <- function(x) {
  parse_number(
    as.character(x),
    na = c("", "NA", "N/A", "Not Available", "Not Applicable", "Too Few to Report")
  )
}

clean_id <- function(x, width) {
  str_pad(str_extract(str_trim(as.character(x)), "[0-9]+"), width, pad = "0")
}

normalize_county <- function(x) {
  x <- str_to_upper(as.character(x))
  x <- str_replace_all(x, "[^A-Z0-9 ]", " ")
  x <- str_replace(
    x,
    " (CITY AND BOROUGH|CENSUS AREA|MUNICIPALITY|COUNTY|PARISH|BOROUGH|CITY)$",
    ""
  )
  x <- str_squish(x)
  recode(
    x,
    "E BATON ROUGE" = "EAST BATON ROUGE",
    "JEFFRSON DAVIS" = "JEFFERSON DAVIS",
    "THE DISTRICT" = "DISTRICT OF COLUMBIA",
    "DU PAGE" = "DUPAGE",
    "MC DUFFIE" = "MCDUFFIE",
    "MC DONOUGH" = "MCDONOUGH",
    "MC HENRY" = "MCHENRY",
    "MC LEAN" = "MCLEAN",
    "MC CRACKEN" = "MCCRACKEN",
    "DE KALB" = "DEKALB",
    "DE SOTO" = "DESOTO",
    "LA SALLE" = "LASALLE",
    "LA PAZ" = "LAPAZ",
    "ST JOHN BAPTIST" = "ST JOHN THE BAPTIST",
    "ST MARYS" = "ST MARY",
    "PRINCE GEORGES" = "PRINCE GEORGE S",
    "NORTHWEST ARTIC" = "NORTHWEST ARCTIC",
    "NORTH SLOPE BOROUH" = "NORTH SLOPE",
    "LAKE OF  WOODS" = "LAKE OF THE WOODS",
    "YELLOW MEDCINE" = "YELLOW MEDICINE",
    "SCOTT BLUFF" = "SCOTTS BLUFF",
    .default = x
  )
}

# -----------------------------------------------------------------------------
# 1. Import raw source files as text to preserve leading zeroes in IDs and FIPS.
# -----------------------------------------------------------------------------
readmission_raw <- read_csv(
  file.path(raw_dir, "readmission.csv"),
  col_types = cols(.default = col_character()), show_col_types = FALSE
)
hospitals_raw <- read_csv(
  file.path(raw_dir, "hospitals.csv"),
  col_types = cols(.default = col_character()), show_col_types = FALSE
)
zip_fips_raw <- read_csv(
  file.path(raw_dir, "zip_fips.csv"),
  col_types = cols(.default = col_character()), show_col_types = FALSE
)
ahrf_raw <- read_csv(
  file.path(raw_dir, "ahrf.csv"),
  col_types = cols(.default = col_character()), show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 2. Clean CMS readmission records and calculate the observed readmission rate.
# -----------------------------------------------------------------------------
readmission_clean <- readmission_raw %>%
  transmute(
    provider_id = clean_id(`Facility ID`, 6),
    hospital_name = str_squish(`Facility Name`),
    state = str_to_upper(str_trim(State)),
    measure_name = str_trim(`Measure Name`),
    discharges = clean_number(`Number of Discharges`),
    readmissions = clean_number(`Number of Readmissions`),
    err = clean_number(`Excess Readmission Ratio`),
    predicted_rate = clean_number(`Predicted Readmission Rate`),
    expected_rate = clean_number(`Expected Readmission Rate`),
    start_date = as.Date(`Start Date`, format = "%m/%d/%Y"),
    end_date = as.Date(`End Date`, format = "%m/%d/%Y")
  ) %>%
  filter(discharges > 0, !is.na(err)) %>%
  mutate(
    observed_rate = 100 * readmissions / discharges,
    measure = measure_name %>%
      str_remove(fixed("READM-30-")) %>%
      str_remove(fixed("-HRRP"))
  )

# -----------------------------------------------------------------------------
# 3. Clean the hospital directory.
# -----------------------------------------------------------------------------
hospitals_clean <- hospitals_raw %>%
  transmute(
    provider_id = clean_id(`Facility ID`, 6),
    hospital_name_directory = str_squish(`Facility Name`),
    address = str_squish(Address),
    city = str_squish(`City/Town`),
    state_directory = str_to_upper(str_trim(State)),
    zip = clean_id(`ZIP Code`, 5),
    county_name = str_squish(`County/Parish`),
    hospital_type = str_squish(`Hospital Type`),
    hospital_ownership = str_squish(`Hospital Ownership`),
    emergency_services = str_squish(`Emergency Services`),
    hospital_overall_rating = clean_number(`Hospital overall rating`),
    state_key = state_directory,
    county_key = normalize_county(county_name)
  ) %>%
  distinct(provider_id, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 4. Build an authoritative state/county-to-FIPS mapping from AHRF.
#    Only unique state-county keys are retained to prevent many-to-many joins.
# -----------------------------------------------------------------------------
ahrf_clean <- ahrf_raw %>%
  mutate(
    county_fips = clean_id(fips_st_cnty, 5),
    state_key = str_to_upper(str_trim(st_name_abbrev)),
    county_key = normalize_county(cnty_name)
  )

county_map <- ahrf_clean %>%
  distinct(state_key, county_key, county_fips) %>%
  group_by(state_key, county_key) %>%
  filter(n_distinct(county_fips) == 1) %>%
  ungroup()

hospitals_clean <- hospitals_clean %>%
  left_join(county_map, by = c("state_key", "county_key"), relationship = "many-to-one")

# Conservative fallback: use ZIP only if that ZIP maps to exactly one county.
unique_zip_map <- zip_fips_raw %>%
  transmute(
    zip = clean_id(ZIP, 5),
    county_fips_zip = clean_id(STCOUNTYFP, 5)
  ) %>%
  distinct() %>%
  group_by(zip) %>%
  filter(n_distinct(county_fips_zip) == 1) %>%
  ungroup()

hospitals_clean <- hospitals_clean %>%
  left_join(unique_zip_map, by = "zip", relationship = "many-to-one") %>%
  mutate(
    fips_method = case_when(
      !is.na(county_fips) ~ "state_county",
      !is.na(county_fips_zip) ~ "unique_zip",
      TRUE ~ "unmatched"
    ),
    county_fips = coalesce(county_fips, county_fips_zip)
  ) %>%
  select(-county_fips_zip, -state_key, -county_key)

# -----------------------------------------------------------------------------
# 5. Select and clean county-level socioeconomic measures from AHRF.
# -----------------------------------------------------------------------------
ses <- ahrf_clean %>%
  transmute(
    county_fips,
    county_name_ahrf = cnty_name,
    population_2023 = clean_number(popn_est_23),
    pct_age_65plus_2022 =
      100 * clean_number(popn_est_ge65_22) / clean_number(popn_est_23),
    median_family_income_2022 = clean_number(medn_famly_incom_22),
    unemployment_rate_2022 = clean_number(unemply_rate_ge16_22),
    poverty_rate_2022 = clean_number(pers_povty_pct_22),
    uninsured_under65_rate_2021 = clean_number(pers_noins_lt65_pct_21)
  ) %>%
  distinct(county_fips, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 6. Join sources and validate the analytical dataset.
# -----------------------------------------------------------------------------
analytic <- readmission_clean %>%
  left_join(hospitals_clean, by = "provider_id", relationship = "many-to-one") %>%
  left_join(ses, by = "county_fips", relationship = "many-to-one") %>%
  mutate(
    hospital_name_match =
      str_to_upper(hospital_name) == str_to_upper(hospital_name_directory),
    ownership_group = case_when(
      str_starts(hospital_ownership, "Government") ~ "Government",
      str_starts(hospital_ownership, "Voluntary non-profit") ~ "Voluntary nonprofit",
      hospital_ownership == "Proprietary" ~ "Proprietary",
      hospital_ownership == "Physician" ~ "Physician-owned",
      TRUE ~ "Other/Tribal"
    ),
    err_performance = if_else(
      err > 1,
      "Worse than expected (>1)",
      "At or better than expected (<=1)"
    )
  )

model_fields <- c(
  "err", "provider_id", "measure", "ownership_group", "emergency_services",
  "population_2023", "pct_age_65plus_2022", "median_family_income_2022",
  "unemployment_rate_2022", "poverty_rate_2022",
  "uninsured_under65_rate_2021"
)
analytic <- analytic %>%
  mutate(complete_model_record = if_all(all_of(model_fields), ~ !is.na(.x)))

duplicate_keys <- analytic %>%
  count(provider_id, measure_name) %>%
  filter(n > 1)
if (nrow(duplicate_keys) > 0) {
  stop("Duplicate provider-measure keys remain after joins. Review join keys.")
}

audit <- tibble(
  metric = c(
    "Raw CMS readmission rows", "Valid CMS hospital-measure rows",
    "Valid CMS providers", "Rows after corrected joins",
    "Providers after corrected joins", "Duplicate provider-measure keys",
    "Rows with county FIPS", "Rows complete for mixed model",
    "Hospital name mismatches"
  ),
  value = c(
    nrow(readmission_raw), nrow(readmission_clean),
    n_distinct(readmission_clean$provider_id), nrow(analytic),
    n_distinct(analytic$provider_id), nrow(duplicate_keys),
    sum(!is.na(analytic$county_fips)), sum(analytic$complete_model_record),
    sum(!coalesce(analytic$hospital_name_match, FALSE))
  )
)

write_csv(analytic, file.path(out_dir, "hospital_readmission_tableau_ready.csv"))
write_csv(audit, file.path(out_dir, "data_quality_audit.csv"))

# -----------------------------------------------------------------------------
# 7. Prepare the complete-case modeling dataset and standardized predictors.
# -----------------------------------------------------------------------------
dat <- analytic %>%
  filter(complete_model_record, ownership_group != "Other/Tribal") %>%
  mutate(
    provider_id = factor(provider_id),
    measure = relevel(factor(measure), ref = "HF"),
    ownership_group = relevel(factor(ownership_group), ref = "Voluntary nonprofit"),
    emergency_services = relevel(factor(emergency_services), ref = "Yes"),
    log_population = log(population_2023),
    z_poverty = as.numeric(scale(poverty_rate_2022)),
    z_unemployment = as.numeric(scale(unemployment_rate_2022)),
    z_uninsured = as.numeric(scale(uninsured_under65_rate_2021)),
    z_income = as.numeric(scale(median_family_income_2022)),
    z_age65 = as.numeric(scale(pct_age_65plus_2022)),
    z_log_population = as.numeric(scale(log_population))
  )

overall <- dat %>%
  summarise(
    observations = n(), hospitals = n_distinct(provider_id),
    counties = n_distinct(county_fips, na.rm = TRUE),
    mean_err = mean(err, na.rm = TRUE), median_err = median(err, na.rm = TRUE),
    sd_err = sd(err, na.rm = TRUE),
    pct_err_above_1 = mean(err > 1, na.rm = TRUE),
    mean_observed_rate = mean(observed_rate, na.rm = TRUE)
  )

by_measure <- dat %>%
  group_by(measure) %>%
  summarise(
    observations = n(), hospitals = n_distinct(provider_id),
    mean_err = mean(err, na.rm = TRUE), median_err = median(err, na.rm = TRUE),
    mean_observed_rate = mean(observed_rate, na.rm = TRUE),
    pct_err_above_1 = mean(err > 1, na.rm = TRUE), .groups = "drop"
  )

by_ownership <- dat %>%
  group_by(ownership_group) %>%
  summarise(
    observations = n(), hospitals = n_distinct(provider_id),
    mean_err = mean(err, na.rm = TRUE), median_err = median(err, na.rm = TRUE),
    pct_err_above_1 = mean(err > 1, na.rm = TRUE), .groups = "drop"
  )

write_csv(overall, file.path(out_dir, "summary_overall.csv"))
write_csv(by_measure, file.path(out_dir, "summary_by_measure.csv"))
write_csv(by_ownership, file.path(out_dir, "summary_by_ownership.csv"))

# -----------------------------------------------------------------------------
# 8. Fit nested linear mixed-effects models with hospital random intercepts.
# -----------------------------------------------------------------------------
model_control <- lmeControl(
  opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE
)
null_model <- lme(
  err ~ measure, random = ~ 1 | provider_id, data = dat,
  method = "ML", control = model_control
)
base_model <- lme(
  err ~ measure + ownership_group + emergency_services,
  random = ~ 1 | provider_id, data = dat,
  method = "ML", control = model_control
)
ses_model <- lme(
  err ~ measure + ownership_group + emergency_services + z_poverty +
    z_unemployment + z_uninsured + z_income + z_age65 + z_log_population,
  random = ~ 1 | provider_id, data = dat,
  method = "ML", control = model_control
)
final_model <- update(ses_model, method = "REML")

model_compare <- anova(null_model, base_model, ses_model)
model_compare_export <- as.data.frame(model_compare)
model_compare_export$model <- rownames(model_compare_export)
rownames(model_compare_export) <- NULL
model_compare_export <- model_compare_export %>% select(model, -any_of("call"), everything())
write_csv(model_compare_export, file.path(out_dir, "model_comparison.csv"))

coef_tab <- as.data.frame(summary(final_model)$tTable)
coef_tab$term <- rownames(coef_tab)
rownames(coef_tab) <- NULL
names(coef_tab)[1:5] <- c("estimate", "std_error", "df", "t_value", "p_value")
fixed_ci <- intervals(final_model, which = "fixed")$fixed
coef_tab$ci_lower <- fixed_ci[coef_tab$term, "lower"]
coef_tab$ci_upper <- fixed_ci[coef_tab$term, "upper"]
coef_tab <- coef_tab %>%
  select(term, estimate, std_error, df, t_value, p_value, ci_lower, ci_upper)
write_csv(coef_tab, file.path(out_dir, "mixed_model_coefficients.csv"))

variance_components <- VarCorr(final_model)
hospital_variance <- as.numeric(variance_components[1, "Variance"])
residual_variance <- as.numeric(variance_components[2, "Variance"])
icc <- hospital_variance / (hospital_variance + residual_variance)
model_stats <- tibble(
  metric = c(
    "Observations", "Hospitals", "Counties",
    "Hospital random-intercept variance", "Residual variance",
    "Intraclass correlation", "Intraclass correlation percent",
    "AIC_ML", "BIC_ML"
  ),
  value = c(
    nrow(dat), n_distinct(dat$provider_id),
    n_distinct(dat$county_fips, na.rm = TRUE),
    hospital_variance, residual_variance, icc, 100 * icc,
    AIC(ses_model), BIC(ses_model)
  )
)
write_csv(model_stats, file.path(out_dir, "model_statistics.csv"))

# -----------------------------------------------------------------------------
# 9. Create portfolio-ready visualizations.
# -----------------------------------------------------------------------------
theme_portfolio <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#12304A", size = 16),
    plot.subtitle = element_text(color = "#526574"),
    panel.grid.minor = element_blank(), legend.position = "bottom"
  )

p1 <- ggplot(dat, aes(err)) +
  geom_histogram(binwidth = 0.025, fill = "#1E7D8D", color = "white") +
  geom_vline(xintercept = 1, color = "#D95D39", linewidth = 0.9, linetype = 2) +
  labs(
    title = "Distribution of Excess Readmission Ratios",
    subtitle = "An ERR above 1 indicates more readmissions than CMS expected",
    x = "Excess readmission ratio (ERR)", y = "Hospital-condition observations"
  ) + theme_portfolio
ggsave(file.path(visual_dir, "01_err_distribution.png"), p1,
       width = 9, height = 5.4, dpi = 220)

p2 <- ggplot(
  dat, aes(x = reorder(ownership_group, err, median), y = err, fill = ownership_group)
) +
  geom_boxplot(outlier.alpha = 0.18, width = 0.72) +
  geom_hline(yintercept = 1, color = "#D95D39", linetype = 2) +
  coord_flip() + guides(fill = "none") +
  scale_fill_manual(values = c("#2A6F97", "#4D908E", "#90BE6D", "#F9844A")) +
  labs(
    title = "ERR Varies Across Hospital Ownership Groups",
    subtitle = "Unadjusted distributions; adjusted estimates appear in model results",
    x = NULL, y = "Excess readmission ratio"
  ) + theme_portfolio
ggsave(file.path(visual_dir, "02_err_by_ownership.png"), p2,
       width = 9, height = 5.4, dpi = 220)

p3 <- ggplot(
  by_measure, aes(x = reorder(measure, mean_err), y = mean_err, fill = mean_err > 1)
) +
  geom_col(width = 0.68) +
  geom_hline(yintercept = 1, color = "#D95D39", linetype = 2) +
  coord_flip() + guides(fill = "none") +
  scale_fill_manual(values = c("#1E7D8D", "#D95D39")) +
  labs(
    title = "Average ERR by Readmission Measure",
    subtitle = "Descriptive means across eligible hospitals",
    x = NULL, y = "Mean excess readmission ratio"
  ) + theme_portfolio
ggsave(file.path(visual_dir, "03_mean_err_by_measure.png"), p3,
       width = 9, height = 5.4, dpi = 220)

plot_coef <- coef_tab %>%
  filter(str_starts(term, "z_")) %>%
  mutate(label = recode(
    term,
    z_poverty = "Poverty rate",
    z_unemployment = "Unemployment rate",
    z_uninsured = "Uninsured rate (under 65)",
    z_income = "Median family income",
    z_age65 = "Population age 65+",
    z_log_population = "County population (log)"
  ))
p4 <- ggplot(plot_coef, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 0, color = "#8997A1", linetype = 2) +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper), width = 0.16,
    orientation = "y", color = "#12304A"
  ) +
  geom_point(size = 3.2, color = "#1E7D8D") +
  labs(
    title = "Adjusted Socioeconomic Associations with ERR",
    subtitle = "Change per one-SD increase; 95% confidence intervals",
    x = "Adjusted coefficient", y = NULL
  ) + theme_portfolio
ggsave(file.path(visual_dir, "04_ses_coefficient_plot.png"), p4,
       width = 9, height = 5.4, dpi = 220)

random_effects <- tibble(
  provider_id = rownames(ranef(final_model)),
  random_intercept = ranef(final_model)[, 1]
)
p5 <- ggplot(random_effects, aes(random_intercept)) +
  geom_histogram(bins = 35, fill = "#355070", color = "white") +
  geom_vline(xintercept = 0, color = "#D95D39", linetype = 2) +
  labs(
    title = "Residual Between-Hospital Variation",
    subtitle = "Hospital random intercepts after covariate adjustment",
    x = "Hospital random intercept", y = "Hospitals"
  ) + theme_portfolio
ggsave(file.path(visual_dir, "05_hospital_random_effects.png"), p5,
       width = 9, height = 5.4, dpi = 220)

# Tableau model-complete extract.
tableau_model_complete <- dat %>%
  select(
    provider_id, hospital_name, state, county_fips, county_name, measure,
    discharges, readmissions, err, observed_rate, predicted_rate, expected_rate,
    ownership_group, emergency_services, population_2023,
    pct_age_65plus_2022, median_family_income_2022,
    unemployment_rate_2022, poverty_rate_2022,
    uninsured_under65_rate_2021
  )
write_csv(
  tableau_model_complete,
  file.path(out_dir, "tableau_model_complete.csv")
)

# -----------------------------------------------------------------------------
# 10. Build one formatted Excel report containing results, charts, and data.
# -----------------------------------------------------------------------------
excel_file <- file.path(out_dir, "Hospital_Readmission_Analysis_Report.xlsx")
wb <- createWorkbook(creator = "Khanh Nguyen")
title_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = "#FFFFFF", fgFill = "#1F436D", textDecoration = "bold", halign = "left", valign = "center")
section_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = "#1F436D", fgFill = "#EAF2F7", textDecoration = "bold", halign = "left")
header_style <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#1F436D", fgFill = "#EAF2F7", textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE, border = "bottom", borderColour = "#1F436D")
body_style <- createStyle(fontName = "Arial", fontSize = 10, border = "bottom", borderColour = "#D9E2EA")
note_style <- createStyle(fontName = "Arial", fontSize = 11, fontColour = "#34495E", fgFill = "#EAF2F7", wrapText = TRUE, valign = "top")
percent_style <- createStyle(numFmt = "0.0%")
decimal_style <- createStyle(numFmt = "0.0000")
pvalue_style <- createStyle(numFmt = "0.0000")

if (FALSE) { # Previous 10-sheet layout retained only for reference.
add_table_sheet <- function(wb, sheet, title, section, x) {
  x <- as.data.frame(x); names(x) <- make.unique(names(x), sep = "_")
  addWorksheet(wb, sheet, gridLines = FALSE)
  writeData(wb, sheet, title, 1, 1); mergeCells(wb, sheet, cols = 1:max(2, ncol(x)), rows = 1)
  addStyle(wb, sheet, title_style, rows = 1, cols = 1:max(2, ncol(x)), gridExpand = TRUE); setRowHeights(wb, sheet, 1, 30)
  writeData(wb, sheet, section, 3, 1); mergeCells(wb, sheet, cols = 1:max(2, ncol(x)), rows = 3)
  addStyle(wb, sheet, section_style, rows = 3, cols = 1:max(2, ncol(x)), gridExpand = TRUE)
  writeData(wb, sheet, x, 4, 1, headerStyle = header_style, borders = "none")
  if (nrow(x) > 0) addStyle(wb, sheet, body_style, rows = 5:(nrow(x) + 4), cols = 1:ncol(x), gridExpand = TRUE, stack = TRUE)
  addFilter(wb, sheet, rows = 4, cols = 1:ncol(x)); freezePane(wb, sheet, firstActiveRow = 5)
  setColWidths(wb, sheet, cols = 1:ncol(x), widths = 15); setColWidths(wb, sheet, cols = 1:min(2, ncol(x)), widths = 24)
  invisible(NULL)
}

add_data_sheet <- function(wb, sheet, title, subtitle, x) {
  x <- as.data.frame(x); names(x) <- make.unique(names(x), sep = "_")
  addWorksheet(wb, sheet, gridLines = FALSE)
  writeData(wb, sheet, title, 1, 1); mergeCells(wb, sheet, cols = 1:ncol(x), rows = 1)
  addStyle(wb, sheet, title_style, rows = 1, cols = 1:ncol(x), gridExpand = TRUE); setRowHeights(wb, sheet, 1, 30)
  writeData(wb, sheet, subtitle, 2, 1); mergeCells(wb, sheet, cols = 1:ncol(x), rows = 2)
  writeData(wb, sheet, x, 4, 1, headerStyle = header_style, borders = "none")
  addFilter(wb, sheet, rows = 4, cols = 1:ncol(x)); freezePane(wb, sheet, firstActiveRow = 5, firstActiveCol = 2)
  setColWidths(wb, sheet, cols = 1:ncol(x), widths = 15)
  invisible(NULL)
}

addWorksheet(wb, "Executive Summary", gridLines = FALSE)
writeData(wb, "Executive Summary", "Hospital Readmission Rate Analysis", 1, 1)
mergeCells(wb, "Executive Summary", cols = 1:8, rows = 1)
addStyle(wb, "Executive Summary", title_style, rows = 1, cols = 1:8, gridExpand = TRUE); setRowHeights(wb, "Executive Summary", 1, 30)
writeData(wb, "Executive Summary", "Mixed-effects analysis of CMS readmission performance and county socioeconomic context.", 2, 1)
mergeCells(wb, "Executive Summary", cols = 1:8, rows = 2)
writeData(wb, "Executive Summary", "Analysis Summary", 4, 1); mergeCells(wb, "Executive Summary", cols = 1:8, rows = 4)
addStyle(wb, "Executive Summary", section_style, rows = 4, cols = 1:8, gridExpand = TRUE)
executive_summary <- data.frame(Metric = c("Model observations", "Hospitals", "Counties", "Mean ERR", "Share with ERR > 1", "Hospital-level ICC"), Value = c(nrow(dat), n_distinct(dat$provider_id), n_distinct(dat$county_fips, na.rm = TRUE), mean(dat$err, na.rm = TRUE), mean(dat$err > 1, na.rm = TRUE), icc), check.names = FALSE)
writeData(wb, "Executive Summary", executive_summary, 5, 1, headerStyle = header_style)
addStyle(wb, "Executive Summary", body_style, rows = 6:11, cols = 1:2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Executive Summary", decimal_style, rows = 9, cols = 2, stack = TRUE)
addStyle(wb, "Executive Summary", percent_style, rows = 10:11, cols = 2, gridExpand = TRUE, stack = TRUE)
writeData(wb, "Executive Summary", "Key Finding", 13, 1); mergeCells(wb, "Executive Summary", cols = 1:8, rows = 13)
addStyle(wb, "Executive Summary", section_style, rows = 13, cols = 1:8, gridExpand = TRUE)
writeData(wb, "Executive Summary", "After adjustment for readmission condition and hospital characteristics, county poverty and unemployment were positively associated with excess readmission ratios. Results describe associations, not causal effects.", 14, 1)
mergeCells(wb, "Executive Summary", cols = 1:8, rows = 14); addStyle(wb, "Executive Summary", note_style, rows = 14, cols = 1:8, gridExpand = TRUE)
setRowHeights(wb, "Executive Summary", 14, 44); setColWidths(wb, "Executive Summary", 1, 34); setColWidths(wb, "Executive Summary", 2:8, 15)

add_table_sheet(wb, "Data Quality", "Hospital Readmission Analysis — Data Quality", "Data Quality Audit", audit)
add_table_sheet(wb, "Overall Summary", "Hospital Readmission Analysis — Overall Summary", "Overall Readmission Summary", overall)
add_table_sheet(wb, "By Measure", "Hospital Readmission Analysis — Condition Summary", "Readmission Results by Condition", by_measure)
add_table_sheet(wb, "By Ownership", "Hospital Readmission Analysis — Ownership Summary", "Readmission Results by Hospital Ownership", by_ownership)
add_table_sheet(wb, "Model Comparison", "Hospital Readmission Analysis — Model Comparison", "Nested Mixed-Effects Models", model_compare_export)
add_table_sheet(wb, "Model Coefficients", "Hospital Readmission Analysis — Model Coefficients", "Final Mixed-Effects Model Estimates", coef_tab)
add_table_sheet(wb, "Model Statistics", "Hospital Readmission Analysis — Model Statistics", "Final Model Statistics", model_stats)
addStyle(wb, "Overall Summary", percent_style, rows = 5, cols = which(names(overall) == "pct_err_above_1"), stack = TRUE)
addStyle(wb, "By Measure", percent_style, rows = 5:(nrow(by_measure) + 4), cols = which(names(by_measure) == "pct_err_above_1"), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "By Ownership", percent_style, rows = 5:(nrow(by_ownership) + 4), cols = which(names(by_ownership) == "pct_err_above_1"), gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Model Coefficients", pvalue_style, rows = 5:(nrow(coef_tab) + 4), cols = which(names(coef_tab) == "p_value"), gridExpand = TRUE, stack = TRUE)

addWorksheet(wb, "Visualizations", gridLines = FALSE)
writeData(wb, "Visualizations", "Hospital Readmission Analysis — Visualizations", 1, 1)
mergeCells(wb, "Visualizations", cols = 1:16, rows = 1); addStyle(wb, "Visualizations", title_style, rows = 1, cols = 1:16, gridExpand = TRUE)
writeData(wb, "Visualizations", "Descriptive and adjusted model results", 3, 1); mergeCells(wb, "Visualizations", cols = 1:16, rows = 3)
addStyle(wb, "Visualizations", section_style, rows = 3, cols = 1:16, gridExpand = TRUE)
chart_files <- file.path(visual_dir, c("01_err_distribution.png", "02_err_by_ownership.png", "03_mean_err_by_measure.png", "04_ses_coefficient_plot.png", "05_hospital_random_effects.png"))
chart_rows <- c(5, 5, 25, 25, 45); chart_cols <- c(1, 9, 1, 9, 1)
for (i in seq_along(chart_files)) if (file.exists(chart_files[i])) insertImage(wb, "Visualizations", chart_files[i], startRow = chart_rows[i], startCol = chart_cols[i], width = 5.8, height = 3.5, units = "in")
setColWidths(wb, "Visualizations", cols = 1:16, widths = 11)

add_data_sheet(wb, "Tableau Data", "Hospital Readmission Analysis — Tableau Data", "Model-complete analytical records used for statistical results.", tableau_model_complete)
add_data_sheet(wb, "Cleaned Data", "Hospital Readmission Analysis — Cleaned Data", "Validated hospital-condition dataset produced from the raw CMS and AHRF files.", analytic)

saveWorkbook(wb, excel_file, overwrite = TRUE)
}

# Compact four-sheet layout based on the PACTS workbook.
write_section <- function(sheet, row, heading, x) {
  x <- as.data.frame(x); names(x) <- make.unique(names(x), sep = "_"); last_col <- max(2, ncol(x))
  writeData(wb, sheet, heading, startRow = row, startCol = 1); mergeCells(wb, sheet, cols = 1:last_col, rows = row)
  addStyle(wb, sheet, section_style, rows = row, cols = 1:last_col, gridExpand = TRUE)
  writeData(wb, sheet, x, startRow = row + 1, startCol = 1, headerStyle = header_style)
  if (nrow(x) > 0) {
    rows <- (row + 2):(row + nrow(x) + 1)
    addStyle(wb, sheet, body_style, rows = rows, cols = 1:ncol(x), gridExpand = TRUE, stack = TRUE)
    if (ncol(x) > 1) addStyle(wb, sheet, createStyle(halign = "center"), rows = rows, cols = 2:ncol(x), gridExpand = TRUE, stack = TRUE)
  }
  row + nrow(x) + 3
}

addWorksheet(wb, "Summary", gridLines = FALSE)
writeData(wb, "Summary", "Hospital Readmission Rate Analysis", startRow = 1, startCol = 1); mergeCells(wb, "Summary", cols = 1:10, rows = 1)
addStyle(wb, "Summary", title_style, rows = 1, cols = 1:10, gridExpand = TRUE); setRowHeights(wb, "Summary", 1, 30)
writeData(wb, "Summary", "Mixed-effects analysis of CMS readmission performance and county socioeconomic context.", startRow = 2, startCol = 1); mergeCells(wb, "Summary", cols = 1:10, rows = 2)
executive_summary <- data.frame(Metric = c("Model observations", "Hospitals", "Counties", "Mean ERR", "Share with ERR > 1", "Hospital-level ICC"), Value = c(nrow(dat), n_distinct(dat$provider_id), n_distinct(dat$county_fips, na.rm = TRUE), mean(dat$err, na.rm = TRUE), mean(dat$err > 1, na.rm = TRUE), icc), check.names = FALSE)
next_row <- write_section("Summary", 4, "Analysis Summary", executive_summary)
writeData(wb, "Summary", "Key finding: County poverty and unemployment were positively associated with ERR after adjustment. These are associations, not causal effects.", startRow = next_row, startCol = 1)
mergeCells(wb, "Summary", cols = 1:10, rows = next_row); addStyle(wb, "Summary", note_style, rows = next_row, cols = 1:10, gridExpand = TRUE); setRowHeights(wb, "Summary", next_row, 36)
next_row <- write_section("Summary", next_row + 2, "Results by Readmission Measure", by_measure)
next_row <- write_section("Summary", next_row, "Results by Hospital Ownership", by_ownership)
write_section("Summary", next_row, "Data Quality Audit", audit)
setColWidths(wb, "Summary", 1, 34); setColWidths(wb, "Summary", 2:10, 17); freezePane(wb, "Summary", firstActiveRow = 4)
addStyle(wb, "Summary", decimal_style, rows = 9, cols = 2, stack = TRUE); addStyle(wb, "Summary", percent_style, rows = 10:11, cols = 2, gridExpand = TRUE, stack = TRUE)

addWorksheet(wb, "Model Results", gridLines = FALSE)
writeData(wb, "Model Results", "Hospital Readmission Analysis — Model Results", startRow = 1, startCol = 1); mergeCells(wb, "Model Results", cols = 1:10, rows = 1)
addStyle(wb, "Model Results", title_style, rows = 1, cols = 1:10, gridExpand = TRUE); setRowHeights(wb, "Model Results", 1, 30)
model_row <- write_section("Model Results", 3, "Nested Mixed-Effects Model Comparison", model_compare_export)
model_row <- write_section("Model Results", model_row, "Final Model Statistics", model_stats)
coef_start <- model_row; write_section("Model Results", coef_start, "Final Mixed-Effects Model Coefficients", coef_tab)
addStyle(wb, "Model Results", pvalue_style, rows = (coef_start + 2):(coef_start + nrow(coef_tab) + 1), cols = which(names(coef_tab) == "p_value"), gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Model Results", 1, 38); setColWidths(wb, "Model Results", 2:10, 16); freezePane(wb, "Model Results", firstActiveRow = 4)

addWorksheet(wb, "Visualizations", gridLines = FALSE)
writeData(wb, "Visualizations", "Hospital Readmission Analysis — Visualizations", startRow = 1, startCol = 1); mergeCells(wb, "Visualizations", cols = 1:10, rows = 1)
addStyle(wb, "Visualizations", title_style, rows = 1, cols = 1:10, gridExpand = TRUE)
chart_files <- file.path(visual_dir, c("01_err_distribution.png", "02_err_by_ownership.png", "03_mean_err_by_measure.png", "04_ses_coefficient_plot.png", "05_hospital_random_effects.png"))
chart_rows <- c(3, 27, 51, 75, 99)
for (i in seq_along(chart_files)) if (file.exists(chart_files[i])) insertImage(wb, "Visualizations", chart_files[i], startRow = chart_rows[i], startCol = 1, width = 8.8, height = 5.3, units = "in")
setColWidths(wb, "Visualizations", cols = 1:10, widths = 12)

analysis_data <- as.data.frame(tableau_model_complete); names(analysis_data) <- make.unique(names(analysis_data), sep = "_")
addWorksheet(wb, "Analysis Data", gridLines = FALSE)
writeData(wb, "Analysis Data", "Hospital Readmission Analysis — Analysis Data", startRow = 1, startCol = 1); mergeCells(wb, "Analysis Data", cols = 1:ncol(analysis_data), rows = 1)
addStyle(wb, "Analysis Data", title_style, rows = 1, cols = 1:ncol(analysis_data), gridExpand = TRUE); setRowHeights(wb, "Analysis Data", 1, 30)
writeData(wb, "Analysis Data", "Model-complete records; the full cleaned dataset remains available as hospital_readmission_tableau_ready.csv.", startRow = 2, startCol = 1); mergeCells(wb, "Analysis Data", cols = 1:ncol(analysis_data), rows = 2)
writeData(wb, "Analysis Data", analysis_data, startRow = 4, startCol = 1, headerStyle = header_style)
addFilter(wb, "Analysis Data", rows = 4, cols = 1:ncol(analysis_data)); freezePane(wb, "Analysis Data", firstActiveRow = 5, firstActiveCol = 2)
addStyle(wb, "Analysis Data", createStyle(halign = "left"), rows = 5:(nrow(analysis_data) + 4), cols = 1:ncol(analysis_data), gridExpand = TRUE)
setColWidths(wb, "Analysis Data", cols = 1:ncol(analysis_data), widths = 15)

saveWorkbook(wb, excel_file, overwrite = TRUE)

cat("\nDATA QUALITY AUDIT\n")
print(audit, n = Inf)
cat("\nOVERALL SUMMARY\n")
print(overall)
cat("\nMODEL COMPARISON\n")
print(model_compare)
cat("\nMODEL STATISTICS\n")
print(model_stats, n = Inf)
cat("\nPipeline completed successfully.\n")
cat("Tables and cleaned data:", out_dir, "\n")
cat("Charts:", visual_dir, "\n")
cat("Excel report:", excel_file, "\n")
