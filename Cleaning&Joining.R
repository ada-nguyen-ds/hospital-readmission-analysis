############################################################
# 0) SETUP

# Load packages 
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(glue)

library(lme4)
library(emmeans)
library(ggeffects)
library(broom.mixed)

library(performance)
library(DHARMa)

setwd("/Users/khanhnguyen/Desktop/Project Meo Hospital")

############################################################
# 1) READ DATA
############################################################

readm_raw <- read_csv("readmission.csv") %>% clean_names()
hosp_raw  <- read_csv("hospitals.csv")   %>% clean_names()
ahrf_raw  <- read_csv("ahrfpop.csv")     %>% clean_names()

zip_fips_raw <- read_csv("zip_fips.csv") %>% clean_names()

# Quick check
glimpse(readm_raw); glimpse(hosp_raw); glimpse(ahrf_raw)
skim(readm_raw); skim(hosp_raw); skim(ahrf_raw)

############################################################
# 2) CLEAN READMISSION DATA
############################################################
# Goal: Standardize IDs/programs, clean numeric variables, and compute observed_rate
readm <- readm_raw %>%
  transmute(
    provider_id     = as.character(facility_id),
    hospital_name   = str_squish(facility_name),
    readm_program   = str_squish(measure_name),
    
    # Convert text N/A -> NA 
    discharges_raw  = na_if(number_of_discharges, "N/A"),
    readm_raw2      = number_of_readmissions %>%
      na_if("Too Few to Report") %>%
      na_if("N/A"),
    err_raw         = na_if(excess_readmission_ratio, "N/A"),
    pred_raw        = na_if(predicted_readmission_rate, "N/A"),
    exp_raw         = na_if(expected_readmission_rate, "N/A"),
    
    # Parse numeric
    discharges      = parse_number(discharges_raw),
    readmission     = parse_number(readm_raw2),
    err             = parse_number(err_raw),
    predicted_rate  = parse_number(pred_raw),
    expected_rate   = parse_number(exp_raw),
    
    # observed rate (Note: discharges > 0)
    observed_rate   = readmission / discharges * 100
  )

readm_clean <- readm %>%
  filter(!is.na(discharges), discharges > 0) %>%
  filter(!is.na(err))

############################################################
# 3) CLEAN HOSPITAL DATA
############################################################
# Goal: Extract ownership/type/geography and standardize ZIP/state
hosp <- hosp_raw %>%
  transmute(
    provider_id        = as.character(facility_id),
    hospital_name      = str_squish(facility_name),
    address            = str_squish(address),
    city               = str_squish(city_town),
    state              = str_to_upper(str_squish(state)),
    zip                = str_extract(as.character(zip_code), "\\d{5}"),
    county_name        = str_squish(county_parish),
    hospital_type      = str_squish(hospital_type),
    hospital_ownership = str_squish(hospital_ownership),
    emergency_services = str_squish(emergency_services)
  ) %>%
  mutate(across(where(is.character), ~na_if(., "N/A"))) %>%
  mutate(across(where(is.character), ~na_if(., "Not Available"))) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  filter(!is.na(provider_id), provider_id != "") %>%
  distinct(provider_id, .keep_all = TRUE)

############################################################
# 4) ZIP -> COUNTY FIPS (IMPORTANT)
############################################################
#Note: ZIP → multiple counties; use ZIP + state for joins. Ensure county_fips is 5 chars with leading zeros.
zip_fips <- zip_fips_raw %>%
  transmute(
    zip        = str_extract(as.character(zip), "\\d{5}"),
    state      = str_to_upper(str_squish(state)),
    county_fips = str_extract(as.character(stcountyfp), "\\d{5}")
  ) %>%
  filter(!is.na(zip), !is.na(state), !is.na(county_fips)) %>%
  distinct(zip, state, county_fips)  # keep unique on 03 keys

# Join county_fips to hosp
hosp_geo <- hosp %>%
  left_join(zip_fips, by = c("zip", "state"))

# Join readmission + hosp_geo
readm_joined <- readm_clean %>%
  left_join(hosp_geo, by = "provider_id")

############################################################
# 5) JOIN AHRF SES DATA (COUNTY-LEVEL)
############################################################
# Note: ensure county_fips format 5 digits in order to join successfully
ahrf_pop <- ahrf_raw %>%
  transmute(
    county_fips = str_pad(as.character(fips_st_cnty), width = 5, side = "left", pad = "0"),
    
    total_population       = popn_est_23,
    pop_over65             = popn_est_ge65_22,
    pop_over65_pct         = popn_est_ge65_22 / popn_est_23 * 100,
    median_age             = medn_age_20,
    urban_pop_pct          = urban_popn_pct_20,
    
    per_capita_income      = per_cap_persnl_incom_22,
    median_hh_income_acs   = medn_hhi_acs_22,
    median_family_income   = medn_famly_incom_22,
    
    # Poverty: (income < 100% poverty line) / total determined * 100
    poverty_rate = (popn_incpovlt50_22 + popn_incpov_50_99_22) /
      pers_povty_stats_detrmnd_22 * 100,
    
    deep_poverty_pct       = pers_deep_povty_22,
    
    unemployment_rate = unemply_no_disblty_clf_18_64_22 /
      nonvtn_civln_popn_18_64_22 * 100,
    
    uninsured_rate         = pers_noins_lt65_pct_21
  )

# Join AHRF vào data chính
readm_final <- readm_joined %>%
  left_join(ahrf_pop, by = "county_fips")

# Final analytic dataset
final_clean <- readm_final %>%
  filter(!is.na(err)) %>%
  filter(!is.na(total_population)) %>%
  filter(!is.na(median_family_income))


############################################################

# 8) SCALE CONTINUOUS VARS (in order to compare effect size)
############################################################
final_clean <- final_clean %>%
  mutate(
    log_pop_sc = as.numeric(scale(log1p(total_population))),
    pov_sc     = as.numeric(scale(poverty_rate)),
    unemp_sc   = as.numeric(scale(unemployment_rate)),
    unins_sc   = as.numeric(scale(uninsured_rate))
  )

m_final_sc <- lmer(err ~ readm_program + hospital_ownership +
                     log_pop_sc + pov_sc + unemp_sc + unins_sc +
                     (1|provider_id),
                   data = final_clean, REML = TRUE)

summary(m_final_sc)

### 4.3 Coefficient Plot
coef_df <- tidy(m_final_sc, effects = "fixed", conf.int = TRUE) %>% filter(term != "(Intercept)")
coef_df <- coef_df %>%
  mutate(sig = ifelse(conf.low > 0 | conf.high < 0,
                      "Significant", "Not significant"))


