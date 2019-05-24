# Load libraries
library(dplyr)

# Load data
op_zones <- read.csv("opportunity_zones.csv", header = TRUE, stringsAsFactors = FALSE)
acs_2010 <- read.csv("acs_2010_clean.csv", header = TRUE, stringsAsFactors = FALSE)
acs_2016 <- read.csv("acs_2016_clean.csv", header = TRUE, stringsAsFactors = FALSE)

# Houston MSA Stats
hou_med_inc_2010 <- 60777
hou_med_inc_2016 <- 63802
hou_med_inc_diff <- hou_med_inc_2016 - hou_med_inc_2010
hou_med_inc_growth <- hou_med_inc_diff/hou_med_inc_2010

hou_edu_2010 <- 229219
hou_edu_2016 <- 274496
hou_edu_diff <- hou_edu_2016 - hou_edu_2010
hou_edu_growth <- hou_edu_diff/hou_edu_2010

hou_white_2010 <- 1133978
hou_white_2016 <- 1305482
hou_white_diff <- hou_white_2016 - hou_white_2010
hou_white_growth <- hou_white_diff/hou_white_2010

hou_med_val_2010 <- 123800
hou_med_val_2016 <- 140300
hou_med_val_diff <- hou_med_val_2016 - hou_med_val_2010
hou_med_val_growth <- hou_med_val_diff/hou_med_val_2010

hou_med_rent_2010 <- 793
hou_med_rent_2016 <- 898
hou_med_rent_diff <- hou_med_rent_2016 - hou_med_rent_2010
hou_med_rent_growth <- hou_med_rent_diff/hou_med_rent_2010

# Gentrification study to classify weak and strong gentrified census tracts
# https://media.terry.uga.edu/socrates/contact/documents/2017/04/05/Determinants_of_Gentrification.pdf

# Join and wrangle data
acs_data <- acs_2010 %>%
  left_join(acs_2016, by = "GEOID") %>%
  left_join(op_zones, by = c("GEOID" = "census_tract")) %>%
  filter(substr(GEOID, 1,2) == "48",
         substr(GEOID, 3,5) == "201" | substr(GEOID, 3,5) == "339" | substr(GEOID, 3,5) == "157") %>%
  mutate(pct_white_2010 = white_pop_2010/total_pop_2010,
         pct_nonwhite_2010 = (total_pop_2010 - white_pop_2010)/total_pop_2010,
         pct_black_2010 = black_pop_2010/total_pop_2010,
         pct_asian_2010 = asian_pop_2010/total_pop_2010,
         pct_hispanic_2010 = hispanic_pop_2010/total_pop_2010,
         pct_renters_2010 = renters_2010/total_pop_2010,
         pct_inpov_2010 = poverty_2010/total_pop_2010,
         pct_college_2010 = college_2010/total_pop_2010,
         pct_white_2016 = white_pop_2016/total_pop_2016,
         pct_nonwhite_2016 = (total_pop_2016 - white_pop_2016)/total_pop_2016,
         pct_black_2016 = black_pop_2016/total_pop_2016,
         pct_asian_2016 = asian_pop_2016/total_pop_2016,
         pct_hispanic_2016 = hispanic_pop_2016/total_pop_2016,
         pct_renters_2016 = renters_2016/total_pop_2016,
         pct_inpov_2016 = poverty_2016/total_pop_2016,
         pct_college_2016 = college_2016/total_pop_2016,
         nonwhite_pop_2010 = total_pop_2010 - white_pop_2010,
         nonwhite_pop_2016 = total_pop_2016 - white_pop_2016) %>%
  mutate(white_change = (white_pop_2016 - white_pop_2010)/white_pop_2010,
         nonwhite_change = (nonwhite_pop_2016 - nonwhite_pop_2010)/nonwhite_pop_2010,
         black_change = (black_pop_2016 - black_pop_2010)/black_pop_2010,
         asian_change = (asian_pop_2016 - asian_pop_2010)/asian_pop_2010,
         hispanic_change = (hispanic_pop_2016 - hispanic_pop_2010)/hispanic_pop_2010,
         inpov_change = (poverty_2016 - poverty_2010)/poverty_2010,
         college_change = (college_2016 - college_2010)/college_2010,
         pop_change = (total_pop_2016 - total_pop_2010)/total_pop_2010,
         med_inc_change = (med_income_2016 - med_income_2010)/med_income_2010,
         med_rent_change = (med_rent_2016 - med_rent_2010)/med_rent_2010,
         med_val_change = (med_value_2016 - med_value_2010)/med_value_2010,
         renters_change = (renters_2016 - renters_2010)/renters_2010,
         total_houses_change = (total_houses_2016 - total_houses_2010)/total_houses_2010,
         op_zone = ifelse(!is.na(tract_type), 1, 0),
         op_zone_eligible = ifelse(pct_inpov_2016 >= .2 & med_income_2016 <= 0.8*hou_med_inc_2016, 1, 0)) %>%
  mutate(low_inc_vulnerability = ifelse(pct_inpov_2016 > median(pct_inpov_2016, na.rm = TRUE), 1, 0),
         edu_vulnerability = ifelse(pct_college_2016 < median(pct_college_2016, na.rm = TRUE), 1, 0),
         nonwhite_vulnerability = ifelse(pct_nonwhite_2016 > median(pct_nonwhite_2016, na.rm = TRUE), 1, 0),
         renter_vulnerability = ifelse(pct_renters_2016 > median(pct_renters_2016, na.rm = TRUE), 1, 0),
         edu_gentrification = ifelse(college_change > hou_edu_growth, 1, 0),
         med_inc_gentrification = ifelse(med_inc_change > hou_med_inc_growth, 1, 0),
         white_gentrification = ifelse(white_change > hou_white_growth, 1, 0),
         med_rent_gentrification = ifelse(med_rent_change > hou_med_rent_growth, 1, 0),
         med_val_gentrification = ifelse(med_val_change > hou_med_val_growth, 1, 0)) %>%
  mutate(vulnerable = ifelse(rowSums(.[63:66], na.rm = TRUE) >= 3, 1, 0),
         dem_gentrification = ifelse((edu_gentrification == 1 | med_inc_gentrification == 1) & white_gentrification == 1, 1, 0),
         inv_gentrification = ifelse(med_rent_gentrification == 1 | med_val_gentrification == 1, 1, 0),
         fed_vulnerable = ifelse(med_income_2016 < hou_med_inc_2016, 1, 0),
         fed_vulnerable_2010 = ifelse(med_income_2010 < hou_med_inc_2010, 1, 0)) %>%
  mutate(fed_gentrifying = ifelse(fed_vulnerable_2010 == 1 & (med_rent_change > median(med_rent_change, na.rm = TRUE) | med_val_change > median(med_val_change, na.rm = TRUE)) & college_change > median(college_change, na.rm = TRUE), 1, 0)) %>%
  dplyr::select(-c(state, county, tract_type, acs_data_source))

