# Load library
library(tidyverse)
library(tidycensus)

# Intialize API key
census_api_key("4f0d11faa7b869eff627c61b6dc24895b5a9fefc", install = TRUE, overwrite = TRUE)

# Get ACS data
oz_bg_data <- get_acs(geography = "block group",
          year = 2016,
          survey = "acs5",
          state = 48,
          county = c(201, 339, 157),
          variables = c(total_pop_2016 = "B01003_001",
                        white_pop_2016 = "B02001_002",
                        black_pop_2016 = "B02001_003",
                        asian_pop_2016 = "B02001_005",
                        hispanic_pop_2016 = "B03003_003",
                        med_income_2016 = "B19013_001",
                        total_houses_2016 = "B25034_001"))

# Clean and reshape data for use
oz_bg_clean <- oz_bg_data %>%
  select(-c(NAME, moe)) %>%
  spread(variable, estimate) %>%
  mutate(pct_white_2016 = white_pop_2016/total_pop_2016,
         pct_black_2016 = black_pop_2016/total_pop_2016,
         pct_asian_2016 = asian_pop_2016/total_pop_2016,
         pct_hispanic_2016 = hispanic_pop_2016/total_pop_2016)
