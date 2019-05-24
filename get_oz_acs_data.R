# Load libraries
library(dplyr)
library(readxl)
library(tidycensus)
library(purrr)
library(tidyr)

## If throwing error "Error: Columns 1, 2, 3, 4, 5, … (and 2 more) must be named."
# library(devtools)
# devtools::install_github("mdlincoln/clipr")
# devtools::install_github("tidyverse/dplyr@rc_0.8.0")
# devtools::install_github("tidyverse/hms")
# devtools::install_github("r-spatial/sf")
# devtools::install_github("tidyverse/stringr")
# devtools::install_github("walkerke/tidycensus")

### Get OZ data ###
oz_df <- read_excel("Designated QOZs.12.14.18.xlsx", sheet = 1, skip = 4)

# Rename columns 
colnames(oz_df) <- c("state", "county", "census_tract", "tract_type", "acs_data_source")

# write.csv(oz_df, "opportunity_zones.csv", row.names = FALSE)

### Get ACS data ###

## Load census variables from tidy data package
# v15 <- load_variables(2016, "acs5", cache = TRUE)
# 
# View(v15)

# Intialize API key
census_api_key("4f0d11faa7b869eff627c61b6dc24895b5a9fefc", install = TRUE, overwrite = TRUE)

# Get US states
us <- unique(fips_codes$state)[1:51]

# Get ACS data by census tract for all states

# 2016 ACS
acs_2016_raw <- map_df(us, function(x) {
  get_acs(geography = "tract",
          year = 2016,
          survey = "acs5",
          state = x,
          variables = c(total_pop_2016 = "B01003_001",
                        white_pop_2016 = "B02001_002",
                        black_pop_2016 = "B02001_003",
                        asian_pop_2016 = "B02001_005",
                        hispanic_pop_2016 = "B03003_003",
                        med_income_2016 = "B19013_001",
                        college_2016 = "B16010_041",
                        renters_2016 = "B07013_003",
                        total_houses_2016 = "B25034_001",
                        med_value_2016 = "B25107_001",
                        med_rent_2016 = "B25111_001",
                        poverty_2016 = "B17001_002"))
})

# 2010 ACS 
# map_df() does not work for 2012 data for some reason
for (i in 1:length(us)){
  # Pull data for state
  acs_tmp <- get_acs(geography = "tract",
                        survey = "acs5",
                        year = 2010,
                        state = us[i],
                        variables = c(total_pop_2010 = "B01003_001",
                                      white_pop_2010 = "B02001_002",
                                      black_pop_2010 = "B02001_003",
                                      asian_pop_2010 = "B02001_005",
                                      hispanic_pop_2010 = "B03003_003",
                                      med_income_2010 = "B19013_001",
                                      college_2010 = "B16010_041",
                                      renters_2010 = "B07013_003",
                                      total_houses_2010 = "B25034_001",
                                      med_value_2010 = "B25107_001",
                                      med_rent_2010 = "B25111_001",
                                      poverty_2010 = "B17001_002"))
  
  # Assign dataframe as state variable name
  assign(us[i], acs_tmp)
}

# Row bind all the 2010 ACS data together
us_list <- list()

for (i in 1:length(us)){
  temp_df <- get(us[i])
  us_list[[i]] <- temp_df
}

acs_2010_raw <- do.call(rbind, us_list)

# write.csv(acs_2016_raw, "acs_2016_raw.csv", row.names = FALSE)
# write.csv(acs_2010_raw, "acs_2010_raw.csv", row.names = FALSE)

# Clean and reshape data for use
acs_2016_clean <- acs_2016_raw %>%
  select(-c(NAME, moe)) %>%
  spread(variable, estimate)

acs_2010_clean <- acs_2010_raw %>%
  select(-c(NAME, moe)) %>%
  spread(variable, estimate)

write.csv(acs_2016_clean, "acs_2016_clean.csv", row.names = FALSE)
write.csv(acs_2010_clean, "acs_2010_clean.csv", row.names = FALSE)

