# Load libraries
library(dplyr)
library(lubridate)
library(data.table)

# Read in permit data ----

# Name permit columns
permit_names <- c("account", "permit_id", "agency_id", "permit_status",
                  "description", "state_class", "permit_type", "permit_type_dscr",
                  "property_type", "issue_date", "year", "site_number", "site_pfx",
                  "site_street_name", "site_tp", "site_sfx", "site_apt")

permits <- fread("data/permits.txt", header = FALSE, stringsAsFactors = FALSE, 
                 col.names = permit_names, quote = "", data.table = FALSE)

colnames(permits) <- permit_names

# Convert date column
permits$issue_date <- as.character(permits$issue_date)
permits$issue_date[permits$issue_date == ""] <- NA
permits$issue_date <- gsub("^[[:alpha:]]+$", NA, permits$issue_date)
permits$issue_date <- as.Date(as.character(permits$issue_date), format = "%m/%d%/%Y")

# Remove non UTF-8 characters 
permits$description <- iconv(permits$description, "UTF-8", "UTF-8", sub = "")

# Write cleaned permits to csv 
write.csv(permits, "data/permits.csv", row.names = FALSE)

