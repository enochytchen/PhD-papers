## File name: Cost_00_death.R 
## Updated: 2024-06-18 Enoch Chen: capture death from Cause of Death register
## Project: CML cost  
## Notes:

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}
setwd(wd)

## Set decimals 
options("scipen"=100, "digits"=1)

###################
## Read packages ##
###################
library(haven)
library(dplyr)
library(lubridate)
library(readr)

#################
## Import data ##
#################
setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")

df <- read_sas("r_dors_kml_11731_2018.sas7bdat")

# Rename column
df <- df %>% 
      rename(idnr = LopNr) %>%
      mutate(recordnr = row_number())

# Convert autopsy column to numeric
df <- df %>% mutate(autopsy = as.numeric(DBGRUND1))


# Generate deathdate components
df <- df %>% mutate(
  deathdate = DODSDAT,
  deathdate_yyyy = substr(deathdate, 1, 4),
  deathdate_mm = substr(deathdate, 5, 2),
  deathdate_dd = substr(deathdate, 7, 2)
)

# Handle missing month and date (mmdd = 0000)
df <- df %>% mutate(
  deathdate_dd = if_else(deathdate_mm == "00" & deathdate_dd == "00", "01", deathdate_dd),
  deathdate_mm = if_else(deathdate_mm == "00" & deathdate_dd == "01", "07", deathdate_mm),
  deathdate_dd = if_else(deathdate_dd == "00", "15", deathdate_dd)
)

# Convert deathdate components to numeric
df <- df %>% mutate(
  deathdate_yyyy = as.numeric(deathdate_yyyy),
  deathdate_mm = as.numeric(deathdate_mm),
  deathdate_dd = as.numeric(deathdate_dd)
)

# Reassemble deathdate
df <- df %>% mutate(deathdate = make_date(deathdate_yyyy, deathdate_mm, deathdate_dd))

# Drop original deathdate variable
df <- df %>% select(-DODSDAT)

# Sort by idnr
df <- df %>% arrange(idnr)

# Flag death
Death <- df %>% mutate(death = ifelse(!is.na(deathdate), 1, 0))

# Save data
setwd(wd)
saveRDS(Death, "../Data/Cost_00_death.rds")


