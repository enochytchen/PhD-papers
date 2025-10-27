## File name: Cost_00_cancer_dx.R 
## Updated: 2024-06-18 Enoch Chen: capture diagnosis from Swedish Cancer Registry
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

#################
## Import data ##
#################
setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")
SweCan <- read_sas("R_can_kml_11731_2018.sas7bdat")

# Rename variables with labels as comments
df <- SweCan %>%
      rename(
        idnr = LopNr,           # "ID number"
        year = AR,              # "year at diagnosis"
        region = LKF,           # "lan kommun (region)"
        tumornr = tnr,          # "Tumor number"
        hospital = SJUKHUS,     # "hospital"
        cause = ulorsak,        # "cause of death"
        age = ALDER,            # "age at diagnosis"
        sex = KON,              # "sex"
        autopsy = OBD1,         # "Autopsy finding"
        ben = BEN,              # "Benign"
        pad = PAD,              # "C24/hist"
        diagdate = DIADAT       # "Date of diagnosis"
      )

# Handle sex variable
df <- df %>%
      mutate(
        sex = as.numeric(sex),
        sex = recode(sex, `1` = "male", `2` = "female"),
        male = ifelse(sex == "male", 1, 0)
      )

# Handle autopsy variable
df <- df %>%
  mutate(
    autopsy = as.numeric(autopsy),
    autopsy = recode(autopsy, `1` = "Unexpected", `2` = "Expected", `3` = "?")
  )

# Handle ben variable
df <- df %>%
  mutate(
    ben = as.numeric(ben),
    ben = recode(ben, `3` = "benign")
  )

# Process diagdate
df <- df %>%
  mutate(
    diagdate_yyyy = substr(diagdate, 1, 4),
    diagdate_mmdd = substr(diagdate, 5, 8),
    diagdate_mmdd = ifelse(diagdate_mmdd == "0229", "0301", diagdate_mmdd),
    diagdate_mm = substr(diagdate_mmdd, 1, 2),
    diagdate_dd = substr(diagdate_mmdd, 3, 4),
    diagdate_dd = ifelse(diagdate_dd == "", "15", diagdate_dd)
  ) %>%
  mutate_at(vars(diagdate_yyyy, diagdate_mm, diagdate_dd), as.numeric) %>%
  mutate(
    diagdate = as.Date(paste(diagdate_yyyy, diagdate_mm, diagdate_dd, sep = "-"), format = "%Y-%m-%d")
  ) %>%
  select(-diagdate_yyyy, -diagdate_mmdd, -diagdate_mm, -diagdate_dd)

# Drop unnecessary variables
df <- df %>%
      select(-Fall, -Kontroll, -region, -hospital, -KLINIK, -SIDA, starts_with("SNO"), -PAT)

# Sort data
SweCan_final <- df %>%
                arrange(idnr, diagdate)

# Colnames
colnames(SweCan_final)
nrow(SweCan_final)

# Save data
setwd(wd)
saveRDS(SweCan_final, "../Data/Cost_00_cancer_dx.rds")
################################################################################

