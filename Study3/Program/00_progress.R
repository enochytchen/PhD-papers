## File name: 00_progress.R 
## Updated: 2024-06-18 Enoch Chen: capture progression to BP/CP from 
##                                 SweCan CML and SweCan
## Notes:

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}
setwd(wd)

###################
## Read packages ##
###################
library(dplyr)
library(haven)
library(tidyr)

#################
## Import data ##
#################
cmlreg <- read_dta("/Users/yitche/SweCan_CML/Data/Clean_data/cmlreg.dta")

# Capture AP/BP from cmlreg
df <- cmlreg %>%
      arrange(idnr) %>%
      filter(!is.na(date_ap) | !is.na(date_bp) | dstage == 2 | dstage == 3 | fu_stage == 2 | fu_stage == 3 | ap == 1 | bp == 1)

# Detailed info of all the patients with AP/BP
print(select(df, idnr, date_dx, date_assessment, date_report, ap, date_ap, bp, date_bp, dstage, fu_stage))

# Update dates based on conditions
df <- df %>%
  mutate(date_ap = if_else(dstage == 2, date_dx, date_ap),
         date_bp = if_else(dstage == 3, date_dx, date_bp),
         date_ap = if_else(fu_stage == 2, pmin(date_assessment, date_report, na.rm = TRUE), date_ap),
         date_bp = if_else(fu_stage == 3, pmin(date_assessment, date_report, na.rm = TRUE), date_bp))

# Update ap/bp flags
df <- df %>%
  mutate(ap = if_else(!is.na(date_ap), 1, ap),
         bp = if_else(!is.na(date_bp), 1, bp))

# List updated columns
print(select(df, idnr, date_dx, date_assessment, date_report, ap, date_ap, bp, date_bp, dstage, fu_stage))

# Flag progress
df <- df %>%
  mutate(progress = if_else(ap == 1 | bp == 1, 1, NA_integer_),
         date_prog = pmin(date_ap, date_bp, na.rm = TRUE),
         date_prog = ifelse(is.na(date_prog) & !is.na(date_dx) & dstage != 1, date_dx, date_prog))

# Format date_prog
df$date_prog <- as.Date(df$date_prog)

# List with progress flag
temp <- subset(df, idnr == 16187)
print(select(temp, idnr, date_dx, date_assessment, date_report, ap, date_ap, bp, date_bp, dstage, fu_stage, progress, date_prog))

# Drop duplicates
df2 <- df %>%
       select(idnr, date_prog) %>%
       arrange(idnr, date_prog) %>%
       distinct(idnr, .keep_all = TRUE)

# Merge with SweCan data
# Load SweCan data
swecan_df <- readRDS("../Data/00_cancer_dx_BPCML.rds")

# Merge the dataframes
df3 <- bind_rows(df2, swecan_df)

# Update bp and date_bp based on SweCan data
df3 <- df3 %>%
       mutate(date_prog = pmin(date_prog, diagdate, na.rm = TRUE))

# Convert progress to string
df3 <- df3 %>%
      mutate(progress = ifelse(!is.na(date_prog), "Progress to AP/BP", NA))

# Keep relevant columns and sort by idnr
df3 <- df3 %>%
       select(idnr, progress, date_prog) %>%
       distinct(idnr, progress, date_prog) %>%
       arrange(idnr)

saveRDS(df3, "../Data/00_progress.rds")
###########################################################################