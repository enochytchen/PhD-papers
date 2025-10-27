## File name: 00_tfr.R 
## Updated: 2024-06-18 Enoch Chen: capture TFR from the Swedish CML register
##                                 using codes from the Flygt paper
## Project: CML cost  
## Notes:

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}

setwd(wd)

# Load necessary libraries
library(dplyr)

# Load the data
df <- readRDS("../Data/flygt2021_cmlbase2.rds")

# View df structure
str(df)

# Rename variables
df <- df %>%
  rename(
    idnr = lopnr,
    date_tfr_start = final_tki_utsatt_date,
    tfr_reason = final_tki_utsatt_orsak
  )

# Convert date format (assuming date is in Stata format, might need as.Date conversion)
df$date_tfr_start <- as.Date(df$date_tfr_start, format="%d")

# See how many had records of TKI discontinuation
num_discontinuations <- df %>%
  filter(!is.na(date_tfr_start)) %>%
  nrow()

cat("Number of records with TKI discontinuation: ", num_discontinuations, "\n")

# See the distribution of the TKI discontinuation reason
summary(df$tfr_reason)
table(df$tfr_reason)

# Only keep patients who were reported TKI discontinuation and the reason was DMR
df <- df %>%
  filter(tfr_reason == "Deep molecular response")

# Create TFR start indicator
df <- df %>%
  mutate(tfr_start = if_else(!is.na(date_tfr_start), "TFR starts", NA_character_))

# Rename variables for clarity
attr(df$date_tfr_start, "label") <- "Date of TFR start"
attr(df$tfr_start, "label") <- "Flag of TFR start"

# Order variables
df <- df %>%
  select(idnr, tfr_start, date_tfr_start, tfr_reason)

# Sort df
df <- df %>%
  arrange(idnr, date_tfr_start)

# Manually debug: remove patient with specific idnr
df <- df %>%
  filter(!(idnr == 71080))

# Save the cleaned df
setwd(wd)
saveRDS(df, file = "../Data/00_tfr.rds")
