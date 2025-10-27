## File name: Cost_00_sct.R 
## Updated: 2024-06-18 Enoch Chen: capture alloSCT from patient register
## Project: CML cost  
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
setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")
patient_reg <- read_sas("r_par_sv_kml_11731_2018.sas7bdat")

# Generate record number by order
df <- patient_reg %>%
      mutate(recordnr = row_number())

# Rename LopNr to idnr
df <- df %>%
      rename(idnr = LopNr)

# Keep specific columns
df <- df %>%
      select(idnr, recordnr, OP, starts_with("OPD"))

# Surgery procedure: Split the OP variable into multiple OP* variables
for (i in 1:30) {
  position <- 1 + 6 * (i - 1)
  df <- df %>%
    mutate(!!paste0("OP", i) := substr(OP, position, position + 4))
}

# Drop the original OP column
df <- df %>%
      select(-OP)

# Rename date variable into date_OP
names(df) <- gsub("^OPD", "date_OP", names(df))

# Transpose the data
# Make a sequential list of procedures per patient
df_long <- df %>%
  pivot_longer(cols = starts_with("OP"), names_to = "nr", values_to = "procedure") %>%
  mutate(nr = as.integer(gsub("OP", "", nr))) %>%
  filter(procedure != "")
head(df_long)

# Make the procedure date correspond to procedure
df_long$date_procedure = NA

# Create the dynamic case_when statement
conditions <- paste0("nr == ", 1:30, " ~ date_OP", 1:30, collapse = ", ")

df_long <- df_long %>%
          mutate(date_procedure = eval(parse(text = paste0("case_when(", conditions, ", TRUE ~ as.Date(NA))"))))

head(df_long)

# Select columns
df_long2 <- df_long[, c("idnr", "procedure", "date_procedure")] %>%
            arrange(idnr, date_procedure) 
surgery <- subset(df_long2, !is.na(date_procedure))

###################################
## Capture those who had AlloSCT ##
###################################
sct <- surgery %>%
       filter(procedure %in% c("DR041", "DR042", "DR044", "DR045", "DR046", "DR047"))

# Sort by idnr and date_procedure
sct <- sct %>%
       arrange(idnr, date_procedure)

# Only keep the first code per idnr
sct <- sct %>%
       distinct(idnr, .keep_all = TRUE)

# Generate flag for SCT
sct <- sct %>%
       mutate(sct = "SCT", 
              date_sct = date_procedure)

# Keep variables
sct_final <- sct %>%
             select(idnr, sct, date_sct)

# Save data
setwd(wd)
saveRDS(sct_final, "../Data/Cost_00_sct.rds")
