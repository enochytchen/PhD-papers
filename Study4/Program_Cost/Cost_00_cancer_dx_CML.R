## File name: Cost_00_cancer_dx_CML.R 
## Updated: 2024-06-18 Enoch Chen: capture CML diagnosis from Swedish Cancer Registry
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

#################
## Import data ##
#################
cancer_dx <- readRDS("../Data/Cost_00_cancer_dx.rds")

###################################
## CML ICD7= 2051; ICDO10 = C921 ##
###################################
table(cancer_dx$ICDO10)
table(cancer_dx$ICD7)

## Capture CML patients based on ICDO10 
cancer_dx_CML <- subset(cancer_dx, ICDO10 == "C921")
nrow(cancer_dx_CML)

## Check any duplicates
duplicates <- cancer_dx_CML %>%
              group_by(idnr) %>%
              filter(n() > 1) %>%
              ungroup()

# Save data
setwd(wd)
saveRDS(cancer_dx_CML, "../Data/Cost_00_cancer_dx_CML.rds")
###################################################