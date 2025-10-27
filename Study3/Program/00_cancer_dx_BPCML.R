## File name: 00_cancer_dx_BPCML.R 
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
cancer_dx <- readRDS("../Data/00_cancer_dx.rds")

################################################################
## Define blastic phase as patients diagnosed with ALL or AML ##
################################################################
table(df2$ICDO10)
table(df2$ICD7)

## Capture blastic phase patients based on ICDO10
## AML: ICDO10 == "C920"
## ALL: ICDO10 == "C910"
cancer_dx_BPCML <- subset(cancer_dx, ICDO10 == "C920" | ICDO10 == "C910")
nrow(cancer_dx_BPCML)

# Save data
setwd(wd)
saveRDS(cancer_dx_BPCML, "../Data/00_cancer_dx_BPCML.rds")
###################################################