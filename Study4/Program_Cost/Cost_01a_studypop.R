## File name: Cost_01a_studypop.R
## Updated: 2024-11-21 Enoch Chen: re-organize the codes to comply with the analysis plan
##          2024-06-19 Enoch Chen: create study population by linking all the registers
## Project: CML cost  

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}

setwd(wd)

###################
## Read packages ##
###################
library(haven)
library(dplyr)
library(lubridate)
library(tidyr)

##############################
## Baseline characteristics ##
##############################
# Stage at diagnosis
SweCanCML_dxstage <- readRDS("../Data/SweCanCML_dxstage_raw.rds")

# Extract stage information
SweCanCML_dxstage <- SweCanCML_dxstage %>%
                     arrange(idnr, date_report) %>%
                     group_by(idnr) %>%
                     slice(1) %>%
                     ungroup() %>%
                     select(idnr, dstage)

# CML study base
setwd("/Users/yitche/cmlbase")

cml <- read_sas("studybase_cases.sas7bdat")
nrow(cml)
colnames(cml)

# Keep essential variables only and rename them
varlist <- c("idnr", "sex", "startdat", "startage", "emigrationdat", "deathdat")
cml2 <- cml[, varlist]
colnames(cml2) <- c("idnr", "sex", "date_dx", "dxage", "date_emigration", "date_death")

# Check the maximum and minimum diagnosis date
min(cml2$date_dx)
max(cml2$date_dx)

# Add a date_study_exit to indicate the last day of follow-up or the day of death or emigration
cml2 <- cml2 %>%
        mutate(date_study_exit = as.Date("2018-12-31")) %>%
        mutate(date_exit = pmin(date_death, date_emigration, date_study_exit, na.rm = TRUE))

## Merge with stage information
cml3 <- left_join(cml2, SweCanCML_dxstage, by = "idnr")

## Only patients who diagnosed after 2007 (Prescribed drug register was complete)
min(cml3$date_dx)
cmlbase <- subset(cml3, date_dx >= as.Date("2007-01-01"))

#################################################
## Import all the different procedures by date ##
## TKI, progression, TFR, alloSCT              ##
#################################################
##############
## TKI data ##
##############
setwd(wd)
tki <- readRDS(file = "../Data/Cost_00_tki.rds")
str(tki)
nrow(tki)

## TKI Data from Swedish CML register
## A few idnr did not have complete TKI records from Prescribed drug register but Swedish CML register
tki_cmlreg <- data.frame(c(8456, 22424, 22517, 31405, 42886, 78824, 88908, 95960, 97579))
colnames(tki_cmlreg) <- "idnr"
tki_cmlreg <- tki_cmlreg %>%
              mutate(tki = c("DAS", "DAS", "NIL", "NIL", "NIL", "DAS", "NIL", "NIL", "NIL"),
                     date_tki = c("29mar2011", "22nov2010", "03apr2008", "20apr2009", 
                                  "06may2009", "22mar2010", "07mar2012", "27aug2008", "22sep2009"),
                     date_tki = as.Date(date_tki, "%d%b%Y"))
tki <- tki %>%
       rows_append(tki_cmlreg) %>%
       arrange(idnr, date_tki)

## Process tki data
tki <- tki %>%
       rename(item = tki, 
              date_item = date_tki)%>%
       #sort by idnr and date_item
       arrange(idnr, date_item)
head(tki)

## Mark which line of TKI 
## I want the first kind of item, e.g. IMA, to be called 1L; second kind called 2L; third or forth call 3L+. Name it a new column called line
tki <-  tki %>%
        group_by(idnr) %>%
        mutate(
          line = cumsum(item != lag(item, default = first(item))) + 1,
          line = case_when(
            line == 1 ~ "1L",
            line == 2 ~ "2L",
            line > 2  ~ "3L+"
          )
        ) %>%
        ungroup()

######################
## Progression data ##
######################
progress <- readRDS(file = "../Data/Cost_00_progress.rds")
str(progress)

date_progress <- progress %>%
                 arrange(idnr, date_prog) %>%
                 distinct(idnr, .keep_all = TRUE) %>%
                 select(idnr, date_prog)

progress <- progress %>%
            rename(item = progress, 
                   date_item = date_prog) 
str(progress)
head(progress)

##############
## SCT data ##
##############
sct <- readRDS(file = "../Data/Cost_00_sct.rds")
str(sct)

date_sct <- sct %>%
            group_by(idnr) %>%
            select(idnr, date_sct)

sct <- sct %>%
       rename(item = sct, 
              date_item = date_sct)
str(sct)
head(sct)

##############
## TFR data ##
##############
tfr <- readRDS(file = "../Data/Cost_00_tfr.rds")
str(tfr)

date_tfr_start <- tfr%>%
                  group_by(idnr) %>%
                  select(idnr, date_tfr_start)
  
tfr <- tfr %>%
       rename(item = tfr_start, 
              date_item = date_tfr_start) %>%
       select(idnr, item, date_item)
str(tfr)
head(tfr)

## End of study
## Make death, emigration or end of study a procedure to calculate length of stay
end <- cml3[, c("idnr", "date_death", "date_emigration", "date_study_exit")]
end <- end %>%
       mutate(date_item = pmin(date_death, date_emigration, date_study_exit, na.rm = TRUE)) %>%
       select(idnr, date_item) %>%
       mutate(item = "End") %>% 
       arrange(idnr, item, date_item)

head(end)       

######################
### Merge all data ###
######################
df <- tki %>%
      rows_append(progress) %>%
      rows_append(tfr) %>%
      rows_append(sct) %>%
      rows_append(end) %>%
      arrange(idnr, date_item)

head(df)

table(df$item, useNA = "ifany")

# Prescribed Drug register is only complete during this period
df <- subset(df, date_item >= as.Date("2007-01-01") & date_item <= as.Date("2018-12-31")) %>%
      ## Set cost = 0 if NA
      mutate(cost_lan = ifelse(is.na(cost_lan), 0, cost_lan)) %>%
      ## Set line = NA 
      mutate(line = ifelse(is.na(line), NA, line)) 

# Save the cleaned study population 
setwd(wd)
saveRDS(df, file = "../Data/Cost_01a_studypop.rds")

