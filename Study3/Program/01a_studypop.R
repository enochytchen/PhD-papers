## File name: 01a_studypop.R
## Updated: 2024-08-14 Enoch Chen: update data cleaning using R 
##          2024-06-19 Enoch Chen: create study population by linking all the registers
## Project: CML QALY  

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
library(survival)

##############################
## Baseline characteristics ##
##############################
# Read diagnosis stage data from CML register
setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")
SweCanCML <- read_sas("sos_kml1_au_21nov19.sas7bdat", encoding="latin1")

SweCanCML <- SweCanCML[, c("LopNr", "u_inrappdtm", "a_sjukstadium", "d0_sokalscoregrp", "d0_eutosscoregrp")] |>
             rename(idnr = LopNr,
                    date_report = u_inrappdtm,
                    dstage = a_sjukstadium, 
                    dxsokal = d0_sokalscoregrp, 
                    dxeutos = d0_eutosscoregrp)
str(SweCanCML)

# Extract stage information, and sokal/eutos group
SweCanCML_dxstage <- SweCanCML %>%
                     arrange(idnr, date_report) %>%
                     group_by(idnr) %>%
                     slice(1) %>%
                     ungroup()

# Read CML study base
## CML study base
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

# Add a date_study_exit to indicate the last day of follow-up
# Add a date_exit to indicate the individaul left
cml2 <- cml2 %>%
        mutate(date_study_exit = as.Date("2018-12-31")) %>%
        mutate(date_exit = pmin(date_death, date_emigration, date_study_exit, na.rm = TRUE))

## Merge with stage information
cml3 <- left_join(cml2, SweCanCML_dxstage, by = "idnr")

## Only patients who diagnosed after 2007 (Prescribed drug register was complete)
## Only chronic phase
## Only over 18
min(cml3$date_dx)
cmlbase <- subset(cml3, date_dx >= as.Date("2007-01-01") & dstage == 1 & dxage >= 18)
table(cmlbase$dstage)

#################################################
## Import all the different procedures by date ##
## TKI, progression, TFR, alloSCT              ##
#################################################

## TKI data 
setwd(wd)
tki <- readRDS(file = "../Data/00_tki.rds")
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
       arrange(idnr, date_tki) %>%
       rename(item = tki, 
              date_item = date_tki) %>%
       select(idnr, item, date_item)

# Progression data
progress <- readRDS(file = "../Data/00_progress.rds")
str(progress)

date_progress <- progress %>%
                 arrange(idnr, date_prog) %>%
                 distinct(idnr, .keep_all = TRUE) %>%
                 dplyr::select(idnr, date_prog)

progress <- progress %>%
            rename(item = progress, 
                   date_item = date_prog) %>%
            arrange(idnr, date_item) %>%
            distinct(idnr, .keep_all = TRUE)
str(progress)


# SCT data
sct <- readRDS(file = "../Data/00_sct.rds")
str(sct)


date_sct <- sct %>%
            group_by(idnr) %>%
            dplyr::select(idnr, date_sct)

sct <- sct %>%
       rename(item = sct, 
              date_item = date_sct)
str(sct)

# TFR data
tfr <- readRDS(file = "../Data/00_tfr.rds")
str(tfr)

date_tfr_start <- tfr%>%
                  group_by(idnr) %>%
                  dplyr::select(idnr, date_tfr_start)
  
tfr <- tfr %>%
       rename(item = tfr_start, 
              date_item = date_tfr_start)%>%
       dplyr::select(idnr, item, date_item)
str(tfr)

################################################################
## Merge the baseline characteristics with all the procedures ##
################################################################

df <- tki %>%
      rows_append(progress) %>%
      rows_append(tfr) %>%
      rows_append(sct) %>%
      arrange(idnr, date_item)

table(df$item, useNA = "ifany")

# Prescribed Drug register is only complete during this period
df <- subset(df, date_item >= as.Date("2007-01-01") & date_item <= as.Date("2018-12-31"))

table(df$item, useNA = "ifany")

## Left join the baseline characteristics (cml3) with all other procedures
cml4 <- left_join(cmlbase, df, by = "idnr") %>% 
        left_join(date_progress, by = "idnr", relationship = "many-to-many")  %>%
        left_join(date_sct, by = "idnr", relationship = "many-to-many") %>%
        left_join(date_tfr_start, by = "idnr", relationship = "many-to-many") %>%
        filter(!is.na(item))

nrow(distinct(cml4, idnr))

## Drop pts who had progression before diagnosis
cml4 <- cml4 %>%
        mutate(diff = as.numeric(difftime(date_prog, date_dx, units = "days")))%>%
        filter(diff>=0 | is.na(diff))%>%
        dplyr::select(-diff)

nrow(distinct(cml4, idnr))

## Those who have date_prog = date_death, add one day to date_death
cml4 <- cml4 %>%
        mutate(date_death = case_when(
                            date_prog == date_death & !is.na(date_prog) ~ date_death + 1,
                            TRUE ~ date_death
        ))

nrow(distinct(cml4, idnr))

cml4 <- cml4 %>%
        # Keep essential variables
        dplyr::select(idnr, sex, dxage, dstage, dxsokal, dxeutos, date_dx, date_death, date_emigration, date_study_exit, date_exit, date_item, item,
                      date_prog, date_sct, date_tfr_start)

#####################################################
## Those records with progression, SCT, TFR starts ##
#####################################################
cml4_subpop <- subset(cml4, item %in% c("Progress to AP/BP", "SCT", "TFR starts"))

## Print indr 3688 (this pt has SCT only)
## This can help me check the data is claimed properly or not
idnr3688 <- subset(cml4_subpop, idnr == 3688) %>%
            dplyr::select(idnr, date_dx, date_death, date_exit, date_item, item, date_prog, date_sct) %>%
            print()

## Print indr 4898 (this pt has TFR starts only)
## This can help me check the data is claimed properly or not
idnr4898 <- subset(cml4_subpop, idnr == 4898) %>%
            dplyr::select(idnr, date_dx, date_death, date_exit, date_item, item, date_prog, date_sct, date_tfr_start) %>%
            print()

##################
## Mark tfr_end ##
##################
cml5 <- cml4 %>%
        arrange(idnr, date_item) %>% # Ensure rows are ordered by date_item
        group_by(idnr) %>%
        mutate(date_last_item = max(date_item, na.rm = TRUE)) %>%
        ungroup() %>%
        # Replace NA values in date_last_item with the last valid date before "End"
        group_by(idnr) %>%
        mutate(tfr_end = ifelse(date_last_item != date_tfr_start & !is.na(date_tfr_start), 1 , 0)) %>%
        ungroup() %>%
        dplyr::select(-date_last_item)

nrow(distinct(cml5, idnr))

# Save the cleaned study population 
setwd(wd)
saveRDS(cml5, file = "../Data/01a_studypop.rds")
 
