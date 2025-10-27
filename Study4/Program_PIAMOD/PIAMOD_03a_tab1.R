## File name: PIAMOD_03a_tab1
## Updated: 2025-03-17 Enoch Chen: Incidence functions are the same. Just two scenarios (fixed vs. increasing relative survival)
##          2025-02-03 Enoch Chen: Make table 1

## Project: CML Cost
## Purpose: Make Table 1 for all four scenarios

# Read packages
library(flextable)
library(dplyr)

# Read data
data1_constantRS_dynINC <- read.csv("../Data/PIAMOD_03_piamod_constantRS_dynINC.csv") |>
                           mutate(Scenario = "Scenario 1. Fixed relative survival")

data2_dynRS_dynINC <- read.csv("../Data/PIAMOD_03_piamod_dynRS_dynINC.csv") |>
                       mutate(Scenario = "Scenario 2. Dynamic relative survival")

# Rbind all data
data_all <- rbind(data1_constantRS_dynINC, data2_dynRS_dynINC)

##############################

# Keep relevant variables
# MEANPOP 
# "STD.INC.lb", "STD.INC.ub", "STD.CASES", "STD.CASES.lb", "STD.CASES.ub"
# "STD.PREVAL.lb", "STD.PREVAL.ub", "STD.PREV.CASES", 
# "STD.PREV.CASES.lb", "STD.PREV.CASES.ub"

var_list <- c("Scenario", "SEX", "AGE_GROUP" ,"YEAR", "MEANPOP", 
              "EXP.INCID.", "EXP.INCID.lb", "EXP.INCID.ub",
              "EXP.CASES", "EXP.CASES.lb", "EXP.CASES.ub",
              "EXP.PREVAL.", "EXP.PREVAL.lb", "EXP.PREVAL.ub",
              "PREV.CASES", "PREV.CASES.lb", "PREV.CASES.ub",
              "STD.INC.", "STD.INC.lb", "STD.INC.ub",
              "STD.PREV.", "STD.PREV.lb", "STD.PREV.ub")

data_all <- data_all[, var_list]

# Keep only SEX == 3 and YEAR == 2015 2020 2025 2030
data_all2 <-  data_all |>
              filter(AGE_GROUP == "0-99") |>
              filter(SEX == 3 & YEAR %in% c(2015, 2020, 2025, 2030)) |>
              mutate(YEAR = as.character(YEAR),
                     MEANPOP = round(MEANPOP / 1000, 0),
                     EXP.INCID. = round(EXP.INCID., 2),
                     EXP.INCID.lb = round(EXP.INCID.lb, 2),
                     EXP.INCID.ub = round(EXP.INCID.ub, 2),
                     EXP.CASES = round(EXP.CASES, 1),
                     EXP.CASES.lb = round(EXP.CASES.lb, 1),
                     EXP.CASES.ub = round(EXP.CASES.ub, 1),
                     EXP.PREVAL. = round(EXP.PREVAL., 2),
                     EXP.PREVAL.lb = round(EXP.PREVAL.lb, 2),
                     EXP.PREVAL.ub = round(EXP.PREVAL.ub, 2),
                     PREV.CASES = round(PREV.CASES, 1),
                     PREV.CASES.lb = round(PREV.CASES.lb, 1),
                     PREV.CASES.ub = round(PREV.CASES.ub, 1),
                     STD.INC. = round(STD.INC., 2),
                     STD.INC.lb = round(STD.INC.lb, 2),
                     STD.INC.ub = round(STD.INC.ub, 2),
                     STD.PREV. = round(STD.PREV., 2),
                     STD.PREV.lb = round(STD.PREV.lb, 2),
                     STD.PREV.ub = round(STD.PREV.ub, 2)
                     ) |>
                     select(-SEX, -AGE_GROUP)

head(data_all2)

# fttable
data_all3 <- data_all2 %>%
              mutate(
                `Incident cases` = 
                  sprintf("%.1f (%.1f-%.1f)", EXP.CASES, EXP.CASES.lb, EXP.CASES.ub),
                `Incidence per 100 000 person-years` = 
                  sprintf("%.2f (%.2f-%.2f)", EXP.INCID., EXP.INCID.lb, EXP.INCID.ub),
                `Age-standardized Incidence per 100 000 person-years` = 
                  sprintf("%.2f (%.2f-%.2f)", STD.INC., STD.INC.lb, STD.INC.ub),
                `Prevalent cases` = 
                  sprintf("%.1f (%.1f-%.1f)", PREV.CASES, PREV.CASES.lb, PREV.CASES.ub),
                `Prevalence per 100 000 persons` = 
                  sprintf("%.2f (%.2f-%.2f)", EXP.PREVAL., EXP.PREVAL.lb, EXP.PREVAL.ub),
                `Age-standardized Prevalence per 100 000 persons` = 
                  sprintf("%.2f (%.2f-%.2f)", STD.PREV., STD.PREV.lb, STD.PREV.ub)
              ) %>%
              select(Scenario, YEAR, MEANPOP,
                     `Incident cases`,
                     `Incidence per 100 000 person-years`,
                     `Age-standardized Incidence per 100 000 person-years`,
                     `Prevalent cases`,
                     `Prevalence per 100 000 persons`,
                     `Age-standardized Prevalence per 100 000 persons`) %>%
              rename(
                "Year" = YEAR,
                "Population in thousands" = MEANPOP
              ) %>%
              mutate(Scenario = ifelse(Year != "2015", "", Scenario))

# Create flextable
ft <- flextable(data_all3) %>%
      theme_vanilla() %>%
      autofit() 
ft

# Write to csv
timestamp <- format(Sys.time(), "%Y%m%d")
save_as_docx(ft, path=paste0("../Output/PIAMOD_03a_tab1_", timestamp, ".docx"))
