## File name: 01b_studypop_multistate.R
## Updated: 2024-08-14 Enoch Chen: update data cleaning using R 
##          2024-06-19 Enoch Chen: Make the CML data into a multistate structure
## Project: CML QALY  

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
}

setwd(wd)

###################
## Read packages ##
###################
library(haven)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)

###########################
## Read study population ##
###########################
studypop1 <- readRDS(file = "../Data/01a_studypop.rds")
str(studypop1)
nrow(distinct(studypop1, idnr))

# Make data into two subsets to identify records before/after TFR
###########################
# Whoever had TFR records #
###########################

studypop1_withTFR <- studypop1%>%
                     arrange(idnr, date_item)%>%
                     group_by(idnr, item)%>%
                     filter(!is.na(date_tfr_start))%>%
                     filter(item %in% c("BOS", "DAS", "IMA", "NIL", "PON"))%>%
                     ungroup()
nrow(distinct(studypop1_withTFR, idnr))

# Take the earliest record for each item before TFR
studypop1_withTFR_beforeTFR <- studypop1_withTFR %>%
                               filter(date_item < date_tfr_start) %>%
                               group_by(idnr, item)%>%
                               filter(date_item == min(date_item))%>%
                               mutate(afterTFR = 0) %>%
                               ungroup()

# Take the earliest record for each item after TFR
studypop1_withTFR_afterTFR <- studypop1_withTFR%>%
                              filter(date_item > date_tfr_start)%>%
                              group_by(idnr, item)%>%
                              filter(date_item == min(date_item))%>%
                              mutate(afterTFR = 1) %>%
                              ungroup()

# Append both sub-populations above together
studypop1_withTFR <- studypop1_withTFR_beforeTFR %>%
                     rows_append(studypop1_withTFR_afterTFR) %>%
                     arrange(idnr, date_item) %>%
                     group_by(idnr) %>%
                     mutate(order = as.numeric(row_number())) %>%
                     mutate(order = case_when(
                            # If it's TKI after TFR and same as previous TKI, then same line
                            afterTFR == 1 & item == lag(item) ~ as.character(lag(order)),
                            TRUE ~ as.character(order))) %>%
                     mutate(order = ifelse(afterTFR == 1, paste0(order, "_", sep ="" ), order)) %>%
                     dplyr::select(-afterTFR)

# Create variables based on order
studypop1_withTFR <- studypop1_withTFR %>%
                     mutate(
                        date_tki1 = case_when(order == "1" ~ date_item, TRUE ~ NA_Date_),
                        date_tki1_ = case_when(order == "1_" ~ date_item, TRUE ~ NA_Date_),
                        date_tki2 = case_when(order == "2" ~ date_item, TRUE ~ NA_Date_),
                        date_tki2_ = case_when(order == "2_" ~ date_item, TRUE ~ NA_Date_),
                        date_tki3 = case_when(order == "3" ~ date_item, TRUE ~ NA_Date_),
                        date_tki3_ = case_when(order == "3_" ~ date_item, TRUE ~ NA_Date_),
                        tki1 = case_when(order == "1" ~ item, TRUE ~ NA_character_),
                        tki1_ = case_when(order == "1_" ~ item, TRUE ~ NA_character_),
                        tki2 = case_when(order == "2" ~ item, TRUE ~ NA_character_),
                        tki2_ = case_when(order == "2_" ~ item, TRUE ~ NA_character_),
                        tki3 = case_when(order == "3" ~ item, TRUE ~ NA_character_),
                        tki3_ = case_when(order == "3_" ~ item, TRUE ~ NA_character_))

####################################
# Whoever did not have TFR records #
####################################
studypop1_woTFR <- studypop1%>%
                   arrange(idnr, date_item)%>%
                   group_by(idnr, item)%>%
                   filter(is.na(date_tfr_start))%>%
                   filter(item %in% c("BOS", "DAS", "IMA", "NIL", "PON"))%>%
                   filter(date_item == min(date_item))%>%
                   ungroup()
                   
nrow(distinct(studypop1_woTFR, idnr))

# Create order by row number for defining lines of TKI
studypop1_woTFR <- studypop1_woTFR%>%
                   group_by(idnr) %>%
                   mutate(order = row_number())%>%
                   ungroup()

studypop1_woTFR <- studypop1_woTFR%>%
                   mutate(date_tki1 = as.Date(ifelse(order == 1, date_item, NA)),
                          date_tki2 = as.Date(ifelse(order == 2, date_item, NA)),
                          date_tki3 = as.Date(ifelse(order == 3, date_item, NA)),
                          tki1 = case_when(order == 1 ~ item, TRUE ~ NA_character_),
                          tki2 = case_when(order == 2 ~ item, TRUE ~ NA_character_),
                          tki3 = case_when(order == 3 ~ item, TRUE ~ NA_character_))%>%
                   filter(order<=3)%>% # 3L+ TKI are all in the same state
                   dplyr::select(-order)
nrow(distinct(studypop1_woTFR, idnr))

################################
## Combine two subpopulations ##
################################
studypop1_combined <- studypop1_withTFR %>%
                      rows_append(studypop1_woTFR)

# Some patients have more than one row
nrow(studypop1_combined)
nrow(distinct(studypop1_combined, idnr))

# Reduce to one row per patient
# Define function 
comb <- function(x) {
  toString(unique(x[!is.na(x)]))
}

studypop1_combined <- studypop1_combined %>%
                      group_by(idnr) %>%
                      summarise(
                        dxage = first(dxage),
                        dxsokal = first(dxsokal), 
                        dxeutos = first(dxeutos),
                        date_dx = first(date_dx),
                        sex = first(sex),
                        date_prog = first(date_prog),
                        date_sct = first(date_sct),
                        date_tfr_start = first(date_tfr_start),
                        tfr_end = first(tfr_end),
                        date_death = first(date_death),
                        date_emigration = first(date_emigration),
                        date_study_exit = first(date_study_exit),
                        date_tki1 = comb(date_tki1),
                        date_tki1_ = comb(date_tki1_),
                        date_tki2 = comb(date_tki2),
                        date_tki2_ = comb(date_tki2_),
                        date_tki3 = comb(date_tki3),
                        date_tki3_ = comb(date_tki3_),
                        tki1 = comb(tki1),
                        tki1_ = comb(tki1_),
                        tki2 = comb(tki2),
                        tki2_ = comb(tki2_),
                        tki3 = comb(tki3),
                        tki3_ = comb(tki3_))%>%
                        # Make date variables as date
                        mutate( date_tki1 = as.Date(date_tki1),
                                date_tki1_ = as.Date(date_tki1_),
                                date_tki2 = as.Date(date_tki2),
                                date_tki2_ = as.Date(date_tki2_),
                                date_tki3 = as.Date(date_tki3),
                                date_tki3_ = as.Date(date_tki3_))

# Now each patient only has one record
nrow(studypop1_combined)
nrow(distinct(studypop1_combined, idnr))

# Pick one patient for double-check
print(subset(studypop1, idnr == 11))
print(subset(studypop1_combined, idnr == 11))

##################################################################
## Create date_tfr1 to date_tfr3 to differentiate TFR after TKI ##
##################################################################
studypop_tfr <- studypop1 %>%
                # Reduce to obtain order
                group_by(idnr, item) %>%
                filter(date_item == min(date_item)) %>%
                group_by(idnr) %>%
                mutate(order = as.numeric(row_number())) %>%
                ungroup() %>%
                arrange(idnr, date_item) %>%
                mutate(date_tfr1 = ifelse(item == "TFR starts" & order == 2, date_tfr_start, NA),
                       date_tfr2 = ifelse(item == "TFR starts" & order == 3, date_tfr_start, NA),
                       date_tfr3 = ifelse(item == "TFR starts" & order == 4, date_tfr_start, NA)) %>%
                mutate(date_tfr1 = as.Date(date_tfr1),
                       date_tfr2 = as.Date(date_tfr2),
                       date_tfr3 = as.Date(date_tfr3))%>%
                group_by(idnr) %>%
                filter(item == "TFR starts" & (any(!is.na(date_tfr1) | !is.na(date_tfr2) | !is.na(date_tfr3)))) %>%
                ungroup()%>%
                dplyr::select(idnr, date_tfr1, date_tfr2, date_tfr3)
nrow(distinct(studypop_tfr, idnr))

###########
## Merge ##
###########
studypop2_combined <- left_join(studypop1_combined, studypop_tfr, by = "idnr")
nrow(distinct(studypop2_combined))

# Save the cleaned study population 
setwd(wd)
saveRDS(studypop2_combined, file = "../Data/01b_studypop_multistate.rds")
