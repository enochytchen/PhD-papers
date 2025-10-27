## File name: 01c_studypop_msset.R
## Updated: 2024-07-06 Enoch Chen: set the data into multistate structure
## Project: CML QALY  
## Notes:

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
data <- readRDS(file = "../Data/01b_studypop_multistate.rds")
str(data)

##############################
## Define transition matrix ##
##############################
tmat <- matrix(c(NA, NA, 1, NA, NA, NA, 2, 3, 4, NA, NA, 5,
                 NA, NA, NA, 6, NA, NA, NA, NA, NA, NA, NA, 7,
                 NA, NA, NA, NA, 8, NA, 9, 10, NA, 11, NA, 12,
                 NA, NA, NA, NA, NA, 13, NA, NA, NA, NA, NA, 14,
                 NA, NA, NA, NA, NA, NA, 15, 16, NA, NA, 17, 18,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 19,
                 NA, NA, NA, NA, NA, NA, NA, 20, NA, NA, NA, 21,
                 NA, NA, NA, NA, NA, NA, 22, NA, NA, NA, NA, 23,
                 NA, 24, NA, 25, NA, NA, NA, NA, NA, NA, NA, 26,
                 NA, NA, NA, 27, NA, 28, NA, NA, NA, NA, NA, 29,
                 NA, NA, NA, NA, 30, NA, NA, NA, NA, NA, NA, 31,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
               nrow = 12, byrow = TRUE)

colnames(tmat) <- c("to:tki1", "to:tki1_", "to:tki2", "to:tki2_", "to:tki3", "to:tki3_",
                    "to:prog", "to:sct", "to:tfr1", "to:tfr2", "to:tfr3", "to:death")
rownames(tmat) <- c("to:tki1", "to:tki1_", "to:tki2", "to:tki2_", "to:tki3", "to:tki3_",
                    "to:prog", "to:sct", "to:tfr1", "to:tfr2", "to:tfr3", "to:death")

print(tmat)

# Make 31 duplicates for each patient for defining transitions later
data <- data %>% 
        group_by(idnr) %>% 
        slice(rep(1:n(), each = 31)) %>% 
        ungroup()

# Manually make msset format
data$start <- NA
data$stop <- NA
data$from <- NA
data$to <- NA
data$status <- NA

# Specify _from _to
data$trans <- rep(1:31, nrow(data) / 31)
data$from <- ifelse(data$trans >= 1 & data$trans <= 5, 1, data$from)
data$from <- ifelse(data$trans >= 6 & data$trans <= 7, 2, data$from)
data$from <- ifelse(data$trans >= 8 & data$trans <= 12, 3, data$from)
data$from <- ifelse(data$trans >= 13 & data$trans <= 14, 4, data$from)
data$from <- ifelse(data$trans >= 15 & data$trans <= 18, 5, data$from)
data$from <- ifelse(data$trans >= 19, 6, data$from)
data$from <- ifelse(data$trans >= 20 & data$trans <= 21, 7, data$from)
data$from <- ifelse(data$trans >= 22 & data$trans <= 23, 8, data$from)
data$from <- ifelse(data$trans >= 24 & data$trans <= 26, 9, data$from)
data$from <- ifelse(data$trans >= 27 & data$trans <= 29, 10, data$from)
data$from <- ifelse(data$trans >= 30 & data$trans <= 31, 11, data$from)

data$to <- ifelse(data$trans == 24, 2, data$to)
data$to <- ifelse(data$trans == 1, 3, data$to)
data$to <- ifelse(data$trans == 6 | data$trans == 25 | data$trans == 27, 4, data$to)
data$to <- ifelse(data$trans == 8, 5, data$to)
data$to <- ifelse(data$trans == 13 | data$trans == 28 | data$trans == 30, 6, data$to)
data$to <- ifelse(data$trans == 2 | data$trans == 9 | data$trans == 15 | data$trans == 22, 7, data$to)
data$to <- ifelse(data$trans == 3 | data$trans == 10 | data$trans == 16 | data$trans == 20, 8, data$to)
data$to <- ifelse(data$trans == 4, 9, data$to)
data$to <- ifelse(data$trans == 11, 10, data$to)
data$to <- ifelse(data$trans == 17, 11, data$to)
data$to <- ifelse(data$trans == 5 | data$trans == 7 | data$trans == 12 | data$trans == 14 | data$trans == 18 | data$trans == 19 | data$trans == 21 | data$trans == 23 | data$trans == 26 | data$trans == 29 | data$trans == 31, 12, data$to)

data$from <- factor(data$from, levels = 1:12, labels = c("1L-TKI", "1L-TKI after TFR", "2L-TKI", "2L-TKI after TFR", "3L-TKI", "3L-TKI after TFR", "Prog", "SCT", "TFR after 1L-TKI", "TFR after 2L-TKI", "TFR after 3L-TKI", "Death"))
data$to <- factor(data$to, levels = 1:12, labels = c("1L-TKI", "1L-TKI after TFR", "2L-TKI", "2L-TKI after TFR", "3L-TKI", "3L-TKI after TFR", "Prog", "SCT", "TFR after 1L-TKI", "TFR after 2L-TKI", "TFR after 3L-TKI", "Death"))

########################################################
## Define the conditions and specify _start and _stop ##
########################################################
# _trans == 1-5
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_tki1) & trans >= 1 & trans <= 5, date_tki1, start),
          stop = ifelse(!is.na(date_tki1) & trans >= 1 & trans <= 5, 
                         pmin(date_tki2, date_prog, date_sct, date_tfr1, date_death, na.rm = TRUE), stop),
          stop = ifelse(!is.na(date_tki1) & trans >= 1 & trans <= 5 & is.na(stop), date_study_exit, stop)
        )

# _trans == 6-7
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_tki1_) & trans >= 6 & trans <= 7 & pmin(date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tki1_, date_tki1_, start),
          stop = ifelse(!is.na(date_tki1_) & trans >= 6 & trans <= 7 & pmin(date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tki1_, pmin(date_tki2_, date_death, na.rm = TRUE), stop),
          stop = ifelse(!is.na(date_tki1_) & trans >= 6 & trans <= 7 & pmin(date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tki1_ & is.na(stop), date_study_exit, stop)
        )

# _trans == 8-12
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_tki2) & trans >= 8 & trans <= 12 & pmin(date_tki2, date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_tki2, date_tki2, start),
          stop = ifelse(!is.na(date_tki2) & trans >= 8 & trans <= 12 & pmin(date_tki2, date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_tki2, pmin(date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE), stop),
          stop = ifelse(!is.na(date_tki2) & trans >= 8 & trans <= 12 & pmin(date_tki2, date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_tki2 & is.na(stop), date_study_exit, stop)
        )

# _trans == 13-14
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_tki2_) & trans >= 13 & trans <= 14 & pmin(date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tki2_, date_tki2_, start),
          stop = ifelse(!is.na(date_tki2_) & trans >= 13 & trans <= 14 & pmin(date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tki2_, pmin(date_tki3_, date_death, na.rm = TRUE), stop),
          stop = ifelse(!is.na(date_tki2_) & trans >= 13 & trans <= 14 & pmin(date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tki2_ & is.na(stop), date_study_exit, stop)
        )

# _trans == 15-18
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_tki3) & trans >= 15 & trans <= 18 & pmin(date_tki3, date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE) == date_tki3, date_tki3, start),
          stop = ifelse(!is.na(date_tki3) & trans >= 15 & trans <= 18 & pmin(date_tki3, date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE) == date_tki3, pmin(date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE), stop),
          stop = ifelse(!is.na(date_tki3) & trans >= 15 & trans <= 18 & pmin(date_tki3, date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE) == date_tki3 & is.na(stop), date_study_exit, stop)
        )

# _trans == 19
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_tki3_) & trans == 19 & pmin(date_tki3_, date_death, na.rm = TRUE) == date_tki3_, date_tki3_, start),
          stop = ifelse(!is.na(date_tki3_) & trans == 19 & pmin(date_tki3_, date_death, na.rm = TRUE) == date_tki3_, date_death, stop),
          stop = ifelse(!is.na(date_tki3_) & trans == 19 & pmin(date_tki3_, date_death, na.rm = TRUE) == date_tki3_ & is.na(stop), date_study_exit, stop)
        )

# _trans == 20-21
data <- data %>%
        mutate(
          start = ifelse(!is.na(date_prog) & trans >= 20 & trans <= 21 & pmin(date_prog, date_sct, date_death, na.rm = TRUE) == date_prog, date_prog, start),
          stop = ifelse(!is.na(date_prog) & trans >= 20 & trans <= 21 & pmin(date_prog, date_sct, date_death, na.rm = TRUE) == date_prog, pmin(date_sct, date_death, na.rm = TRUE), stop),
          stop = ifelse(!is.na(date_prog) & trans >= 20 & trans <= 21 & pmin(date_prog, date_sct, date_death, na.rm = TRUE) == date_prog & is.na(stop), date_study_exit, stop)
        )

# _trans == 22-23
data <- data %>%
  mutate(
    start = ifelse(!is.na(date_sct) & trans >= 22 & trans <= 23 & pmin(date_prog, date_sct, date_death, na.rm = TRUE) == date_sct, date_sct, start),
    stop = ifelse(!is.na(date_sct) & trans >= 22 & trans <= 23 & pmin(date_prog, date_sct, date_death, na.rm = TRUE) == date_sct, pmin(date_prog, date_death, na.rm = TRUE), stop),
    stop = ifelse(!is.na(date_sct) & trans >= 22 & trans <= 23 & pmin(date_prog, date_sct, date_death, na.rm = TRUE) == date_sct & is.na(stop), date_study_exit, stop)
  )

# _trans == 24-26
data <- data %>%
  mutate(
    start = ifelse(!is.na(date_tfr1) & trans >= 24 & trans <= 26 & pmin(date_tfr1, date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tfr1, date_tfr1, start),
    stop = ifelse(!is.na(date_tfr1) & trans >= 24 & trans <= 26 & pmin(date_tfr1, date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tfr1, pmin(date_tki1_, date_tki2_, date_death, na.rm = TRUE), stop),
    stop = ifelse(!is.na(date_tfr1) & trans >= 24 & trans <= 26 & pmin(date_tfr1, date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tfr1 & is.na(stop), date_study_exit, stop)
  )

# _trans == 27-29
data <- data %>%
  mutate(
    start = ifelse(!is.na(date_tfr2) & trans >= 27 & trans <= 29 & pmin(date_tfr2, date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tfr2, date_tfr2, start),
    stop = ifelse(!is.na(date_tfr2) & trans >= 27 & trans <= 29 & pmin(date_tfr2, date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tfr2, pmin(date_tki2_, date_tki3_, date_death, na.rm = TRUE), stop),
    stop = ifelse(!is.na(date_tfr2) & trans >= 27 & trans <= 29 & pmin(date_tfr2, date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tfr2 & is.na(stop), date_study_exit, stop)
  )

# _trans == 30-31
data <- data %>%
  mutate(
    start = ifelse(!is.na(date_tfr3) & trans >= 30 & trans <= 31 & pmin(date_tfr3, date_tki3_, date_death, na.rm = TRUE) == date_tfr3, date_tfr3, start),
    stop = ifelse(!is.na(date_tfr3) & trans >= 30 & trans <= 31 & pmin(date_tfr3, date_tki3_, date_death, na.rm = TRUE) == date_tfr3, pmin(date_tki3_, date_death, na.rm = TRUE), stop),
    stop = ifelse(!is.na(date_tfr3) & trans >= 30 & trans <= 31 & pmin(date_tfr3, date_tki3_, date_death, na.rm = TRUE) == date_tfr3 & is.na(stop), date_study_exit, stop)
  )

# All rows
nrow(data)

# Drop if any missing
data <- data %>%
        filter(!is.na(start) & !is.na(stop))

# After filter out missing
nrow(data)

###########################
## Define status (event) ##
###########################
data$status <- 0

# _trans == 1-5
data <- data %>%
  mutate(
  status = case_when(
      trans == 1 & !is.na(date_tki1) & !is.na(date_tki2) & pmin(date_tki2, date_prog, date_sct, date_tfr1, date_death, na.rm = TRUE) == date_tki2 ~ 1,
      trans == 2 & !is.na(date_tki1) & !is.na(date_prog) & pmin(date_tki2, date_prog, date_sct, date_tfr1, date_death, na.rm = TRUE) == date_prog ~ 1,
      trans == 3 & !is.na(date_tki1) & !is.na(date_sct) & pmin(date_tki2, date_prog, date_sct, date_tfr1, date_death, na.rm = TRUE) == date_sct ~ 1,
      trans == 4 & !is.na(date_tki1) & !is.na(date_tfr1) & pmin(date_tki2, date_prog, date_sct, date_tfr1, date_death, na.rm = TRUE) == date_tfr1 ~ 1,
      trans == 5 & !is.na(date_tki1) & !is.na(date_death) == 1 & is.na(date_tki2) & is.na(date_prog) & is.na(date_sct) & is.na(date_tfr1) ~ 1,
      TRUE ~ status))

# _trans == 6-7
data <- data %>%
        mutate(
          status = case_when(
            trans == 6 & !is.na(date_tki1_) & !is.na(date_tki2_) & pmin(date_tki2_, date_death, na.rm = TRUE) == date_tki2_ ~ 1,
            trans == 7 & !is.na(date_tki1_) & !is.na(date_death) & is.na(date_tki2_) ~ 1,
            TRUE ~ status
          )
        )

# _trans == 8-12
data <- data %>%
  mutate(
    status = case_when(
      trans == 8 & !is.na(date_tki2) & !is.na(date_tki3) & pmin(date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_tki3 ~ 1,
      trans == 9 & !is.na(date_tki2) & !is.na(date_prog) & pmin(date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_prog ~ 1,
      trans == 10 & !is.na(date_tki2) & !is.na(date_sct) & pmin(date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_sct ~ 1,
      trans == 11 & !is.na(date_tki2) & !is.na(date_tfr2) & pmin(date_tki3, date_prog, date_sct, date_tfr2, date_death, na.rm = TRUE) == date_tfr2 ~ 1,
      trans == 12 & !is.na(date_tki2) & !is.na(date_death) & is.na(date_tki3) & is.na(date_prog) & is.na(date_sct) & is.na(date_tfr2) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 13-14
data <- data %>%
  mutate(
    status = case_when(
      trans == 13 & !is.na(date_tki2_) & !is.na(date_tki3_) & pmin(date_tki3_, date_death, na.rm = TRUE) == date_tki3_ ~ 1,
      trans == 14 & !is.na(date_tki2_) & !is.na(date_death) & is.na(date_tki3_)  ~ 1,
      TRUE ~ status
    )
  )

# _trans == 15-18
data <- data %>%
  mutate(
    status = case_when(
      trans == 15 & !is.na(date_tki3) & !is.na(date_prog) & pmin(date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE) == date_prog ~ 1,
      trans == 16 & !is.na(date_tki3) & !is.na(date_sct) & pmin(date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE) == date_sct ~ 1,
      trans == 17 & !is.na(date_tki3) & !is.na(date_tfr3) & pmin(date_prog, date_sct, date_tfr3, date_death, na.rm = TRUE) == date_tfr3 ~ 1,
      trans == 18 & !is.na(date_tki3) & !is.na(date_death) & is.na(date_prog) & is.na(date_sct) & is.na(date_tfr3) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 19
data <- data %>%
  mutate(
    status = case_when(
      trans == 19 & !is.na(date_tki3_) & !is.na(date_death) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 20-21
data <- data %>%
  mutate(
    status = case_when(
      trans == 20 & !is.na(date_prog) & !is.na(date_sct) & pmin(date_sct, date_death, na.rm = TRUE) == date_sct ~ 1,
      trans == 21 & !is.na(date_prog) & !is.na(date_death) & is.na(date_sct) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 22-23
data <- data %>%
  mutate(
    status = case_when(
      trans == 22 & !is.na(date_sct) & !is.na(date_prog) & pmin(date_prog, date_death, na.rm = TRUE) == date_prog ~ 1,
      trans == 23 & !is.na(date_sct) & !is.na(date_death) & is.na(date_prog) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 24-26
data <- data %>%
  mutate(
    status = case_when(
      trans == 24 & !is.na(date_tfr1) & !is.na(date_tki1_) & pmin(date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tki1_ ~ 1,
      trans == 25 & !is.na(date_tfr1) & !is.na(date_tki2_) & pmin(date_tki1_, date_tki2_, date_death, na.rm = TRUE) == date_tki2_ ~ 1,
      trans == 26 & !is.na(date_tfr1) & !is.na(date_death) & is.na(date_tki1_) & is.na(date_tki2_) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 27-29
data <- data %>%
  mutate(
    status = case_when(
      trans == 27 & !is.na(date_tfr2) & !is.na(date_tki2_) & pmin(date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tki2_ ~ 1,
      trans == 28 & !is.na(date_tfr2) & !is.na(date_tki3_) & pmin(date_tki2_, date_tki3_, date_death, na.rm = TRUE) == date_tki3_ ~ 1,
      trans == 29 & !is.na(date_tfr2) & !is.na(date_death) & is.na(date_tki2_) & is.na(date_tki3_) ~ 1,
      TRUE ~ status
    )
  )

# _trans == 30-31
data <- data %>%
  mutate(
    status = case_when(
      trans == 30 & !is.na(date_tfr3) & !is.na(date_tki3_) & pmin(date_tki3_, date_death, na.rm = TRUE) == date_tki3_ ~ 1,
      trans == 31 & !is.na(date_tfr3) & !is.na(date_death) & is.na(date_tki3_)  ~ 1,
      TRUE ~ status
    )
  )


# List rows where _start or _stop is NA
na_rows <- data %>%
           filter(is.na(start) | is.na(stop))%>%
           dplyr::select(idnr, start, stop, from, to, status, trans)
print(na_rows)

# Define the list of patient IDs
patient_ids <- c(6172, 13374, 20954)

# Filter the dataframe for these patient IDs and select the relevant columns
check_rows <- data%>%
              filter(idnr %in% patient_ids)%>%
              dplyr::select(idnr, start, stop, from, to, status, trans)

# Display the selected rows
print(check_rows)

#############################
## Merge with popmort file ##
#############################
studypop <- data %>%
            # Caution! the start here is date_tki1 instead of start!
            mutate(t = as.numeric(as.Date(stop) - date_tki1)/365.241,
                   dxyear = year(date_dx),
                   year = floor(dxyear + t), # attained year
                   age = floor(dxage + t),   # attained age
                   sex = ifelse(sex == 1, 1, 0))

# Load population mortality data
library(readstata13)
popmort <- read.dta13("../Data/00_popmort2022_proj.dta", nonint.factors = TRUE)
colnames(popmort)
colnames(popmort) <- c("year", "age", "sex", "rate", "prob")

# Sex variable
popmort <- popmort %>%
           mutate(sex = ifelse(sex == 1, 1, 0))

# Merge data with life table
studypop_rate <- studypop %>%
                 left_join(popmort, by = c("year" = "year", "sex" = "sex", "age" = "age"))%>%
                 dplyr::select(-t) # take away t to prevent confusion later in modelling

# Save the cleaned study population 
setwd(wd)
saveRDS(studypop_rate, file = "../Data/01c_studypop_msset.rds")

# Split by dxyear
saveRDS(subset(studypop_rate, dxyear >= 2007 & dxyear <= 2012), file = "../Data/01c_studypop_msset_0712.rds")
saveRDS(subset(studypop_rate, dxyear >= 2013 & dxyear <= 2017), file = "../Data/01c_studypop_msset_1317.rds")
