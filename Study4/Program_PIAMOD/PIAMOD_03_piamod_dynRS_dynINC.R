## File name: PIAMOD_03_piamod_dynRS_dynINC
## Updated: 2025-02-03 Enoch Chen: added dynamic relative survival and dynamic incidence 

## Project: CML Cost
## Purpose: organise PIAMOD's output data
library(tidyverse)
library(ggplot2)
library(dplyr)

##########################################################
####### Prevalence data for CML output from PIAMOD #######
##########################################################
# Define a function to read and process res file
read_and_process_piamod <- function(file_path) {
  # Read all lines, skipping the header
  lines <- readLines(file_path)[-c(1,2)]
  
  # Split each line by whitespace
  split_lines <- strsplit(lines, "\\s+")
  
  # Ensure each line has exactly 25 columns by padding with NA if necessary
  data_list <- lapply(split_lines, function(x) {
    length(x) <- 25
    return(x)
  })
  
  # Convert the list of vectors into a data frame
  df <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)
  
  # Assign column names to the data frame
  colnames(df) <- c("TYPE", "MODEL", "LRS", "SEX", "AMIN", "AMAX", "YEAR", 
                    "SURVIVORS", "MEANPOP", "OBS.CASES", "OBS.INCID.", "GEN.MORT.", 
                    "EXP.DTH.", "EXP.MORT.", "EXP.INCID.", "S.E.", "EXP.CASES", 
                    "S.E.2", "EXP.PREVAL.", "S.E.3", "PREV.CASES", "S.E.4", 
                    "STD.EXP.M", "STD.INC.", "STD.PREV.")
  
  # Convert each column to numeric
  df[] <- lapply(df, function(col) as.numeric(as.character(col)))
  
  # Return the processed data frame
  return(df)
}

# Use the function to read and process both files
piamod_m <- read_and_process_piamod("../Output/PIAMOD_Output/dynRS_dynINC_M.res")
piamod_f <- read_and_process_piamod("../Output/PIAMOD_Output/dynRS_dynINC_F.res")

# Read the confidence intervals for prediction period
prev_CIs_m <- read.table("../Data/PIAMOD_05a1_prev_CIs_m.txt") |>
  dplyr::select(year, EXP.PREVAL.S.E., STD.PREVAL.S.E.)|>
  rename(YEAR = year) |>
  mutate(AMIN = 0, AMAX = 99)

prev_CIs_f <- read.table("../Data/PIAMOD_05a1_prev_CIs_f.txt")|>
  dplyr::select(year, EXP.PREVAL.S.E., STD.PREVAL.S.E.)|>
  rename(YEAR = year) |>
  mutate(AMIN = 0, AMAX = 99)

# Merge 
piamod_m <- merge(piamod_m, prev_CIs_m, by = c("YEAR", "AMIN", "AMAX"),  all = TRUE)
piamod_f <- merge(piamod_f, prev_CIs_f, by = c("YEAR", "AMIN", "AMAX"),  all = TRUE)

# Check the output
head(piamod_m)
head(piamod_f)

# Generate number of parameters
sum_digits <- function(x) {
  sum(as.numeric(unlist(strsplit(as.character(x), ""))))
}

# Apply the function to the 'MODEL' column
piamod_m$param <- sapply(piamod_m$MODEL, sum_digits) + 1 
piamod_f$param <- sapply(piamod_f$MODEL, sum_digits) + 1

########################################################################
####### Plot LRS and parameters to decide which model to select ########
########################################################################
# I selected 402 for males and 602 for females
# See the selection process in the R script "PIAMOD_02_piamod_dynRS_constantINC.R"
piamod_m_final <- subset(piamod_m, piamod_m$MODEL==402000)%>%
  select(-param)
piamod_f_final <- subset(piamod_f, piamod_f$MODEL==602000)%>%
  select(-param)
temp <- subset(piamod_m, YEAR == 2020 & AMIN == 0 & AMAX == 99)

##############################################################
######### Create the total population (Males+Females) ########
##############################################################
# Select variables to sum, excluding non-numeric columns
columns_to_sum <- c("SEX", "SURVIVORS", "MEANPOP", "OBS.CASES", "EXP.CASES", "PREV.CASES")

# Initialize piamod_total_final with piamod_m_final
piamod_total_final <- piamod_m_final

# Ensure the columns are numeric
piamod_m_final[columns_to_sum] <- lapply(piamod_m_final[columns_to_sum], as.numeric)
piamod_f_final[columns_to_sum] <- lapply(piamod_f_final[columns_to_sum], as.numeric)

# Compute the sum of corresponding columns in piamod_m_final and piamod_f_final
piamod_total_final[columns_to_sum] <- piamod_m_final[columns_to_sum] + piamod_f_final[columns_to_sum]

# Calculate the combined standard error for S.E., S.E.2, S.E.3, and S.E.4
# s_z(x + y)^2 = (n-1)s_x^2 + (m-1)s_y^2 / (n+m-2)
mean_columns <- c("OBS.INCID.", "EXP.INCID.", "EXP.PREVAL.", "STD.INC.", "STD.PREV.")
for (mean_col in mean_columns) {
  piamod_total_final[, mean_col] <- (piamod_m_final$MEANPOP * (piamod_m_final[[mean_col]]) + 
                                       piamod_f_final$MEANPOP * (piamod_f_final[[mean_col]])) / 
    (piamod_m_final$MEANPOP + piamod_f_final$MEANPOP)
}

se_columns <- c("S.E.", "S.E.2", "S.E.3", "S.E.4", "EXP.PREVAL.S.E.", "STD.PREVAL.S.E.")
for (se_col in se_columns) {
  piamod_total_final[, se_col] <- sqrt(((piamod_m_final$MEANPOP - 1) * (piamod_m_final[[se_col]]^2) + 
                                          (piamod_f_final$MEANPOP - 1) * (piamod_f_final[[se_col]]^2)) / 
                                         (piamod_m_final$MEANPOP + piamod_f_final$MEANPOP - 2))
}
head(piamod_total_final, 1)
head(piamod_m_final, 1)
head(piamod_f_final, 1)

# Stack three data frames
piamod <- rbind(piamod_m_final, piamod_f_final, piamod_total_final) %>%
  select(-c("MODEL","LRS", "TYPE", "STD.EXP.M"))

# Create a reference dataset for 2019 values
se_ref <- piamod %>%
  filter(YEAR == 2019) %>%
  select(SEX, AMIN, AMAX, S.E., S.E.2)

# Replace values for years 2020-2030 with those from 2019
piamod <- piamod %>%
  mutate(across(c(S.E., S.E.2), ~ 
                  if_else(YEAR >= 2020 & YEAR <= 2030, 
                          se_ref[[cur_column()]][match(interaction(SEX, AMIN, AMAX), 
                                                       interaction(se_ref$SEX, se_ref$AMIN, se_ref$AMAX))], .)))

# For prevalences used the one calculated
piamod <- piamod %>%
  mutate(
    across(c(S.E.3, S.E.4), ~ if_else(
      YEAR >= 1973 & YEAR <= 2030, 
      case_when(
        cur_column() == "S.E.3" ~ EXP.PREVAL.S.E.,
        cur_column() == "S.E.4" ~ EXP.PREVAL.S.E.*MEANPOP/100000
      ),
      .
    ))
  )

# Create 95% CIs
z <- qnorm(.975)
piamod$EXP.INCID.lb <- piamod$EXP.INCID. - (z* piamod$S.E.)
piamod$EXP.INCID.ub <- piamod$EXP.INCID. + (z* piamod$S.E.)
piamod$EXP.CASES.lb <- piamod$EXP.CASES - (z* piamod$S.E.2)
piamod$EXP.CASES.ub <- piamod$EXP.CASES + (z* piamod$S.E.2)
piamod$EXP.PREVAL.lb <- piamod$EXP.PREVAL. - (z* piamod$S.E.3)
piamod$EXP.PREVAL.ub <- piamod$EXP.PREVAL. + (z* piamod$S.E.3)
piamod$PREV.CASES.lb <- piamod$PREV.CASES - (z* piamod$S.E.4)
piamod$PREV.CASES.ub <- piamod$PREV.CASES + (z* piamod$S.E.4)
piamod$STD.INC.lb <- piamod$STD.INC. - (z* piamod$S.E.)
piamod$STD.INC.ub <- piamod$STD.INC. + (z* piamod$S.E.)
piamod$STD.PREV.lb <- piamod$STD.PREV. - (z* piamod$S.E.3)
piamod$STD.PREV.ub <- piamod$STD.PREV. + (z* piamod$S.E.3)


###########################################
######### Re-categorize age groups ########
###########################################
# Now is still 5-year age groups
# I will re-categorize the age groups to 20-44, 45-54, 55-64, 65-74, 75-99 years
# First, do not touch those groups with AMIN == 0 and AMAX == 99
nrow(piamod)
piamod_a <- subset(piamod, AMIN == 0 & AMAX == 99)
piamod_a$AGE_GROUP <- "0-99"
piamod_a <- piamod_a |>
  select(SEX, AGE_GROUP, YEAR, SURVIVORS, MEANPOP, OBS.CASES,    
         OBS.INCID., EXP.INCID., S.E.,
         EXP.CASES, S.E.2, EXP.PREVAL., S.E.3, PREV.CASES, S.E.4, 
         EXP.INCID.lb, EXP.INCID.ub, EXP.CASES.lb, EXP.CASES.ub, 
         EXP.PREVAL.lb, EXP.PREVAL.ub, PREV.CASES.lb, PREV.CASES.ub,
         STD.INC., STD.INC.lb, STD.INC.ub, STD.PREV., STD.PREV.lb, STD.PREV.ub)
nrow(piamod_a)

# Then, re-categorize the rest
piamod_b <- subset(piamod, AMIN != 0 | AMAX != 99 | (AMIN == 0 & AMAX != 99) | (AMIN != 0 & AMAX == 99))
nrow(piamod_b)

# Quality check
isTRUE(nrow(piamod) == nrow(piamod_a) + nrow(piamod_b))

# Re-categorize the age groups
piamod_b$AGE_GROUP <- cut(piamod_b$AMIN, breaks = c(19, 44, 54, 64, 74, 99), 
                          labels = c("20-44", "45-54", "55-64", "65-74", "75-99"))

# Only keep if AGE_GROUP is not NA
# Those who are NA are less than 20 years
piamod_b <- piamod_b[!is.na(piamod_b$AGE_GROUP),]

# Organize the columns as SEX AMIN AMAX AGE_GROUP and the rest
piamod_b <- piamod_b |>
  select(SEX, AMIN, AMAX, AGE_GROUP, everything())

head(piamod_b)

# Define a map for age groups
age_group_map <- list(
  `20-44` = c(20, 25, 30, 35, 40),
  `45-54` = c(45, 50),
  `55-64` = c(55, 60),
  `65-74` = c(65, 70),
  `75-99` = c(75, 80, 85, 90, 95)
)

# Summarize data for all age groups
piamod_b_sum_M <- purrr::map_dfr(
  names(age_group_map),
  ~ piamod_b %>%
    filter(SEX == 1 & AMIN %in% age_group_map[[.x]]) %>%
    group_by(YEAR) %>%
    summarise(across(all_of(columns_to_sum), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      AMIN = min(age_group_map[[.x]]),
      AMAX = max(age_group_map[[.x]]),
      AGE_GROUP = .x
    )
) |>
  mutate(SEX = 1)

piamod_b_sum_F <- purrr::map_dfr(
  names(age_group_map),
  ~ piamod_b %>%
    filter(SEX == 2 & AMIN %in% age_group_map[[.x]]) %>%
    group_by(YEAR) %>%
    summarise(across(all_of(columns_to_sum), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      AMIN = min(age_group_map[[.x]]),
      AMAX = max(age_group_map[[.x]]),
      AGE_GROUP = .x
    )
)|>
  mutate(SEX = 2)

piamod_b_sum_T <- purrr::map_dfr(
  names(age_group_map),
  ~ piamod_b %>%
    filter(SEX == 3 & AMIN %in% age_group_map[[.x]]) %>%
    group_by(YEAR) %>%
    summarise(across(all_of(columns_to_sum), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      AMIN = min(age_group_map[[.x]]),
      AMAX = max(age_group_map[[.x]]),
      AGE_GROUP = .x
    )
)|>
  mutate(SEX = 3)

# Combine the data frames
piamod_b_sum <- bind_rows(piamod_b_sum_M, piamod_b_sum_F, piamod_b_sum_T)


# Calculate means
mean_columns <- c("OBS.INCID.", "EXP.INCID.", "EXP.PREVAL.", "STD.INC.", "STD.PREV.")

piamod_b_mean <- piamod_b %>%
  group_by(SEX, YEAR, AGE_GROUP) %>%
  summarise(
    across(
      all_of(mean_columns),
      ~ weighted.mean(.x, w = MEANPOP, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# Calculate S.E.
piamod_b_se <- piamod_b |>
  select(SEX, AMIN, AMAX, AGE_GROUP, YEAR, MEANPOP, S.E., S.E.2, S.E.3, S.E.4)

# Define the SE columns
se_columns <- c("S.E.", "S.E.2", "S.E.3", "S.E.4")

# Function to calculate total SE using all SE columns
calculate_total_se <- function(amins, meanpop, se_cols) {
  weighted_se <- sum((meanpop[amins] - 1) * (se_cols[amins]^2))
  sum_meanpop <- sum(meanpop[amins])
  return(sqrt(weighted_se / (sum_meanpop - length(amins))))
}

# Optimized code with loop and across
piamod_b_se_all <- piamod_b_se %>%
  group_by(YEAR, SEX, AGE_GROUP) %>%
  mutate(
    # Apply calculate_total_se for each SE column dynamically
    across(
      starts_with("S.E."), 
      ~ case_when(
        AGE_GROUP == "20-44" ~ calculate_total_se(which(AMIN %in% c(20, 25, 30, 35, 40)), MEANPOP, .),
        AGE_GROUP == "45-54" ~ calculate_total_se(which(AMIN %in% c(45, 50)), MEANPOP, .),
        AGE_GROUP == "55-64" ~ calculate_total_se(which(AMIN %in% c(55, 60)), MEANPOP, .),
        AGE_GROUP == "65-74" ~ calculate_total_se(which(AMIN %in% c(65, 70)), MEANPOP, .),
        AGE_GROUP == "75-99" ~ calculate_total_se(which(AMIN %in% c(75, 80, 85, 90, 95)), MEANPOP, .),
        TRUE ~ NA_real_
      ),
      .names = "total_{.col}"  # Rename the columns to include "total_"
    )
  ) %>%
  ungroup() %>%
  distinct(SEX, AGE_GROUP, YEAR, .keep_all = TRUE) %>%  # Keep first row for each combination
  select(SEX, AGE_GROUP, YEAR, starts_with("total_")) %>%
  rename_with(~ gsub("total_", "", .x))

# Merge the sum, mean, and SE
piamod_b_final <- piamod_b_sum %>%
  left_join(piamod_b_mean, by = c("SEX", "YEAR", "AGE_GROUP")) %>%
  left_join(piamod_b_se_all, by = c("SEX", "YEAR", "AGE_GROUP"))

# Create 95% CIs
z <- qnorm(.975)
piamod_b_final$EXP.INCID.lb <- piamod_b_final$EXP.INCID. - (z* piamod_b_final$S.E.)
piamod_b_final$EXP.INCID.ub <- piamod_b_final$EXP.INCID. + (z* piamod_b_final$S.E.)
piamod_b_final$EXP.CASES.lb <- piamod_b_final$EXP.CASES - (z* piamod_b_final$S.E.2)
piamod_b_final$EXP.CASES.ub <- piamod_b_final$EXP.CASES + (z* piamod_b_final$S.E.2)
piamod_b_final$EXP.PREVAL.lb <- piamod_b_final$EXP.PREVAL. - (z* piamod_b_final$S.E.3)
piamod_b_final$EXP.PREVAL.ub <- piamod_b_final$EXP.PREVAL. + (z* piamod_b_final$S.E.3)
piamod_b_final$PREV.CASES.lb <- piamod_b_final$PREV.CASES - (z* piamod_b_final$S.E.4)
piamod_b_final$PREV.CASES.ub <- piamod_b_final$PREV.CASES + (z* piamod_b_final$S.E.4)
piamod_b_final$STD.INC.lb <- piamod_b_final$STD.INC. - (z* piamod_b_final$S.E.)
piamod_b_final$STD.INC.ub <- piamod_b_final$STD.INC. + (z* piamod_b_final$S.E.)
piamod_b_final$STD.PREV.lb <- piamod_b_final$STD.PREV. - (z* piamod_b_final$S.E.3)
piamod_b_final$STD.PREV.ub <- piamod_b_final$STD.PREV. + (z* piamod_b_final$S.E.3)

# Order the columns
piamod_b_final <- piamod_b_final |>
  select(SEX, AGE_GROUP, YEAR, SURVIVORS, MEANPOP, OBS.CASES,    
         OBS.INCID., EXP.INCID., S.E.,
         EXP.CASES, S.E.2, EXP.PREVAL., S.E.3, PREV.CASES, S.E.4, 
         EXP.INCID.lb, EXP.INCID.ub, EXP.CASES.lb, EXP.CASES.ub, 
         EXP.PREVAL.lb, EXP.PREVAL.ub, PREV.CASES.lb, PREV.CASES.ub,
         STD.INC., STD.INC.lb, STD.INC.ub, STD.PREV., STD.PREV.lb, STD.PREV.ub)

# Put all data together
piamod_final <- rbind(piamod_a, piamod_b_final)
colnames(piamod_final)
nrow(piamod_final)

# Save the final data
write.csv(piamod_final, "../Data/PIAMOD_03_piamod_dynRS_dynINC.csv", row.names = FALSE)

