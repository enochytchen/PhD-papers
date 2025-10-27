## Filename: Cost_04a_prev_dist.R 
## Updated: 2025-03-26 Enoch Chen: took the average of ages in each age group instead of median
##          2024-12-05 Enoch Chen: Tabulate the prevalence cases by year


## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
}
setwd(wd)

###################
## Read packages ##
###################
library(tidyverse)
library(dplyr)
library(parallel)

########################
## Read piamod output ##
########################
piamod <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
head(piamod)
table(piamod$AGE_GROUP, piamod$SEX)

## Create the one with total
piamod_ALL <- piamod[piamod$SEX == 3 & piamod$AGE_GROUP == "0-99" & piamod$YEAR >= 2000,] |>
              dplyr::select(c("YEAR", "PREV.CASES"))

## The original dataset contains a group with 0-99 and total (males + females)
## We fitted the survival models with age and sex; hence, 
## Keep only males and females, keep the age groups as they are 
## and keep the years from 2000 onwards
piamod1 <- piamod[(piamod$SEX == 1 | piamod$SEX == 2) & piamod$AGE_GROUP != "0-99" & 
                   piamod$YEAR >= 2000,] |>
           mutate(SEX = ifelse(SEX == 2, 0, SEX)) # Female == SEX == 2, now SEX == 0

# So now we have 31 years of data, by SEX and AGE_GROUP
table(piamod1$AGE_GROUP, piamod1$SEX)

## Select essential variables
## Use observed incidence cases for 2007 to 2019
## Use expected incidence cases for 2020 to 2030
## Use expected prevalence cases for all years
## Only keep incidence cases
piamod2 <- piamod1 |>
           mutate(INC.CASES = ifelse(YEAR < 2020, OBS.CASES, EXP.CASES)) |>
           dplyr::select(c("SEX", "AGE_GROUP", "YEAR", "INC.CASES"))

#########################################
## Create a list of data frames by age ##
## group and sex                       ##
#########################################

dxyears <- 2000:2030
attyears <- 2000:2030 # Eventually we only care about 2015 to 2030
age_groups <- unique(piamod2$AGE_GROUP)
sexes <- unique(piamod2$SEX)

# Create lists to store data frames
listdf_num <- list()

states <- c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD")

# Loop over age groups, sexes, and diagnosis years
for (age_group in age_groups) {
  for (sex in sexes) {
    for (dxyear in dxyears) {
      
      # Create a data frame for the given age group, sex, and dxyear
      df <- data.frame(
        attyear = attyears,
        YEAR = dxyear,
        time = attyears - dxyear,
        AGE_GROUP = age_group,
        SEX = sex,
        Tki1 = rep(0, length(attyears)),
        Tki2 = rep(0, length(attyears)),
        Tki3 = rep(0, length(attyears)),
        Sct = rep(0, length(attyears)), 
        Tfr = rep(0, length(attyears)),
        Prog = rep(0, length(attyears)),
        ExcD = rep(0, length(attyears)),
        ExpD = rep(0, length(attyears))
      ) |>
      filter(attyear >= dxyear)
      
      # Store in list, keyed by age group, sex, and dxyear
      # Two lists, one for the number of cases, one for the probabilities
      listdf_num[[""]] <- df 
    }
  }
}
df_num <- do.call(rbind, listdf_num)

# Merge the incidence cases with the empty data frame
df_num2 <- left_join(piamod2, df_num, by = c("YEAR", "AGE_GROUP", "SEX"))

# Merge with phase at diagnosis data
phase_at_dx <- readRDS(file="../Data/Cost_00_phase_at_dx.rds") |>
               dplyr::select(YEAR, CP, Prog) |>
               rename(prop_CP = CP,
                      prop_Prog = Prog)

df_num3 <- df_num2 |>
           left_join(phase_at_dx, by = "YEAR") |>
           mutate(INC.CASES.CP = INC.CASES * prop_CP/100,
                  INC.CASES.Prog = INC.CASES * prop_Prog/100)
head(df_num3)

df_INC.CASES.CP <- df_num3 |>
                   dplyr::select(-c("prop_CP", "prop_Prog", "INC.CASES", "INC.CASES.Prog"))

df_INC.CASES.Prog <- df_num3 |>
                     dplyr::select(-c("prop_CP", "prop_Prog", "INC.CASES", "INC.CASES.CP"))

# Save the data frame
df_num4 <- df_num3 |>
           dplyr::select(SEX, AGE_GROUP, YEAR, INC.CASES.CP, INC.CASES.Prog) |>
           distinct() |>  # drop duplicates
           group_by(YEAR) |>  # group by SEX and YEAR
           summarise(across(c(INC.CASES.CP, INC.CASES.Prog), sum), .groups = "drop") |>
           mutate(INC.CASES.CP = round(INC.CASES.CP, 1),
                  INC.CASES.Prog = round(INC.CASES.Prog, 1))
write.csv(df_num4, file="../Data/Cost_04a_prev_dist_inc.csv", row.names = FALSE)

#############################################################################################    
##=========================================================================================##
## Use the microsimulation model to determine the distribution of patients diagnosed at CP ##
##=========================================================================================##
#############################################################################################
    ## Run Microsimulation model
    source("Cost_03b_microsim_cml.R")
    
    # List of age-sex combinations
    ages <- 20:99
    sexes <- c(1, 0)
    
    ndata_list <- setNames(
      lapply(ages, function(age) {
        lapply(sexes, function(sex) {
          data.frame(dxage = age, sex = sex)
        })
      }) |> unlist(recursive = FALSE),
      paste0("ndata_age", rep(ages, each = 2), "_", rep(c("m", "f"), times = length(ages)))
    )
    
    # Function to set up parameters and run the simulation
    run_simulation <- function(ndata) {
      param <- list(
        mu0 = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$rate,
        ages = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$age,
        partitionBy = 0.1,
        debug = FALSE,
        dxage = ndata$dxage,
        sex = ndata$sex,
        hsu_tki1 = 0.76,
        hsu_tki2 = 0.68,
        hsu_tki3 = 0.68,
        hsu_prog = 0.41,
        hsu_sct = 0.80,
        hsu_tfr = 0.84,
        m1 = gsm_design(m1, newdata = ndata),
        m2 = gsm_design(m2, newdata = ndata),
        m3rate = m3rate$rate,
        m4 = gsm_design(m4, newdata = ndata),
        m7rate = m7rate$rate,
        m8 = gsm_design(m8, newdata = ndata),
        m9 = gsm_design(m9, newdata = ndata),
        m10 = gsm_design(m10, newdata = ndata),
        m11 = gsm_design(m11, newdata = ndata),
        m13rate = m13rate$rate,
        m15rate = m15rate$rate,
        m16 = gsm_design(m16, newdata = ndata),
        m17rate = m17rate$rate,
        m20 = gsm_design(m20, newdata = ndata),
        m21 = gsm_design(m21, newdata = ndata),
        m22rate = m22rate$rate,
        m24rate = m24rate$rate,
        m25rate = m25rate$rate,
        m27rate = m27rate$rate,
        m28rate = m28rate$rate,
        m30rate = m30rate$rate
      )
      simulations(1e5, param, indivp = FALSE)
    }
    
    # Apply the simulation for each ndata
    listsims <- mclapply(ndata_list, run_simulation, mc.cores = 5)
    
    # Name results by age-sex combination
    names(listsims) <- names(ndata_list)
    
    # Extract the results from the microsimulation model
    listprev <- lapply(listsims, function(x) as.data.frame(x$prev))
    
    # Create a lookup table for age group and sex
    ages <- 20:99
    sexes <- c("m", "f")
    SEX <- c(1, 0)  # 1 = Male, 0 = Female
    
    lookup_tab <- do.call(rbind, lapply(ages, function(age) {
      do.call(rbind, lapply(1:2, function(i) {
        data.frame(
          dataset_name = paste0("ndata_age", age, "_", sexes[i]),
          AGE = age,
          SEX = SEX[i],
          AGE_GROUP = cut(age,
                          breaks = c(19, 44, 54, 64, 74, 99),
                          labels = c("20-44", "45-54", "55-64", "65-74", "75-99"),
                          right = TRUE)
        )
      }))
    }))
    
    listprev2 <- lapply(seq_along(listprev), function(i) {
      # Extract one df
      prevalence <- listprev[[i]]
      # Calculate the stacked probabilities
      prevalence$prob <- prevalence$number / listsims[[i]]$n
      # Process and filter the data
      prevalence <- prevalence |>
                    mutate(time = round(time, 1)) |>
                    filter(time %in% seq(0, 50, by = 1))
      # Label with dataframe names
      prevalence$dataset_name <- names(listprev)[i]
      
      # Merge with lookup table
      prevalence <- prevalence |>
                    left_join(lookup_tab, by = "dataset_name") 
      
      return(prevalence)
    })
    
    # Combine results from all datasets into one data frame
    listprev3 <- do.call(rbind, listprev2) |>
                 dplyr::select(-c("dataset_name", "number"))
    
    print(tail(listprev3))
    
    # Average across ages
    listprev4 <- listprev3 |>
                 group_by(state, time, SEX, AGE_GROUP) |>
                 summarise(prob = mean(prob), .groups = "drop")
    
    # Now I want to have the dataset being wide
    # First, create a list of age groups and sex values to loop through
    age_groups <- unique(listprev4$AGE_GROUP)
    sex_values <- unique(listprev4$SEX)
    
    # List of states to match (same as those in the 'prevalence' table)
    states <- c("Tki1", "Tki2", "Tki3", "Sct", "Tfr1", "Tfr2", "Tfr3", "Prog", "ExcD", "ExpD")
    
    # Initialize an empty list to store data for each age-sex combination
    results_list <- list()
    
    # Loop over all age groups and sex combinations
    for (age_group in age_groups) {
      for (sex in sex_values) {
        
        # Subset the data for the given age group and sex combination
        temp <- subset(listprev4, AGE_GROUP == age_group & SEX == sex)
        
        # Fill in the probabilities for each state by time
        for (state in states) {
          temp[[state]] <- sapply(temp$time, function(year) {
            prob_value <- temp$prob[temp$time == year & temp$state == state]
            if (length(prob_value) > 0) {
              return(prob_value)
            } else {
              return(0)  # If no matching probability is found, return 0
            }
          })
        }
        
        # Filter rows 
        temp <- temp |>
                filter(time <= max(temp$time) & state == "Tki1") |>
                dplyr::select(-state)
        
        # Sum over all states for quality check
        temp$sum <- rowSums(temp[, states])
        
        # Store the processed data in the list
        results_list[[""]] <- temp
      }
    }
    
    # Combine all the individual dataframes into a single dataframe
    # This is the distribution of CP-CML patients starting from 1L TKI over time,
    # by age ageroup and sex 
    df_prob <- do.call(rbind, results_list) |>
               dplyr::select(-prob)
    
    # Change the column names
    colnames(df_prob) <- c("time", "SEX", "AGE_GROUP", "p.Tki1", "p.Tki2", "p.Tki3",     
                           "p.Sct", "p.Tfr1", "p.Tfr2", "p.Tfr3", "p.Prog", "p.ExcD", "p.ExpD", "p.sum")      
    df_prob <- df_prob |>
               mutate(p.Tfr = p.Tfr1 + p.Tfr2 + p.Tfr3) |>
               dplyr::select(-c("p.Tfr1", "p.Tfr2", "p.Tfr3"))
               
    colnames(df_INC.CASES.CP)
    colnames(df_prob)
    
    # Merge the prevalence probabilities with the empty data frame
    df_prev <- dplyr::left_join(df_INC.CASES.CP, df_prob, by = c("time", "AGE_GROUP", "SEX"))
    colnames(df_prev)
    
    # Multiply the prevalence probabilities with the incidence cases
    df_prev2 <- df_prev |>
                mutate(Tki1 = INC.CASES.CP * p.Tki1,
                       Tki2 = INC.CASES.CP * p.Tki2,
                       Tki3 = INC.CASES.CP * p.Tki3,
                       Sct = INC.CASES.CP * p.Sct,
                       Tfr = INC.CASES.CP * p.Tfr, 
                       Prog = INC.CASES.CP * p.Prog,
                       ExcD = INC.CASES.CP * p.ExcD,
                       ExpD = INC.CASES.CP * p.ExpD) |>
                dplyr::select(-c("p.Tki1", "p.Tki2", "p.Tki3", "p.Sct", "p.Tfr", 
                                 "p.Prog", "p.ExcD", "p.ExpD", "p.sum"))
    colnames(df_prev2)
    
    # Make data longer
    df_wide <- df_prev2 |>
      pivot_longer(
        cols = c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD"),
        names_to = "variable",
        values_to = "value"
      ) |>
      pivot_wider(
        names_from = attyear,
        values_from = value
      )
    
    write.csv(df_wide, file="../Data/Cost_04a_prev_dist_CPinc_year.csv", row.names = FALSE)
    
    # List of states
    states <- c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD")
    
    # Break the dataset into various dataframes in a list
    # "Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD"
    df_wide_list <- list()
    for (state in states) {
      df_wide_list[[state]] <- df_wide |>
                               filter(variable == state) |>
                               dplyr::select(-variable)
    }
    
    # Initialize an empty list to store results
    state_results <- list()
    
    # Loop through each state
    for (state in states) {
      # Extract the wide data frame for the current state
      state_df <- df_wide_list[[state]]
      
      # Replace NAs with 0
      state_df[is.na(state_df)] <- 0
      
      # Group and summarize by SEX and AGE_GROUP
      row_sums_state <- state_df |>
        dplyr::group_by(SEX, AGE_GROUP) |>
        dplyr::select(-SEX, -AGE_GROUP, -YEAR, -INC.CASES.CP, -time) |>
        summarise(across(everything(), sum, na.rm = TRUE)) |>
        ungroup()
      
      # Create a total row for the state
      total_row <- row_sums_state |>
        summarise(
          SEX = 3,
          AGE_GROUP = "TOTAL",
          across(`2000`:`2030`, sum, na.rm = TRUE)
        )
      
      # Combine the summarized data with the total row
      row_sums_state <- bind_rows(row_sums_state, total_row)
      
      # Add a column for the state
      row_sums_state <- row_sums_state |>
        mutate(state = state)
      
      # Store the result in the list
      state_results[[state]] <- row_sums_state
    }
    
    # Combine all state results into a single data frame
    final_result <- bind_rows(state_results)
    final_result <- final_result |>
                    dplyr::select(state, everything()) |>
                    mutate(across(where(is.numeric), ~ round(.x, 2)))
    
    final_result_TOTAL_CP <- subset(final_result, AGE_GROUP == "TOTAL")
      write.csv(final_result_TOTAL_CP, file="../Data/Cost_04a_prev_dist_CPinc_year.csv", row.names = FALSE)
    final_result_ALIVE_CP <- subset(final_result_TOTAL_CP, state %in% c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog")) |>
      summarise(
        AGE_GROUP = "TOTAL_ALIVE",
        across(`2000`:`2030`, sum, na.rm = TRUE)
      )
  
###############################################################################################    
##===========================================================================================##
## Use the microsimulation model to determine the distribution of patients diagnosed at Prog ##
##===========================================================================================##
###############################################################################################
    ## Run Microsimulation model
    source("Cost_03c_microsim_cml_apbp.R")
    
    # List of age-sex combinations
    ages <- 20:99
    sexes <- c(1, 0)
    
    ndata_list <- setNames(
      lapply(ages, function(age) {
        lapply(sexes, function(sex) {
          data.frame(dxage = age, sex = sex)
        })
      }) |> unlist(recursive = FALSE),
      paste0("ndata_age", rep(ages, each = 2), "_", rep(c("m", "f"), times = length(ages)))
    )
    
    # Function to set up parameters and run the simulation
    run_simulation <- function(ndata) {
      param <- list(
        mu0 = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$rate,
        ages = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$age,
        partitionBy = 0.1,
        debug = FALSE,
        dxage = ndata$dxage,
        sex = ndata$sex,
        m21 = gsm_design(m21, newdata = ndata)
      )
      simulations_apbp(1e5, param, indivp = FALSE)
    }
    
    # Apply the simulation for each ndata
    listsims <- mclapply(ndata_list, run_simulation, mc.cores = 5)
    
    # Name results by age-sex combination
    names(listsims) <- names(ndata_list)
    
    # Extract the results from the microsimulation model
    listprev <- lapply(listsims, function(x) as.data.frame(x$prev))
    
    # Create a lookup table for age group and sex
    ages <- 20:99
    sexes <- c("m", "f")
    SEX <- c(1, 0)  # 1 = Male, 0 = Female
    
    lookup_tab <- do.call(rbind, lapply(ages, function(age) {
      do.call(rbind, lapply(1:2, function(i) {
        data.frame(
          dataset_name = paste0("ndata_age", age, "_", sexes[i]),
          AGE = age,
          SEX = SEX[i],
          AGE_GROUP = cut(age,
                          breaks = c(19, 44, 54, 64, 74, 99),
                          labels = c("20-44", "45-54", "55-64", "65-74", "75-99"),
                          right = TRUE)
        )
      }))
    }))
    
    listprev2 <- lapply(seq_along(listprev), function(i) {
      # Extract one df
      prevalence <- listprev[[i]]
      # Calculate the stacked probabilities
      prevalence$prob <- prevalence$number / listsims[[i]]$n
      # Process and filter the data
      prevalence <- prevalence |>
        mutate(time = round(time, 1)) |>
        filter(time %in% seq(0, 50, by = 1))
      # Label with dataframe names
      prevalence$dataset_name <- names(listprev)[i]
      
      # Merge with lookup table
      prevalence <- prevalence |>
        left_join(lookup_tab, by = "dataset_name") 
      
      return(prevalence)
    })
    
    # Combine results from all datasets into one data frame
    listprev3 <- do.call(rbind, listprev2) |>
      dplyr::select(-c("dataset_name", "number"))
    
    print(tail(listprev3))
    
    # Average across ages
    listprev4 <- listprev3 |>
                 group_by(state, time, SEX, AGE_GROUP) |>
                 summarise(prob = mean(prob), .groups = "drop")
    
    # Now I want to have the dataset being wide
    # First, create a list of age groups and sex values to loop through
    age_groups <- unique(listprev4$AGE_GROUP)
    sex_values <- unique(listprev4$SEX)
    
    # List of states to match (same as those in the 'prevalence' table)
    states <- c("Prog", "ExcD", "ExpD")
    
    # Initialize an empty list to store data for each age-sex combination
    results_list <- list()
    
    # Loop over all age groups and sex combinations
    for (age_group in age_groups) {
     for (sex in sex_values) {
        
        # Subset the data for the given age group and sex combination
        temp <- subset(listprev4, AGE_GROUP == age_group & SEX == sex)
        
        # Fill in the probabilities for each state by time
        for (state in states) {
          temp[[state]] <- sapply(temp$time, function(year) {
            prob_value <- temp$prob[temp$time == year & temp$state == state]
            if (length(prob_value) > 0) {
              return(prob_value)
            } else {
              return(0)  # If no matching probability is found, return 0
            }
          })
        }
        
        # Filter rows 
        temp <- temp |>
          filter(time <= max(temp$time) & state == "Prog") |>
          dplyr::select(-state)
        
        # Sum over all states for quality check
        temp$sum <- rowSums(temp[, states])
        
        # Store the processed data in the list
        results_list[[""]] <- temp
      }
    }
    
    # Combine all the individual dataframes into a single dataframe
    # This is the distribution of AP/BP-CML patients starting from Prog over time,
    # by age ageroup and sex 
    df_prob <- do.call(rbind, results_list) |>
      dplyr::select(-prob)
    
    # Change the column names
    colnames(df_prob) <- c("time", "SEX", "AGE_GROUP", "p.Prog", "p.ExcD", "p.ExpD", "p.sum")      
    
    colnames(df_INC.CASES.Prog)
    colnames(df_prob)
    
    # Merge the prevalence probabilities with the empty data frame
    df_prev <- dplyr::left_join(df_INC.CASES.Prog, df_prob, by = c("time", "AGE_GROUP", "SEX"))
    colnames(df_prev)
    
    # Multiply the prevalence probabilities with the incidence cases
    df_prev2 <- df_prev |>
      mutate(Tki1 = INC.CASES.Prog * 0,
             Tki2 = INC.CASES.Prog * 0,
             Tki3 = INC.CASES.Prog * 0,
             Sct = INC.CASES.Prog * 0,
             Tfr = INC.CASES.Prog * 0, 
             Prog = INC.CASES.Prog * p.Prog,
             ExcD = INC.CASES.Prog * p.ExcD,
             ExpD = INC.CASES.Prog * p.ExpD) |>
      dplyr::select(-c("p.Prog", "p.ExcD", "p.ExpD", "p.sum"))
    colnames(df_prev2)
    
    # Make data longer
    df_wide <- df_prev2 |>
      pivot_longer(
        cols = c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD"),
        names_to = "variable",
        values_to = "value"
      ) |>
      pivot_wider(
        names_from = attyear,
        values_from = value
      )
    
    write.csv(df_wide, file="../Data/Cost_04a_prev_dist_Proginc_year.csv", row.names = FALSE)
    
    # List of states
    states <- c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD")
    
    # Break the dataset into various dataframes in a list
    # "Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog", "ExcD", "ExpD"
    df_wide_list <- list()
    for (state in states) {
      df_wide_list[[state]] <- df_wide |>
        filter(variable == state) |>
        dplyr::select(-variable)
    }
    
    # Initialize an empty list to store results
    state_results <- list()
    
    # Loop through each state
    for (state in states) {
      # Extract the wide data frame for the current state
      state_df <- df_wide_list[[state]]
      
      # Replace NAs with 0
      state_df[is.na(state_df)] <- 0
      
      # Group and summarize by SEX and AGE_GROUP
      row_sums_state <- state_df |>
        dplyr::group_by(SEX, AGE_GROUP) |>
        dplyr::select(-SEX, -AGE_GROUP, -YEAR, -INC.CASES.Prog, -time) |>
        summarise(across(everything(), sum, na.rm = TRUE)) |>
        ungroup()
      
      # Create a total row for the state
      total_row <- row_sums_state |>
        summarise(
          SEX = 3,
          AGE_GROUP = "TOTAL",
          across(`2000`:`2030`, sum, na.rm = TRUE)
        )
      
      # Combine the summarized data with the total row
      row_sums_state <- bind_rows(row_sums_state, total_row)
      
      # Add a column for the state
      row_sums_state <- row_sums_state |>
        mutate(state = state)
      
      # Store the result in the list
      state_results[[state]] <- row_sums_state
    }
    
    # Combine all state results into a single data frame
    final_result <- bind_rows(state_results)
    final_result <- final_result |>
      dplyr::select(state, everything()) |>
      mutate(across(where(is.numeric), ~ round(.x, 2)))
    
    final_result_TOTAL_Prog <- subset(final_result, AGE_GROUP == "TOTAL")
    
    final_result_ALIVE_Prog <- subset(final_result_TOTAL_Prog, state %in% c("Tki1", "Tki2", "Tki3", "Sct", "Tfr", "Prog")) |>
      summarise(
        AGE_GROUP = "TOTAL_ALIVE",
        across(`2000`:`2030`, sum, na.rm = TRUE)
      )

##########################################
## Combine the results from CP and Prog ##
##########################################
# Transpose the data for all PIAMOD for later use
piamod_ALL2 <- piamod_ALL |>
               pivot_wider(names_from = YEAR, values_from = PREV.CASES) |>
               mutate(across(`2000`:`2030`, ~ round(.x, 2)))

cols_to_sum <- as.character(2000:2030)

## All = dx at CP + dx at Prog
##########################################    
final_result_TOTAL <- final_result_TOTAL_CP
final_result_ALIVE <- final_result_ALIVE_CP    

# Add cell-by-cell for 2000–2030 columns
final_result_TOTAL[, cols_to_sum] <- final_result_TOTAL_CP[, cols_to_sum] + final_result_TOTAL_Prog[, cols_to_sum]
final_result_ALIVE[, cols_to_sum] <- final_result_ALIVE_CP[, cols_to_sum] + final_result_ALIVE_Prog[, cols_to_sum] 

# I want every number divide by the total number of patients of each year
final_result_TOTAL2 <- final_result_TOTAL |>
  mutate(across(`2000`:`2030`, ~ .x / final_result_ALIVE[[cur_column()]])) |>
  filter(!state %in% c("ExcD", "ExpD"))

# Transpose the data
piamod_ALL2 <- piamod_ALL |>
  pivot_wider(names_from = YEAR, values_from = PREV.CASES) |>
  mutate(across(`2000`:`2030`, ~ round(.x, 2)))

# Ensure the last_row is reshaped correctly for operations
final_result_TOTAL3 <- final_result_TOTAL2 |>
  mutate(across(`2000`:`2030`, ~ .x * piamod_ALL2[[cur_column()]])) 

# Transpose the year columns
final_result_TOTAL4 <- final_result_TOTAL3 |>
  pivot_longer(
    cols = `2000`:`2030`,
    names_to = "YEAR",
    values_to = "PREV.CASES")

# Read stem cell transplantation data to obtain the number of patients with SCT
sct <- readRDS("../Data/Cost_00_sct.rds")
sct_new <- sct |>
      rename(state = sct) |>
      mutate(year = floor(year(date_sct))) |>
      # Count the number of cases per state and year
      group_by(state, year) |>
      summarise(cases = n(), .groups = "drop") |>
      arrange(state, year) |>
      # Create PREV.CASES as the lagged number of cases for each state
      group_by(state) |>
      mutate(PREV.CASES = lag(cases)) |>
      ungroup() |>
      # Select only the required variables
      dplyr::select(state, YEAR = year, PREV.CASES) |>
      mutate(YEAR = as.character(YEAR),
             state = "Sct") 
    
    sct_new2 <- sct_new |>
      filter(YEAR == "2018") |>
      mutate(YEAR = list(as.character(2019:2030))) |>
      unnest(cols = c(YEAR)) |>
      dplyr::select(YEAR, state, PREV.CASES) 
    
    sct_new3 <- bind_rows(sct_new, sct_new2) |>
      mutate(SEX = 3,
             AGE_GROUP = "TOTAL")|>
      rename(PREV.CASES_SCT_1y = PREV.CASES)
    
    
    # View the resulting dataset
    print(sct_new3)
    
    # merge with the final result if state == SCT
    final_result_TOTAL5 <- final_result_TOTAL4 |>
      left_join(sct_new3, by = c("YEAR", "state", "SEX", "AGE_GROUP"))
    
    # Do the deduction to obtain patients who are in the SCT state but 2nd+ year
    final_result_TOTAL6 <- final_result_TOTAL5 |>
      mutate(PREV.CASES = ifelse(state == "Sct", PREV.CASES - PREV.CASES_SCT_1y, PREV.CASES)) |>
      dplyr::select(-PREV.CASES_SCT_1y) |>
      mutate(state = ifelse(state =="Sct", "Sct_2y", state))
    
    # And rowappend
    print(sct_new3)
    sct_new4 <- sct_new3 |>
      mutate(state = "Sct_1y") |>
      rename(PREV.CASES = PREV.CASES_SCT_1y)
    print(sct_new4)
    
    # Append the SCT data to the final result
    final_result_TOTAL7 <- bind_rows(final_result_TOTAL6, sct_new4) |>
      mutate(YEAR = as.numeric(YEAR)) |>
      filter(YEAR >= 2015)|>
      arrange(state)
    
# Save dataset
write.csv(final_result_TOTAL7, file = "../Data/Cost_04a_prev_dist.csv", row.names = FALSE)

