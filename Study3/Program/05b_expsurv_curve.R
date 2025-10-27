## Updated: 2024-01-29 Enoch Chen: use popmort2022 and projected rates instead of popmort2018
##          2024-01-09 Enoch Chen: Add age 45
##          2023-11-20 Enoch Chen: Redefine the studypop dx 2007-2017 and hence multistate structure changed
##          2023-09-01 Enoch Chen: make the QALE even smaller interval
## Purpose: Estimate life expectancy of disease-free individuals 

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

library(haven)
library(survival)
library(dplyr)

## Read popmort file
popmort <- read.dta13("../Data/00_popmort2022_proj.dta", nonint.factors = TRUE) %>%
           mutate(sex = ifelse(sex==1, 1, 0))

## Original variables _year and _age were not accepted by R
names(popmort) = c("year", "age", "sex", "rate", "prob")
str(popmort)
popmort$sex <- as.numeric(popmort$sex)
class(popmort$sex)

# Convert 'sex' to numeric
popmort$sex <- as.numeric(popmort$sex)

output3 <- NULL

for (year_temp in c(2007:2017)){
  for (age_temp in c(45, 55, 65, 75, 85)){
    for (sex_temp in c(1,0)){
      popmort_temp <- NULL
      for (i in seq(0, 60, by=1)){
        x <- popmort %>%
          filter(sex == sex_temp, 
                 year == year_temp + i, 
                 age == min(age_temp + i, 105))
        
        # Check if 'x' is empty
        if (nrow(x) == 0) {
          print(paste("No data for sex =", sex_temp, 
                      "year =", year_temp + i, 
                      "age =", min(age_temp + i, 105)))
          next  # Skip to the next iteration if 'x' is empty
        }
        
        x$year_temp <- year_temp
        x$flag <- paste(year_temp, age_temp, sex_temp, sep = "_")
        popmort_temp <- rbind(popmort_temp, x)
      }
      
      if (is.null(popmort_temp) || nrow(popmort_temp) == 0) {
        next  # Skip further processing if popmort_temp is empty
      }
      
      output <- NULL
      for (i in 1:nrow(popmort_temp)) {
        # Extract information from the original row
        flag <- popmort_temp$flag[i]
        age  <- popmort_temp$age[i]
        year <- popmort_temp$year[i]
        sex  <- popmort_temp$sex[i]
        rate <- popmort_temp$rate[i]
        prob <- popmort_temp$prob[i]
        # Create 10 rows for each 1-year interval with the same rate
        new_rows <- data.frame(
          flag = rep(flag, 10),
          age = seq(age, age + 0.9, by = 0.1),
          year = seq(year, year + 0.9, by = 0.1),
          sex  = rep(sex, 10),
          rate = rep(rate, 10),
          prob = rep(prob, 10)
        )
        output <- bind_rows(output, new_rows)
      }
      
      mu = popmort_temp$rate
      delta = 0.1
      rep = 1/delta
      mui = rep(mu, each=rep)
      cumprob <- exp(-cumsum(mui)*delta)
      output2 <- cbind(output, cumprob)
      
      ## Set the first row survival to 1
      output2 <- output2 %>%
        group_by(flag) %>% 
        mutate(cumprob = ifelse(row_number() == 1, 1, cumprob)) %>%
        mutate(dxage = first(age)) %>%
        mutate(dxyear = first(year))
      
      output3 <- bind_rows(output3, output2)
    }
  }
}


## Split the age for merging utility values later
output3$age_temp <- floor(output3$age/5) * 5
output3$age_temp <- ifelse(output3$age_temp >= 95, 95 ,output3$age_temp)

# Background utilities (age-specific utilities from the general pop)
bg_m             <- data.frame(age_temp=c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95),
                               sex = 1,
                               utility = c(0.925, 0.938, 0.930, 0.924, 0.914, 0.910,
                                           0.910, 0.915, 0.909, 0.892, 0.865, 0.831, 0.803, 0.751
                               ))
bg_f             <- data.frame(age_temp=c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95),
                               sex = 0,
                               utility = c(0.902,0.910,0.906,0.904,0.895,0.894,0.899,
                                           0.909,0.899,0.883,0.852,0.814,0.761,0.696
                               ))
bg_utility <- rbind(bg_m, bg_f)

output4 <- left_join(output3, bg_utility, by = c("age_temp","sex"))
output4$q_cumprob <- output4$cumprob * output4$utility

## Finally, we average 2007 to 2017
output_45 <- output4 %>%
             filter(dxage==45) %>%
             dplyr::group_by(sex, age) %>%
             dplyr::summarise(cumprob = mean(cumprob),
                               q_cumprob = mean(q_cumprob)) %>%
             dplyr::mutate(dxage=45)

output_55 <- output4 %>%
             filter(dxage==55) %>%
             dplyr::group_by(sex, age) %>%
             dplyr::summarise(cumprob = mean(cumprob),
                       q_cumprob = mean(q_cumprob)) %>%
             dplyr::mutate(dxage=55)

output_65 <- output4 %>%
             filter(dxage==65) %>%
             dplyr::group_by(sex, age) %>%
             dplyr::summarise(cumprob = mean(cumprob),
                              q_cumprob = mean(q_cumprob)) %>%
             dplyr::mutate(dxage=65)

output_75 <- output4 %>%
             filter(dxage==75) %>%
             dplyr::group_by(sex, age) %>%
             dplyr::summarise(cumprob = mean(cumprob),
                              q_cumprob = mean(q_cumprob)) %>%
             dplyr::mutate(dxage=75)

output_85 <- output4 %>%
             filter(dxage==85) %>%
             dplyr::group_by(sex, age) %>%
             dplyr::summarise(cumprob = mean(cumprob),
                             q_cumprob = mean(q_cumprob)) %>%
             dplyr::mutate(dxage=85)

output5 <- data.frame()  # Initialize an empty data frame to store the results
output5 <- rbind(output_45, output_55, output_65, output_75, output_85)  # Append the current iteration results to output3

## Males:1; Females: 0 
output5$sex[output5$sex == 0] <- 0

## Save file
setwd(wd)
getwd()
saveRDS(output5, file = "../Data/05b_expsurv_curve.rds")
