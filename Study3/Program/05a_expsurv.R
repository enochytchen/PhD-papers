## Updated: 2024-01-29 Enoch Chen: use popmort2022 and projected rates instead of popmort2018
##          2023-11-20 Enoch Chen: Redefine the studypop dx 2007-2017 and hence multistate structure changed
##          2023-09-01 Enoch Chen: make the QALE even smaller interval
## Purpose: Estimate life expectancy of disease-free individuals 

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}
 
library(readstata13)
library(survival)
library(dplyr)

## Read data
studypop <- readRDS(file = "../Data/01b_studypop_multistate.rds")
nrow(studypop)

## Read popmort file
popmort <- read.dta13("../Data/00_popmort2022_proj.dta", nonint.factors = TRUE) %>%
           mutate(sex = ifelse(sex==1, 1, 0))


## Original variables _year and _age were not accepted by R
names(popmort) = c("year", "age", "sex", "rate", "prob")

leexp <- NULL
output <- NULL

for (year_temp in c(2007:2017)){
  for (age_temp in seq(45, 85, by = 1)){
    for (sex_temp in c(1, 0)){
  ## Create temp popmort file by increasing year and age
  ## E.g., age 55, year 2007 -> age 56, year 2008   
  popmort_temp <- paste("popmort","age_temp","_","sex_temp",sep = "") 
  popmort_temp <- NULL 
  for (i in c(0:60)){
    x <- popmort[popmort$sex == sex_temp, ]
    x <- x[x$year == year_temp+i, ]
    x <- x[x$age == min(age_temp+i, 105), ]
  popmort_temp <- rbind(popmort_temp, x)
}

## Estimate life expectancy using piecewise-constant popmort
mu = popmort_temp$rate
delta = 0.01
rep = 1/delta
mui = rep(mu, each=rep)
leexp <- tail(cumsum(exp(-cumsum(mui)*delta)*delta),1)

## temp variable for later use in splitting age for applying utility
age_death <- age_temp + leexp
status <- 1 # Just for the split later
df <- data.frame(year_temp, age_temp, sex_temp, leexp, age_death, status)
output <- rbind(output, df)
   }
  }
}

output <- dplyr::mutate(output, id = row_number())

## Obtain the average leexp cross year by age and sex
output_avg <- output %>%
              group_by(year_temp, age_temp, sex_temp) %>%
              summarize(leexp_avg = mean(leexp))

## Prepare survSplit for applying background HSV from Swedish population
output1 <- survSplit(output, cut=c(45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 1e3),
                     start = "age_temp",
                     end = "age_death",
                     event = "status")
output1$lower <- plyr::round_any(output1$age_temp, 5, floor)
output1$sex <- ifelse(output1$sex_temp == 1, 1, 0)
output1 <- output1[,!(names(output1) %in% c("status", "sex_temp"))]

# Backgroud utilities (age-specific utilities from the general pop)
bg_m             <- data.frame(lower=c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95),
                               upper=c(35, 40, 45, 50 ,55, 60, 65, 70, 75, 80, 85, 90, 95, 200),
                               sex = 1,
                               utility = c(0.925, 0.938, 0.930, 0.924, 0.914, 0.910,
                                           0.910, 0.915, 0.909, 0.892, 0.865, 0.831, 0.803, 0.751
                               ))
bg_f             <- data.frame(lower=c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95),
                               upper=c(35, 40, 45, 50 ,55, 60, 65, 70, 75, 80, 85, 90, 95, 200),
                               sex = 0,
                               utility = c(0.902,0.910,0.906,0.904,0.895,0.894,0.899,
                                           0.909,0.899,0.883,0.852,0.814,0.761,0.696
                               ))
bg_utility <- rbind(bg_m, bg_f)
output2 <- left_join(output1, bg_utility, by = c("lower","sex"))

## Calculate LY and QALY 
output2$LY <- output2$age_death - output2$age_temp
output2$QALY <- output2$LY*output2$utility

## Sum up the rows for each self.id
output3 <- output2 %>% 
           group_by(id) %>% 
           dplyr::mutate(QALE=sum(QALY))%>%
           filter(row_number()==1)

output3 <- output3 %>%
           group_by(age_temp, sex) %>% 
           dplyr::mutate(LE_exp=mean(leexp),
                         QALE_exp=mean(QALE)) %>%
           filter(row_number()==1) %>%
           dplyr::select(c("age_temp", "sex", "LE_exp", "QALE_exp"))

colnames(output3)
colnames(output3) <- c("age", "sex", "LE_exp", "QALE_exp")

## Save file
setwd(wd)
saveRDS(output3, file = "../Data/05a_expsurv.rds")
