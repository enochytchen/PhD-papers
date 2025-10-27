## File name: PIAMOD_02b1_NS_fpm.R
## Updated: 2025-02-13 Enoch Chen: fit the models with splines
##          2025-02-03 Enoch Chen: model fitting
## Project: CML Cost
## Purpose: Fit flexible parametric models to predict model-based relative survival
## Input:   PIAMOD_01_analysisdata_cml.dta
## Output:  PIAMOD_02b1_NS_fpm.RData
##          PIAMOD_02b1_NS_fpm.txt

###################
## Read packages ##
###################
library(haven)
library(rstpm2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cuRe)
library(relsurv)

# No exponential notation
options(scipen = 999)

############################
## Read and organize data ##
############################
# CML data
cml <- read_dta("../Data/PIAMOD_01_analysisdata_cml.dta")|>
       mutate(dead = ifelse(dead == 1, 1, 0),
              stime_y = stime,
              stime_d = as.numeric(floor(stime_y*365.241)),
              year = floor(yeardiag + stime_y), # attained year
              age = floor(agediag + stime_y),   # attained age
              age_d = floor(agediag*365.241),   # attained age in days
              sex = ifelse(sex == 1, 1, 2),
              sex_fac = as.factor(ifelse(sex == 1, "male", "female")))

###################################################
## Use general.haz() for getting baseline hazard ##
###################################################
swepop <- transrate.hmd(male = "../Data/lifetable_swe_1y_males.txt",
                        female = "../Data/lifetable_swe_1y_males.txt")

cml$bhaz <- general.haz(time = cml$stime_d,
                        rmap = list(age= "age_d", 
                                    sex= "sex_fac", 
                                    year = "diagdate"),
                        data = cml,
                        ratetable = swepop)

####################################
## Fit flexible parametric models ##
####################################
# Biologically, I assumed there should be main effects of sex, yeardiag, and agediag, and 
# interactions between all of these variables.
# I also assumed that the effects of yeardiag and agediag are non-linear. 
# Consequently, I used splines to model them.
fits <- expand.grid(df_nsx_age = 2:4, df_nsx_year = 2:4, df_tvc = 2:4, df_bhaz = 3:6)
fits$AIC <- NA
mods <- list()

for (i in 1:nrow(fits)) {
  df_nsx_age <- fits$df_nsx_age[i]
  df_nsx_year <- fits$df_nsx_year[i]
  df_tvc <- fits$df_tvc[i]
  df_bhaz <- fits$df_bhaz[i]
  
  fit <- stpm2(Surv(stime_y, dead) ~ sex +                                  # sex 
                                     nsx(yeardiag, df = df_nsx_year) +      # splined yeardiag
                                     nsx(agediag, df = df_nsx_age) +        # splined agediag 
                                     sex:nsx(yeardiag, df = df_nsx_year) +  # interaction between sex and splined yeardiag 
                                     sex:nsx(agediag, df = df_nsx_age) +    # interaction between sex and splined agediag 
                                     nsx(yeardiag, df = df_nsx_year):nsx(agediag, df = df_nsx_age), # interaction splined yeardiag and splined agediag
               tvc = list(sex = df_tvc, 
                          yeardiag = df_tvc,
                          agediag = df_tvc),
               bhazard = "bhaz", df = df_bhaz, data = cml)
  
  fits$AIC[i] <- AIC(fit)
  mods[[i]] <- fit
}

print(fits[which.min(fits$AIC), ])
# The selected model was 2 df splined age, 4 df splined year, 
#                        4 df time-dependent, 4 df baseline hazard

fit <- mods[[52]]
fit

## Save the fitted model
save(fit, file = "../Data/PIAMOD_02b1_NS_fpm.RData")

################
## Prediction ##
################
# Create a data frame for prediction
preddata <- expand.grid(sex = c(1, 2),
                        yeardiag = 1973:2030,
                        agediag = c(9, 20:99),
                        stime_y = seq(0.5, 20, 0.5))

pred_results <- predict(fit, newdata = preddata, type = "surv", se.fit= TRUE) 
preddata$rs <- pred_results$Estimate
preddata$rs.lb <- pred_results$lower
preddata$rs.ub <- pred_results$upper

#############################################################
# Round whatever less than e-10 to e-10
preddata$rs <- round(preddata$rs, 10) 
preddata$rs[preddata$rs < 0.0000000001] <- 0.0000000001
preddata$rs.lb <- round(preddata$rs.lb, 10) 
preddata$rs.lb[preddata$rs.lb < 0.0000000001] <- 0.0000000001
preddata$rs.ub <- round(preddata$rs.ub, 10) 
preddata$rs.ub[preddata$rs.ub < 0.0000000001] <- 0.0000000001

#####################################
## Organize input files for PIAMOD ##
#####################################
head(preddata)
preddata$group <- ifelse(preddata$sex == 1, "SECML_M", "SECML_F")
preddata$yeardiag1 <- preddata$yeardiag
preddata$yeardiag2 <- preddata$yeardiag
preddata$agediag1 <- preddata$agediag
preddata$agediag2 <- preddata$agediag

# If agediag == 9, set agediag1 to 0, and agediag2 to 17
preddata$agediag1[preddata$agediag == 9] <- 0
preddata$agediag2[preddata$agediag == 9] <- 17

# fu_6mo = stime_y *2
preddata$fu_6mo <- preddata$stime_y * 2

preddata2 <- preddata |>
             dplyr::select("group", "sex", "yeardiag1", "yeardiag2", 
                             "agediag1", "agediag2", "fu_6mo", "rs", "rs.lb", "rs.ub") |>
             arrange(sex, yeardiag1, agediag1, fu_6mo)

head(preddata2)

# save as txt without colname
write.table(preddata2, file = "../Data/PIAMOD_02b1_NS_fpm.txt", 
            col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t")
