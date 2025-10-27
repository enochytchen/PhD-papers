## Updated: 2024-05-31 Enoch Chen: update syntaxes. 
##                                 For all transitions not leading to death, take an average rate
##                                 For all transitions leading to death, FPMs
##                                 Clearly assess whether FPRSMs fit or not
##          2024-04-19 Enoch Chen: Rerun model after fixing clockret code
##          2024-02-21 Enoch Chen: separate estimates as one single R file
## Purpose: Estimate LE and QALE 

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

## For Mark Clements only
if (Sys.getenv("USER")=="marcle") {
  if (Sys.getenv("HOSTNAME") == "vector.meb.ki.se")
    setwd("/cifs/P/SweCan_CML/Enoch_PhD/Study3_QALY/Program")
  else setwd("/media/P/SweCan_CML/Enoch_PhD/Study3_QALY/Program")
}

###################
##===============##
## Read packages ##
##===============##
###################
library(dplyr)
library(parallel) 

###########################################
##====================================== ##
## Run the microsimulation model for CML ##
##====================================== ##
###########################################
source("04_microsim_cml.R") 

################################
##============================##
##==========Estimate==========##
##============================##
################################
output0 <- data.frame()

est <-  function(age, sex){
  
        output0 <- NULL
        ## Set seed
        set.seed(12345)
        ## Create the dataset
        ndata <- data.frame(dxage = age, sex = sex)
        ## Define parameter
        param <- list(mu0 = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$rate,
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
                      m3rate  = m3rate$rate,
                      m4 = gsm_design(m4, newdata = ndata),
                      ## Transition 5 only expected hazard
                      ## Transition 6 no event
                      m7rate  = m7rate$rate,
                      m8 = gsm_design(m8, newdata = ndata),
                      m9 = gsm_design(m9, newdata = ndata),
                      m10 = gsm_design(m10, newdata = ndata),
                      m11 = gsm_design(m11, newdata = ndata),
                      ## Transition 12 only expected hazard
                      m13rate  = m13rate$rate,
                      ## Transition 14 only expected hazard
                      m15rate  = m15rate$rate,
                      m16 = gsm_design(m16, newdata = ndata),
                      m17rate  = m17rate$rate,
                      ## Transition 18 only expected hazard
                      ## Transition 19 only expected hazard
                      m20 = gsm_design(m20, newdata = ndata),
                      m21 = gsm_design(m21, newdata = ndata),
                      m22rate  = m22rate$rate,
                      ## Transition 23 only expected hazard
                      m24rate  = m24rate$rate,
                      m25rate  = m25rate$rate,
                      ## Transition 26 only expected hazard
                      m27rate  = m27rate$rate,
                      m28rate  = m28rate$rate,
                      ## Transition 29 only expected hazard
                      m30rate  = m30rate$rate
                      ## Transition 31 only expected hazard
        )
  sim <- simulations(1e6, param, indivp = FALSE)
  
  # Calculate the desired value for the current resample
  LE_mean <- sum(subset(sim$pt, ! state %in% c("AcD", "ExcD", "ExpD"))$pt) / sim$n
  LoS_Tfr  <- (sum(subset(sim$pt, state %in% c("Tfr1", "Tfr2", "Tfr3"))$pt)) / sim$n
  LoS_Prog <- (sum(subset(sim$pt, state %in% c("Prog"))$pt)) / sim$n
  LoS_Sct  <- (sum(subset(sim$pt, state %in% c("Sct"))$pt)) / sim$n
  LoS_Tki1 <- (sum(subset(sim$pt, state %in% c("Tki1", "Tki1afterTfr"))$pt)) / sim$n
  LoS_Tki2 <- (sum(subset(sim$pt, state %in% c("Tki2", "Tki2afterTfr"))$pt)) / sim$n
  LoS_Tki3 <- (sum(subset(sim$pt, state %in% c("Tki3", "Tki3afterTfr"))$pt)) / sim$n
  LoS_ExcD <- (sum(subset(sim$pt, state %in% c("ExcD"))$pt)) / sim$n
  LoS_ExpD <- (sum(subset(sim$pt, state %in% c("ExpD"))$pt)) / sim$n
  LoS_AcD <- (sum(subset(sim$pt, state %in% c("AcD"))$pt)) / sim$n
  
  ## Calculate QALE and quality-adjusted length of stay (QLoS) 
  QALE_mean <- sum(sim$ut$utility) / sim$n
  QLoS_Tfr <- (sum(subset(sim$ut, state %in% c("Tfr1", "Tfr2", "Tfr3"))$ut)) / sim$n
  QLoS_Prog <- (sum(subset(sim$ut, state %in% c("Prog"))$ut)) / sim$n
  QLoS_Sct  <- (sum(subset(sim$ut, state %in% c("Sct"))$ut)) / sim$n
  QLoS_Tki1 <- (sum(subset(sim$ut, state %in% c("Tki1", "Tki1afterTfr"))$ut)) / sim$n
  QLoS_Tki2 <- (sum(subset(sim$ut, state %in% c("Tki2", "Tki2afterTfr"))$ut)) / sim$n
  QLoS_Tki3 <- (sum(subset(sim$ut, state %in% c("Tki3", "Tki3afterTfr"))$ut)) / sim$n
  
  # Create a new data frame to store the results
  estimate_data <- data.frame(age = age, sex = sex, 
                              LE_mean = LE_mean, LoS_Tfr = LoS_Tfr, LoS_Prog = LoS_Prog, 
                              LoS_Sct = LoS_Sct, LoS_Tki1 = LoS_Tki1, LoS_Tki2 = LoS_Tki2, LoS_Tki3 = LoS_Tki3,
                              LoS_ExcD = LoS_ExcD, LoS_ExpD = LoS_ExpD, LoS_AcD = LoS_AcD, 
                              QALE_mean = QALE_mean, QLoS_Tfr=QLoS_Tfr, QLoS_Prog=QLoS_Prog, 
                              QLoS_Sct = QLoS_Sct, QLoS_Tki1 = QLoS_Tki1, QLoS_Tki2 = QLoS_Tki2, QLoS_Tki3 = QLoS_Tki3)
  
  # Combine all resampled data frames into one
  output0 <- rbind(output0, estimate_data)
  return(output0)
}

estimates <- mclapply(c(45, 55, 65, 75, 85), function(j) {  # selected ages 45(10)85
  lapply(c(1,0) , function(k) { # 2 sexes
    est(j, k)
  })
}, mc.cores = 10)

estimates_df <- do.call(rbind, lapply(estimates, function(x) do.call(rbind, x)))

save(estimates_df, file = "../Data/06_estimates_df.RData")

