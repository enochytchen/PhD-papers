## Updated: 2024-04-19 Enoch Chen: Rerun model after fixing clockret code
##          2024-01-15 Enoch Chen: update
##          2023-11-24 Enoch Chen: bootstrap the results after only allowing m5, m14 to die from expected only
##          2023-11-16 Enoch Chen: Mark fixed the toRatio errors 
## Purpose: Estimate CIs of (loss of) LE and QALE by parametric bootstrapping

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
library(mvtnorm) # for bootstrapping
library(parallel) 

##########################################
##======================================##
## Run the microsimulation model for CML##
##======================================##
##########################################
source("04_microsim_cml.R") 

## Set Data folder
setwd("../Data")

##########################################
##======================================##
##==========Test bootstrapping==========##
##======================================##
##########################################
## Bootstrap it for model m1
m1star <- m1
m1star@fullcoef <- drop(rmvnorm(1, coef(m1), vcov(m1)))
m1star

## Draw a rate from a rate model
rsurvrate = function(object, n=1){
  exp(rnorm(n, log(object$event), 1/sqrt(object$event)))/object$tstop
}

## Utility ~ beta distribution
## Define a beta distribution
beta_param <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

## beta distribution for sample mean 0,2 with sd 0.2 (variance 0.2^2)
beta_param(0.2, 0.2^2)

## Use rbeta to check whether the function was defined correctly or not
samples <- rbeta(1e5, 0.6, 2.4) ## Draw 1e5 samples with alpha == 12, and beta == 12
mean(samples)
sd(samples)

## Find beta distribution for each health state utility value
## 1L TKI
rbeta(1, beta_param(0.76, 0.21^2)$alpha, beta_param(0.76, 0.21^2)$beta)
## 2L TKI
rbeta(1, beta_param(0.68, 0.28^2)$alpha, beta_param(0.68, 0.28^2)$beta)
## 3L+ TKI
rbeta(1, beta_param(0.68, 0.23^2)$alpha, beta_param(0.68, 0.23^2)$beta)
## TKI discontinuation
rbeta(1, beta_param(0.84, 0.21^2)$alpha, beta_param(0.84, 0.21^2)$beta)
## Progression
rbeta(1, beta_param(0.41, 0.03^2)$alpha, beta_param(0.41, 0.03^2)$beta)
## AlloSCT
rbeta(1, beta_param(0.80, 0.18^2)$alpha, beta_param(0.80, 0.18^2)$beta)

###############################################################
##===========================================================##
## Resample via bootstrapping to obtain confidence intervals ##
##===========================================================##
###############################################################
bootstrap <- function(resample_num, age, sex){
            output0 <- NULL
                # Set seed as system time plus a random number
                seed <- as.integer(as.numeric(Sys.time())) + sample.int(1000, 1)
                set.seed(seed)
                
                # Create the dataset
                ndata <- data.frame(dxage = age, sex = sex)
                
                # Bootstrap
                bootstrap <- for (i in 1:31) {
                  if (i %in% c(3, 5, 6, 7, 12, 13, 14, 15, 17, 18, 19, 22, 
                               23, 24, 25, 26, 27, 28, 29, 30, 31)) {
                    next  # Skip rates or models not in use.
                  }
                  model_name <- paste0("m", i)
                  model <- get(model_name)
                  modified_model <- model
                  modified_model@fullcoef <- drop(rmvnorm(1, coef(model), vcov(model)))
                  
                  # Create individual variables for all the refitted models
                  assign(paste0(model_name, "star"), modified_model, envir = .GlobalEnv)
                }
                
                param <- list(mu0 = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$rate,
                              ages = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$age,
                              partitionBy = 0.1,
                              debug = FALSE,
                              dxage = ndata$dxage,
                              sex = ndata$sex,
                              hsu_tki1 = rbeta(1, beta_param(0.76, 0.21^2)$alpha, beta_param(0.76, 0.21^2)$beta),
                              hsu_tki2 = rbeta(1, beta_param(0.68, 0.28^2)$alpha, beta_param(0.68, 0.28^2)$beta),
                              hsu_tki3 = rbeta(1, beta_param(0.68, 0.23^2)$alpha, beta_param(0.68, 0.23^2)$beta),
                              hsu_prog = rbeta(1, beta_param(0.41, 0.03^2)$alpha, beta_param(0.41, 0.03^2)$beta),
                              hsu_sct = rbeta(1, beta_param(0.80, 0.18^2)$alpha, beta_param(0.80, 0.18^2)$beta),
                              hsu_tfr = rbeta(1, beta_param(0.84, 0.21^2)$alpha, beta_param(0.84, 0.21^2)$beta),
                              m1 = gsm_design(m1star, newdata = ndata),
                              m2 = gsm_design(m2star, newdata = ndata),
                              m3rate  = rsurvrate(m3rate),
                              m4 = gsm_design(m4star, newdata = ndata),
                              ## Transition 5 only expected hazard
                              ## Transition 6 no event
                              m7rate  = rsurvrate(m7rate),
                              m8 = gsm_design(m8star, newdata = ndata),
                              m9 = gsm_design(m9star, newdata = ndata),
                              m10 = gsm_design(m10star, newdata = ndata),
                              m11 = gsm_design(m11star, newdata = ndata),
                              ## Transition 12 only expected hazard
                              m13rate  = rsurvrate(m13rate),
                              ## Transition 14 only expected hazard
                              m15rate  = rsurvrate(m15rate),
                              m16 = gsm_design(m16star, newdata = ndata),
                              m17rate  = rsurvrate(m17rate),
                              ## Transition 18 only expected hazard
                              ## Transition 19 only expected hazard
                              m20 = gsm_design(m20star, newdata = ndata),
                              m21 = gsm_design(m21star, newdata = ndata),
                              m22rate  = rsurvrate(m22rate),
                              ## Transition 23 only expected hazard
                              m24rate  = rsurvrate(m24rate),
                              m25rate  = rsurvrate(m25rate),
                              ## Transition 26 only expected hazard
                              m27rate  = rsurvrate(m27rate),
                              m28rate  = rsurvrate(m28rate),
                              ## Transition 29 only expected hazard
                              m30rate  = rsurvrate(m30rate)
                              ## Transition 31 only expected hazard
                )

                # Call the simulation again
                sim <- simulations(1e5, param, indivp = FALSE)
                
                # Calculate the desired value for the current resample
                LE <- sum(subset(sim$pt, ! state %in% c("AcD", "ExcD", "ExpD"))$pt) / sim$n
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
                QALE <- sum(sim$ut$utility) / sim$n
                QLoS_Tfr <- (sum(subset(sim$ut, state %in% c("Tfr1", "Tfr2", "Tfr3"))$ut)) / sim$n
                QLoS_Prog <- (sum(subset(sim$ut, state %in% c("Prog"))$ut)) / sim$n
                QLoS_Sct  <- (sum(subset(sim$ut, state %in% c("Sct"))$ut)) / sim$n
                QLoS_Tki1 <- (sum(subset(sim$ut, state %in% c("Tki1", "Tki1afterTfr"))$ut)) / sim$n
                QLoS_Tki2 <- (sum(subset(sim$ut, state %in% c("Tki2", "Tki2afterTfr"))$ut)) / sim$n
                QLoS_Tki3 <- (sum(subset(sim$ut, state %in% c("Tki3", "Tki3afterTfr"))$ut)) / sim$n
                
                # Create a new data frame to store the results
                resample_data <- data.frame(seed = seed, resample_num = resample_num, age = age, sex = sex, 
                                            LE = LE, LoS_Tfr = LoS_Tfr, LoS_Prog = LoS_Prog, 
                                                     LoS_Sct = LoS_Sct, LoS_Tki1 = LoS_Tki1, LoS_Tki2 = LoS_Tki2, LoS_Tki3 = LoS_Tki3,
                                                     LoS_ExcD = LoS_ExcD, LoS_ExpD = LoS_ExpD, LoS_AcD = LoS_AcD, 
                                            QALE = QALE, QLoS_Tfr=QLoS_Tfr, QLoS_Prog=QLoS_Prog, 
                                                     QLoS_Sct = QLoS_Sct, QLoS_Tki1 = QLoS_Tki1, QLoS_Tki2 = QLoS_Tki2, QLoS_Tki3 = QLoS_Tki3)

                # Combine all resampled data frames into one
                output0 <- rbind(output0, resample_data)
                return(output0)
}

## Testing
system.time(bootstrap(1, 45, 1))

############################################################
# Perform bootstrapping using parallel processing (mclapply)
results <- list()
results <- mclapply(1:1000, function(i) {                    ## m = 1000
            lapply(c(45, 55, 65, 75, 85), function(j) {    ## five selected ages 45 55 65 75 85
            lapply(c(1,0) , function(k) {                  ## 2 sexes
              suppressWarnings(bootstrap(i, j, k))
            })
          })
        }, mc.cores = 10)

# Convert lists to a data frame
bootstrap_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) do.call(rbind, y)))))

save(bootstrap_df, file = "06_bootstrap_df.RData")

##########################################################
setwd("../Data")
## Load the estimate results
load("06_estimates_df.RData")

## Load the bootstrapped results
load("06_bootstrap_df.RData")

## Obtain 95% CIs from the bootstrapped results
bootstrap_df <- bootstrap_df[, c("age", "sex", "LE", "QALE")]
bootstrap_df <- bootstrap_df %>%
                dplyr::group_by(age, sex)%>%
                dplyr::mutate(LE_lower = quantile(LE, 0.025),
                              LE_upper = quantile(LE, 0.975),
                              QALE_lower = quantile(QALE, 0.025),
                              QALE_upper = quantile(QALE, 0.975)) %>%
                filter(row_number()==1)
  
bootstrap_df <- bootstrap_df[, c("age", "sex", "LE_lower", "LE_upper", "QALE_lower", "QALE_upper")]

## Merge the datasets
merged_df <- merge(estimates_df, bootstrap_df, by = c("age", "sex"))

## Read expected LE and QALE of the general pop
wd_data = "../Data"
setwd(wd_data)
expsurv<- readRDS("05a_expsurv.rds")

## Calculate LLE and LQALE
output  <- merged_df %>%
           left_join(expsurv, by = c("age", "sex")) %>%
           mutate(LLE_mean = LE_exp - LE_mean,
                  LLE_lower = LE_exp - LE_upper,
                  LLE_upper = LE_exp - LE_lower,
                  LQALE_mean = QALE_exp - QALE_mean,
                  LQALE_lower = QALE_exp - QALE_upper,
                  LQALE_upper = QALE_exp - QALE_lower)%>%
           mutate_at(c("LE_exp", "LE_mean", "LE_lower", "LE_upper",
                        "LLE_mean", "LLE_lower", "LLE_upper",
                        "QALE_exp", "QALE_mean", "QALE_lower", "QALE_upper",
                        "LQALE_mean", "LQALE_lower", "LQALE_upper",
                        "LoS_Tfr", "LoS_Prog", "LoS_Sct", "LoS_Tki1", "LoS_Tki2", "LoS_Tki3",
                        "LoS_ExcD", "LoS_ExpD", "LoS_AcD",
                        "QLoS_Tfr", "QLoS_Prog", "QLoS_Sct", "QLoS_Tki1", "QLoS_Tki2", "QLoS_Tki3"), funs(round(., 1))) %>%
           mutate(PLLE_mean = LLE_mean/LE_exp,
                  PLLE_lower = LLE_lower/LE_exp,
                  PLLE_upper = LLE_upper/LE_exp,
                  PLQALE_mean = LQALE_mean/QALE_exp,
                  PLQALE_lower = LQALE_lower/QALE_exp,
                  PLQALE_upper = LQALE_upper/QALE_exp) %>%
           mutate_at(c("PLLE_mean", "PLLE_lower", "PLLE_upper",
                       "PLQALE_mean", "PLQALE_lower", "PLQALE_upper"), funs(round(., 2)))

output <- output%>%
           dplyr::select(c("age", "sex", "LE_exp", "LE_mean", "LE_lower", "LE_upper",
                            "LLE_mean", "LLE_lower", "LLE_upper",
                            "QALE_exp", "QALE_mean", "QALE_lower", "QALE_upper",
                            "LQALE_mean", "LQALE_lower", "LQALE_upper", 
                            "PLLE_mean", "PLLE_lower", "PLLE_upper",
                            "PLQALE_mean", "PLQALE_lower", "PLQALE_upper",
                             "LoS_Tfr", "LoS_Prog", "LoS_Sct", "LoS_Tki1", "LoS_Tki2", "LoS_Tki3",
                             "LoS_ExcD", "LoS_ExpD", "LoS_AcD",
                             "QLoS_Tfr", "QLoS_Prog", "QLoS_Sct", "QLoS_Tki1", "QLoS_Tki2", "QLoS_Tki3"
            ))%>%
            arrange(desc(sex), age)

setwd(wd_data)
saveRDS(output, file = "06_bootstrap_results.rds")

