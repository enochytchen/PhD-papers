## File name: PIAMOD_05a1_prev_CIs.R
## Updated: 
## Project: CML Cost
## Purpose: Estimate confidence intervals for prevalence cases and rates

# Set seed
set.seed(1314520)
epsilon <- 1e-2 # for prevalence gradient

###################
## Read packages ##
###################
library("dplyr")
library("readr")
library("tinyplot")
library("ggplot2")
library("readxl")
library("patchwork")

##############################################
## Run APC modelling to obtain vcov martrix ##
##############################################
source("PIAMOD_04a_inc_glm.R") 

###########
## Males ##
###########

# Read mean population
meanpop_m <- read.csv("../Data/PIAMOD_03_piamod_constantRS_dynINC.csv",
                      header = TRUE) |>
             filter(SEX == 1 & AGE_GROUP == "0-99") |>
             dplyr::select(YEAR, MEANPOP)

# Estimate
prev_res_m <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_prev/old_constantRS_dynINC_M_crude.csv",
                       header = TRUE) |>
              as.matrix()

std_prev_res_m <-read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_prev/old_constantRS_dynINC_M_std.csv",
                        header = TRUE)|>
              as.matrix()

inc_res_m <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_incidence/old_dynINC_M_crude.csv", 
                      header = TRUE) |>
             as.matrix()

std_inc_res_m <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_incidence/old_dynINC_M_std.csv", 
                      header = TRUE) |>
             as.matrix()

# Uncertainty
prev_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_prev/old_constantRS_dynINC_M_crude_plus.csv",
                      header = TRUE)|>
             dplyr::select(-Crude)|>
             as.matrix()

prev_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_prev/old_constantRS_dynINC_M_crude_minus.csv",
                       header = TRUE)|>
              dplyr::select(-Crude)|>
              as.matrix()

std_prev_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_prev/old_constantRS_dynINC_M_std_plus.csv",
                          header = TRUE)|>
                 dplyr::select(-Std)|>  
                 as.matrix()

std_prev_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_prev/old_constantRS_dynINC_M_std_minus.csv",
                           header = TRUE)|>
                  dplyr::select(-Std)|>
                  as.matrix()

inc_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_incidence/old_dynINC_M_crude_plus.csv",
                      header = TRUE)|>
            dplyr::select(-Crude)|>
            as.matrix()

inc_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_incidence/old_dynINC_M_crude_minus.csv",
                       header = TRUE)|>
              dplyr::select(-Crude)|>
              as.matrix()

std_inc_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_incidence/old_dynINC_M_std_plus.csv",
                          header = TRUE)|>
                 dplyr::select(-Std)|>  
                 as.matrix()

std_inc_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_incidence/old_dynINC_M_std_minus.csv",
                           header = TRUE)|>
                  dplyr::select(-Std)|>
                  as.matrix()

## Calculate gradient of prevalence ----
prev_males <- data.frame(year = prev_res_m[,1], 
                         EXP.PREVAL. = prev_res_m[,2],
                         EXP.PREVAL.lb = prev_res_m[,2],
                         EXP.PREVAL.ub = prev_res_m[,2],
                         sex="Males")
prev_grad <- matrix(0, nrow=1,ncol = dim(prev_res_m)[1]) %>%
  as.data.frame()

std_prev_males <- data.frame(year = std_prev_res_m[,1], 
                             STD.PREV. = std_prev_res_m[,2],
                             STD.PREV.lb = std_prev_res_m[,2],
                             STD.PREV.ub = std_prev_res_m[,2],
                             sex="Males")
std_prev_grad <- matrix(0, nrow=1,ncol = dim(std_prev_res_m)[1]) %>%
  as.data.frame()

inc_males <- data.frame(year = inc_res_m[,1], 
                        exp.inc = inc_res_m[,2],
                        exp.inc.low = inc_res_m[,2],
                        exp.inc.up = inc_res_m[,2],
                        sex="Males")
inc_grad <- matrix(0, nrow=1,ncol = dim(inc_res_m)[1]) %>%
  as.data.frame()

std_inc_males <- data.frame(year = std_inc_res_m[,1], 
                            exp.inc = std_inc_res_m[,2],
                            exp.inc.low = std_inc_res_m[,2],
                            exp.inc.up = std_inc_res_m[,2],
                            sex="Males")
std_inc_grad <- matrix(0, nrow=1,ncol = dim(std_inc_res_m)[1]) %>%
  as.data.frame()

for(k in c(2:8)){
  # prevalence
  fab_plus<-prev_plus[,k]
  fab_minus<-prev_minus[,k]
  prev_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
  
  # standardized prevalence
  fab_plus<-std_prev_plus[,k]
  fab_minus<-std_prev_minus[,k]
  std_prev_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
  
  # incidence
  fab_plus<-inc_plus[,k]
  fab_minus<-inc_minus[,k]
  inc_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
  
  # standardized incidence
  fab_plus<-std_inc_plus[,k]
  fab_minus<-std_inc_minus[,k]
  std_inc_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
}

# prev_grad should be a p × 58 matrix.
# t(prev_grad) will then be 58 × p
# sigma_apc must be a p × p matrix.

prev_grad <- as.matrix(prev_grad)
var_prev_males <-  diag(t(prev_grad)%*%sigma_apc_m%*%prev_grad)

std_prev_grad <- as.matrix(std_prev_grad)
var_std_prev_males<-  diag(t(std_prev_grad)%*%sigma_apc_m%*%std_prev_grad)

inc_grad <- as.matrix(inc_grad)
var_inc_males <-  diag(t(inc_grad)%*%sigma_apc_m%*%inc_grad)

std_inc_grad <- as.matrix(std_inc_grad)
var_std_inc_males<-  diag(t(std_inc_grad)%*%sigma_apc_m%*%std_inc_grad)

# log transform: delta method
log_prev <- log(prev_res_m[,2])
var_log_prev <- ((1/prev_res_m[,2])^2)*var_prev_males

log_std_prev <- log(std_prev_res_m[,2])
var_log_std_prev <- ((1/std_prev_res_m[,2])^2)*var_std_prev_males

log_inc <- log(inc_res_m[,2])
var_log_inc <- ((1/inc_res_m[,2])^2)*var_inc_males

log_std_inc <- log(std_inc_res_m[,2])
var_log_std_inc <- ((1/std_inc_res_m[,2])^2)*var_std_inc_males

# confidence intervals
log_prev_low <- log_prev - qnorm(.975) * sqrt(var_log_prev) 
log_prev_up <- log_prev + qnorm(.975) * sqrt(var_log_prev) 
prev_males$EXP.PREVAL.lb <- exp(log_prev_low)
prev_males$EXP.PREVAL.ub <- exp(log_prev_up)
prev_males$EXP.PREVAL.S.E. <- exp(var_log_prev)
prev_males <- left_join(prev_males, meanpop_m, by = c("year" = "YEAR"))
prev_males$PREV.CASES <- prev_males$EXP.PREVAL.* prev_males$MEANPOP/100000
prev_males$PREV.CASES.lb <- prev_males$EXP.PREVAL.lb * prev_males$MEANPOP/100000
prev_males$PREV.CASES.ub <- prev_males$EXP.PREVAL.ub * prev_males$MEANPOP/100000

log_std_prev_low <- log_std_prev - qnorm(.975) * sqrt(var_log_std_prev) 
log_std_prev_up <- log_std_prev + qnorm(.975) * sqrt(var_log_std_prev) 
std_prev_males$STD.PREV.lb <- exp(log_std_prev_low)
std_prev_males$STD.PREV.ub <- exp(log_std_prev_up)
std_prev_males$STD.PREVAL.S.E. <- exp(var_log_std_prev)
std_prev_males <- left_join(std_prev_males, meanpop_m, by = c("year" = "YEAR"))
std_prev_males$STD.PREV.CASES <- std_prev_males$STD.PREV.* std_prev_males$MEANPOP/100000
std_prev_males$STD.PREV.CASES.lb <- std_prev_males$STD.PREV.lb * std_prev_males$MEANPOP/100000
std_prev_males$STD.PREV.CASES.ub <- std_prev_males$STD.PREV.ub * std_prev_males$MEANPOP/100000

log_inc_low <- log_inc - qnorm(.975) * sqrt(var_log_inc) 
log_inc_up <- log_inc + qnorm(.975) * sqrt(var_log_inc) 
inc_males$exp.inc.low <- exp(log_inc_low)
inc_males$exp.inc.up <- exp(log_inc_up)
inc_males <- left_join(inc_males, meanpop_m, by = c("year" = "YEAR"))
inc_males$inc.CASES <- inc_males$exp.inc* inc_males$MEANPOP/100000
inc_males$inc.CASES.lb <- inc_males$exp.inc.low * inc_males$MEANPOP/100000
inc_males$inc.CASES.ub <- inc_males$exp.inc.up * inc_males$MEANPOP/100000


log_std_inc_low <- log_std_inc - qnorm(.975) * sqrt(var_log_std_inc) 
log_std_inc_up <- log_std_inc + qnorm(.975) * sqrt(var_log_std_inc) 
std_inc_males$exp.inc.low <- exp(log_std_inc_low)
std_inc_males$exp.inc.up <- exp(log_std_inc_up)
std_inc_males <- left_join(std_inc_males, meanpop_m, by = c("year" = "YEAR"))
std_inc_males$inc.CASES <- std_inc_males$exp.inc* std_inc_males$MEANPOP/100000
std_inc_males$inc.CASES.lb <- std_inc_males$exp.inc.low * std_inc_males$MEANPOP/100000
std_inc_males$inc.CASES.ub <- std_inc_males$exp.inc.up * std_inc_males$MEANPOP/100000

colnames(prev_males)
colnames(std_prev_males)

merged_data <- merge(prev_males, std_prev_males, by = c("year", "sex", "MEANPOP"))

## Save as txt
write.table(merged_data, file = "../Data/PIAMOD_05a1_prev_CIs_m.txt", sep = "\t")

#############
## Females ##
#############
# Read mean population
meanpop_f <- read.csv("../Data/PIAMOD_03_piamod_constantRS_dynINC.csv",
                      header = TRUE) |>
             filter(SEX == 2 & AGE_GROUP == "0-99") |>
             dplyr::select(YEAR, MEANPOP)

# Estimate
prev_res_f <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_prev/old_constantRS_dynINC_F_crude.csv",
                       header = TRUE) |>
              as.matrix()

std_prev_res_f <-read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_prev/old_constantRS_dynINC_F_std.csv",
                          header = TRUE)|>
                  as.matrix()

inc_res_f <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_incidence/old_dynINC_F_crude.csv", 
                      header = TRUE) |>
  as.matrix()

std_inc_res_f <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_incidence/old_dynINC_F_std.csv", 
                          header = TRUE) |>
  as.matrix()

# Uncertainty
prev_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_prev/old_constantRS_dynINC_F_crude_plus.csv",
                      header = TRUE)|>
             dplyr::select(-Crude)|>
             as.matrix()

prev_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_prev/old_constantRS_dynINC_F_crude_minus.csv",
                       header = TRUE)|>
              dplyr::select(-Crude)|>
              as.matrix()

std_prev_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_prev/old_constantRS_dynINC_F_std_plus.csv",
                          header = TRUE)|>
  dplyr::select(-Std)|>  
  as.matrix()

std_prev_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_prev/old_constantRS_dynINC_F_std_minus.csv",
                           header = TRUE)|>
  dplyr::select(-Std)|>
  as.matrix()

inc_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_incidence/old_dynINC_F_crude_plus.csv",
                     header = TRUE)|>
  dplyr::select(-Crude)|>
  as.matrix()

inc_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/crude_incidence/old_dynINC_F_crude_minus.csv",
                      header = TRUE)|>
  dplyr::select(-Crude)|>
  as.matrix()

std_inc_plus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_incidence/old_dynINC_F_std_plus.csv",
                         header = TRUE)|>
  dplyr::select(-Std)|>  
  as.matrix()

std_inc_minus <- read.csv("../Output/PIAMOD_Output/PIAMOD_param/std_incidence/old_dynINC_F_std_minus.csv",
                          header = TRUE)|>
  dplyr::select(-Std)|>
  as.matrix()

## Calculate gradient of prevalence ----
prev_females <- data.frame(year = prev_res_f[,1], 
                           EXP.PREVAL. = prev_res_f[,2],
                           EXP.PREVAL.lb = prev_res_f[,2],
                           EXP.PREVAL.ub = prev_res_f[,2],
                           sex="Females")
prev_grad <- matrix(0, nrow=1,ncol = dim(prev_res_f)[1]) %>%
  as.data.frame()

std_prev_females <- data.frame(year = std_prev_res_f[,1], 
                               STD.PREV. = std_prev_res_f[,2],
                               STD.PREV.lb = std_prev_res_f[,2],
                               STD.PREV.ub = std_prev_res_f[,2],
                             sex="Females")
std_prev_grad <- matrix(0, nrow=1,ncol = dim(std_prev_res_f)[1]) %>%
  as.data.frame()

inc_females <- data.frame(year = inc_res_f[,1], 
                        exp.inc = inc_res_f[,2],
                        exp.inc.low = inc_res_f[,2],
                        exp.inc.up = inc_res_f[,2],
                        sex="Females")
inc_grad <- matrix(0, nrow=1,ncol = dim(inc_res_f)[1]) %>%
  as.data.frame()

std_inc_females <- data.frame(year = std_inc_res_f[,1], 
                            exp.inc = std_inc_res_f[,2],
                            exp.inc.low = std_inc_res_f[,2],
                            exp.inc.up = std_inc_res_f[,2],
                            sex="Females")
std_inc_grad <- matrix(0, nrow=1,ncol = dim(std_inc_res_f)[1]) %>%
  as.data.frame()

for(k in c(2:8)){
  # prevalence
  fab_plus<-prev_plus[,k]
  fab_minus<-prev_minus[,k]
  prev_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
  
  # standardized prevalence
  fab_plus<-std_prev_plus[,k]
  fab_minus<-std_prev_minus[,k]
  std_prev_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
  
  # incidence
  fab_plus<-inc_plus[,k]
  fab_minus<-inc_minus[,k]
  inc_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
  
  # standardized incidence
  fab_plus<-std_inc_plus[,k]
  fab_minus<-std_inc_minus[,k]
  std_inc_grad[(k-1),] <- (fab_plus-fab_minus)/(2*epsilon)
}

# prev_grad should be a p × 58 matrix.
# t(prev_grad) will then be 58 × p
# sigma_apc must be a p × p matrix.

prev_grad <- as.matrix(prev_grad)
var_prev_females <-  diag(t(prev_grad)%*%sigma_apc_f%*%prev_grad)

std_prev_grad <- as.matrix(std_prev_grad)
var_std_prev_females<-  diag(t(std_prev_grad)%*%sigma_apc_f%*%std_prev_grad)

inc_grad <- as.matrix(inc_grad)
var_inc_females <-  diag(t(inc_grad)%*%sigma_apc_f%*%inc_grad)

std_inc_grad <- as.matrix(std_inc_grad)
var_std_inc_females<-  diag(t(std_inc_grad)%*%sigma_apc_f%*%std_inc_grad)

# log transform: delta method
log_prev <- log(prev_res_f[,2])
var_log_prev <- ((1/prev_res_f[,2])^2)*var_prev_females

log_std_prev <- log(std_prev_res_f[,2])
var_log_std_prev <- ((1/std_prev_res_f[,2])^2)*var_std_prev_females

log_inc <- log(inc_res_f[,2])
var_log_inc <- ((1/inc_res_f[,2])^2)*var_inc_females

log_std_inc <- log(std_inc_res_f[,2])
var_log_std_inc <- ((1/std_inc_res_f[,2])^2)*var_std_inc_females

# confidence intervals
log_prev_low <- log_prev - qnorm(.975) * sqrt(var_log_prev) 
log_prev_up <- log_prev + qnorm(.975) * sqrt(var_log_prev) 
prev_females$EXP.PREVAL.lb <- exp(log_prev_low)
prev_females$EXP.PREVAL.ub <- exp(log_prev_up)
prev_females$EXP.PREVAL.S.E. <- exp(var_log_prev)
prev_females <- left_join(prev_females, meanpop_f, by = c("year" = "YEAR"))
prev_females$PREV.CASES <- prev_females$EXP.PREVAL.* prev_females$MEANPOP/100000
prev_females$PREV.CASES.lb <- prev_females$EXP.PREVAL.lb * prev_females$MEANPOP/100000
prev_females$PREV.CASES.ub <- prev_females$EXP.PREVAL.ub * prev_females$MEANPOP/100000



log_std_prev_low <- log_std_prev - qnorm(.975) * sqrt(var_log_std_prev) 
log_std_prev_up <- log_std_prev + qnorm(.975) * sqrt(var_log_std_prev) 
std_prev_females$STD.PREV.lb <- exp(log_std_prev_low)
std_prev_females$STD.PREV.ub <- exp(log_std_prev_up)
std_prev_females$STD.PREVAL.S.E. <- exp(var_log_std_prev)
std_prev_females <- left_join(std_prev_females, meanpop_f, by = c("year" = "YEAR"))
std_prev_females$STD.PREV.CASES <- std_prev_females$STD.PREV.* std_prev_females$MEANPOP/100000
std_prev_females$STD.PREV.CASES.lb <- std_prev_females$STD.PREV.lb * std_prev_females$MEANPOP/100000
std_prev_females$STD.PREV.CASES.ub <- std_prev_females$STD.PREV.ub * std_prev_females$MEANPOP/100000

log_inc_low <- log_inc - qnorm(.975) * sqrt(var_log_inc) 
log_inc_up <- log_inc + qnorm(.975) * sqrt(var_log_inc) 
inc_females$exp.inc.low <- exp(log_inc_low)
inc_females$exp.inc.up <- exp(log_inc_up)
inc_females <- left_join(inc_females, meanpop_f, by = c("year" = "YEAR"))
inc_females$inc.CASES <- inc_females$exp.inc* inc_females$MEANPOP/100000
inc_females$inc.CASES.lb <- inc_females$exp.inc.low * inc_females$MEANPOP/100000
inc_females$inc.CASES.ub <- inc_females$exp.inc.up * inc_females$MEANPOP/100000


log_std_inc_low <- log_std_inc - qnorm(.975) * sqrt(var_log_std_inc) 
log_std_inc_up <- log_std_inc + qnorm(.975) * sqrt(var_log_std_inc) 
std_inc_females$exp.inc.low <- exp(log_std_inc_low)
std_inc_females$exp.inc.up <- exp(log_std_inc_up)
std_inc_females <- left_join(std_inc_females, meanpop_f, by = c("year" = "YEAR"))
std_inc_females$inc.CASES <- std_inc_females$exp.inc* std_inc_females$MEANPOP/100000
std_inc_females$inc.CASES.lb <- std_inc_females$exp.inc.low * std_inc_females$MEANPOP/100000
std_inc_females$inc.CASES.ub <- std_inc_females$exp.inc.up * std_inc_females$MEANPOP/100000

merged_data <- merge(prev_females, std_prev_females, by = c("year", "sex", "MEANPOP"))

## Save as txt
write.table(merged_data, file = "../Data/PIAMOD_05a1_prev_CIs_f.txt", sep = "\t")
