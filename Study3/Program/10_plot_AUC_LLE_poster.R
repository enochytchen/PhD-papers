## Updated: 2024-04-19 Enoch Chen: Plot the fig after fixing clock-ret code
##          2023-11-20 Enoch Chen: Redefine the studypop dx 2007-2017 and hence multistate structure changed
##          2023-09-30 Enoch Chen: refitt models after reducing the total states (8->6)
## Purpose: Plot length of stay and QALY by state
## Notes: Need to fix the legend. Also show expected survival and quality-adjusted expected survival
 
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

## set seeds
set.seed(12345)

##########################################
##======================================##
## Run the microsimulation model for CML##
##======================================##
##########################################
source("04_microsim_cml.R") 

## Read expected LE and QALE of the general pop
wd_data = "../Data"
setwd(wd_data)
expsurv<- readRDS("05a_expsurv.rds")

##########################################
##======================================##
##===========Read packages==============##
##======================================##
##########################################
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggh4x)
library(parallel) 

##########################################
##======================================##
##======Read expsurv_curve.rds===========##
##======================================##
##########################################
## Expected survival curve and quality-adjusted expected survival curve
expsurv_curve<- readRDS("../Data/05b_expsurv_curve.rds")
expsurv_curve$time <- expsurv_curve$age - expsurv_curve$dxage
expsurv_curve$age_byime <- expsurv_curve$age
expsurv_curve <- subset(expsurv_curve, select = -age)

###########################
##=======================##
## Read the results data ##
##=======================##
###########################
results <- readRDS("../Data/06_bootstrap_results.rds")
results <- results[results$age %in% c(45, 55, 65, 75, 85), ]
males   <- results[results$sex == 1, ]
females <- results[results$sex == 0, ]

##################################
##==============================##
##==========Plots===============##
##==============================##
##################################
## For ages 45 55 65 75 85; sexes 1=Males, 0=Females
ages <- c(65)
sexes <- c(0)

for (age in ages){
  sim <- mclapply(sexes, function(sex) {
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
    sim <- simulations(1e5, param, indivp = FALSE)
  }, mc.cores = 5)
  
# Access the sim for s = 1 and s = 0
sim_f <- sim[[1]]

#########################################################
# prev means no. of patients in that stay
calculateAndPlot_prev <- function(sim) {
  
  LE <- sum(subset(sim$pt, ! state %in% c("AcD", "ExcD", "ExpD"))$pt) / sim$n
  LLE <- subset(results$LLE_mean, results$age ==  sim[["param"]][["dxage"]] & results$sex ==  sim[["param"]][["sex"]])
  text_LLE <- paste0(LLE, " ", sep = "", "years")    
  
  # Claim data frame
  prevalence <- as.data.frame(sim$prev)
  
  # Calculate the stacked probabilities
  prevalence$prob <- prevalence$number / sim$n
  
  # Calculate overall survival
  prevalence_OS <- prevalence %>%
    filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3",
                        "Tki1afterTfr", "Tki2afterTfr", "Tki3afterTfr")) %>%
    group_by(time) %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    summarise(prob = sum(prob))
  
  # Make shaded are for Loss in life expectancy
  LLE_data <- expsurv_curve %>%
              mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
              filter(dxage == sim[["param"]][["dxage"]], sex == sim[["param"]][["sex"]]) %>%
              left_join(prevalence_OS, by = "time")
  
  # Plot the stacked probabilities
  ggplot() +
    geom_line(data=prevalence_OS, aes(x = time, y = prob, color = "Overall survival"), linewidth = 3) +
    geom_line(data = subset(expsurv_curve, sex == sim[["param"]][["sex"]] & dxage == sim[["param"]][["dxage"]]), 
              aes(x = time, y = cumprob, color = "Expected survival"), linewidth = 3) +
    geom_ribbon(data = LLE_data, 
                aes(x = time, ymin = prob, ymax = cumprob, fill = "Loss in life expectancy")) + 
    geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth= 1.2) +
    labs(x = "Time since 1L TKI entry (years)", y = "(A) Survival probability", fill = "") +
    scale_color_manual(values = c("black", "blue3"), labels = c("Expected survival", "Overall survival")) +
    scale_fill_manual(values = c("Loss in life expectancy" = "gray70")) +
    scale_linetype_manual(values=c(1, 1)) +
    scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 61), minor_breaks = seq(0, 60, by = 5), 
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), minor_breaks = seq(0, 1, by = 0.1), 
                       expand = c(0, 0)) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 24),
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          axis.line = element_line(color = "black"),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 20),
          legend.key = element_rect(colour = NA, fill = NA)) +
          guides(color=guide_legend(nrow=2, byrow=TRUE)) +
          annotate(geom="text", x=50, y=0.9, label=text_LLE, size = 14,
                   color="black")
  }

prevplotlist <- lapply(list(sim_f), function(sim) {
  calculateAndPlot_prev(sim)
})

##########################################################
## Utility
calculateAndPlot_ut <- function(sim) {
  
  # Loss in QALE
  QALE <- sum(sim$ut$utility) / sim$n                         
  LQALE <- subset(results$LQALE_mean, results$age ==  sim[["param"]][["dxage"]] & results$sex ==  sim[["param"]][["sex"]])
  text_LQALE <- paste0(LQALE, " ", sep = "", "QALYs")
  
  util <- as.data.frame(sim$ut)
  partition_by <- 0.1
  
  # Calculate the stacked qaly
  util$qaly <- util$utility*(1/partition_by) / sim$n
  
  # Calculate overall survival
  util_OS <- util %>%
    filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3",
                        "Tki1afterTfr", "Tki2afterTfr", "Tki3afterTfr")) %>%
    group_by(time) %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    summarise(qaly = sum(qaly))
  
  # Make shaded are for Loss in life expectancy
  LQALE_data <- expsurv_curve %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    filter(dxage == sim[["param"]][["dxage"]], sex == sim[["param"]][["sex"]]) %>%
    left_join(util_OS, by = "time")
  
  # Plot the stacked QALYs
  ggplot() +
    geom_line(data=util_OS, aes(x = time, y = qaly, color = "Quality-adjusted overall survival"), linewidth = 3) +
    geom_line(data = subset(expsurv_curve, sex == sim[["param"]][["sex"]] & dxage == sim[["param"]][["dxage"]]), 
              aes(x = time, y = q_cumprob, color = "Quality-adjusted expected survival"), linewidth = 3) +
    geom_ribbon(data = LQALE_data, 
                aes(x = time, ymin = qaly, ymax = q_cumprob, fill = "Loss in QALE"))+ 
    geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth= 1.2) +
    labs(x = "Time since 1L TKI entry (years)", y = "(B) Quality-adjusted survival probability", fill = "") +
    scale_color_manual(values = c("black", "darkorange"), labels = c("Quality-adjusted expected survival", "Quality-adjusted overall survival")) +
    scale_fill_manual(values = c("Loss in QALE" = "lightpink")) +
    scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 61), minor_breaks = seq(0, 60, by = 5), 
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), minor_breaks = seq(0, 1, by = 0.1), 
                       expand = c(0, 0)) +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 24),
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          axis.line = element_line(color = "black"),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 20),
          legend.key = element_rect(colour = NA, fill = NA)) + 
          guides(color=guide_legend(nrow=2,byrow=TRUE)) +
          annotate(geom="text", x=40, y=0.9, label=text_LQALE, size = 14,
                   color="black")
  }
  
utilityplotlist <- lapply(list(sim_f), function(sim) {
  calculateAndPlot_ut(sim)
})
####################
## Female
prev_f <- prevplotlist[[1]]
prev_f

utility_f <- utilityplotlist[[1]]
utility_f

# Plot two figures together
plot <- ggarrange(prev_f, utility_f, ncol = 2,
          common.legend = FALSE, legend="bottom")
annotate_figure(plot, top = text_grob(paste0("Females ", age, " years"), face = "bold", size = 36))
ggsave(paste0("../Output/10_plot_AUC_LLE_f_poster", age, ".pdf"), width = 16, height = 9, dpi = 1000)
}
################################################################