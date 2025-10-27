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

## Expected survival curve and quality-adjusted expected survival curve
expsurv_curve<- readRDS("../Data/05b_expsurv_curve.rds")
expsurv_curve$time <- expsurv_curve$age - expsurv_curve$dxage
expsurv_curve$age_byime <- expsurv_curve$age
expsurv_curve <- subset(expsurv_curve, select = -age)

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
##==========Stacked plots===============##
##======================================##
##########################################
## For age 45, sex 0=Females
age <- 45 
sex <- 0

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
                  m6rate  = m6rate$rate,
                  m7rate  = m7rate$rate,
                  m8 = gsm_design(m8, newdata = ndata),
                  m9 = gsm_design(m9, newdata = ndata),
                  m10 = gsm_design(m10, newdata = ndata),
                  m11 = gsm_design(m11, newdata = ndata),
                  m12 = gsm_design(m12, newdata = ndata),
                  m13rate  = m13rate$rate,
                  ## Transition 14 only expected hazard
                  m15rate  = m15rate$rate,
                  m16 = gsm_design(m16, newdata = ndata),
                  m17rate  = m17rate$rate,
                  m18 = gsm_design(m18, newdata = ndata),
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
  
  #########################################################
  # prev means no. of patients in that stay
  
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
                  
    prevalence <- prevalence %>%
      mutate(state = str_replace(state, "Tfr1", "TKI disc"),
             state = str_replace(state, "Tfr2", "TKI disc"),
             state = str_replace(state, "Tfr3", "TKI disc"),
             state = str_replace(state, "Prog", "Progression"),
             state = str_replace(state, "Sct", "AlloSCT"),
             state = str_replace(state, "Tki1", "1L TKI"),
             state = str_replace(state, "Tki1afterTfr", "1L TKI"),
             state = str_replace(state, "Tki2", "2L TKI"),
             state = str_replace(state, "Tki2afterTfr", "2L TKI"),
             state = str_replace(state, "Tki3", "3L+ TKI"),
             state = str_replace(state, "Tki3afterTfr", "3L+ TKI"),
             state = str_replace(state, "AcD", "All-cause death"),
             state = str_replace(state, "ExcD", "Excess death"),
             state = str_replace(state, "ExpD", "Expected death"))
    
    
    # Define the desired order of the "state" variable
    order <- c("Expected death", "Excess death", "AlloSCT", "Progression", "TKI disc", "3L+ TKI", "2L TKI", "1L TKI")
    
    # Reorder the "state" variable based on the desired order
    prevalence$state <- factor(prevalence$state, levels = order)

    # Plot the stacked probabilities
    prev <-   ggplot() +
              geom_bar(data = prevalence, aes(x = time, y = prob, fill = state), stat = "identity", width = 0.2) +
              geom_line(data = prevalence_OS, aes(x = time, y = prob, color = "OS"), linewidth = 3) +
              geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth=1.2) +
              labs(x = "Time since diagnosis (years)", y = "Survival probability", fill = "") +
              scale_color_manual(values = c("deepskyblue"), labels = c("OS")) +
              scale_fill_manual(values = c("Excess death" = "gray30", 
                                           "Expected death" = "grey70",
                                           "1L TKI"       = "firebrick1",
                                           "2L TKI"      = "darkorange",
                                           "3L+ TKI"      = "gold",
                                           "TKI disc" = "darkolivegreen1",
                                           "Progression" = "royalblue2",
                                           "AlloSCT" = "mediumorchid3")) +
              scale_linetype_manual(values=c(1, 1)) +
              scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), minor_breaks = seq(0, 60, by = 5), 
                                 expand = c(0, 0)) +
              scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), minor_breaks = seq(0, 1, by = 0.1), 
                                 expand = c(0, 0)) +
              theme(panel.grid = element_blank(),
                    axis.text = element_text(size = 24),
                    axis.title.x = element_text(size = 24),
                    axis.title.y = element_text(size = 24),
                    legend.title = element_blank(),
                    legend.position = 'bottom',
                    legend.text = element_text(size = 20),
                    legend.key = element_rect(colour = NA, fill = NA),
                    plot.title = element_text(size = 36, hjust = 0.5)) +
              guides(color = guide_legend(nrow = 1, byrow = TRUE, order = 1),
                     fill = guide_legend(nrow = 4, byrow = TRUE, reverse = TRUE, order = 2),
                     x = "axis_minor", y = "axis_minor") +
              ggtitle("(A) Probability in state")
    prev
  ##########################################################
  ## Utility
    
    util <- as.data.frame(sim$ut)
    partition_by <- 0.1
    
    # Calculate the stacked probabilities
    util$qaly <- util$utility*(1/partition_by) / sim$n
    
    # Calculate quality-adjusted overall survival
    util_OS <- util %>%
               filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3",
                          "Tki1afterTfr", "Tki2afterTfr", "Tki3afterTfr")) %>%  
               group_by(time) %>%
               mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
               summarise(qaly = sum(qaly))
    
    util <- util %>%
      mutate(state = str_replace(state, "Tfr1", "TKI disc"),
             state = str_replace(state, "Tfr2", "TKI disc"),
             state = str_replace(state, "Tfr3", "TKI disc"),
             state = str_replace(state, "Prog", "Progression"),
             state = str_replace(state, "Sct", "AlloSCT"),
             state = str_replace(state, "Tki1", "1L TKI"),
             state = str_replace(state, "Tki1afterTfr", "1L TKI"),
             state = str_replace(state, "Tki2", "2L TKI"),
             state = str_replace(state, "Tki2afterTfr", "2L TKI"),
             state = str_replace(state, "Tki3", "3L+ TKI"),
             state = str_replace(state, "Tki3afterTfr", "3L+ TKI"),
             state = str_replace(state, "AcD", "Death"),
             state = str_replace(state, "ExcD", "Death"),
             state = str_replace(state, "ExpD", "Death"))
    
    # Define the desired order of the "state" variable
    order <- c("Death", "AlloSCT", "Progression", "TKI disc", "3L+ TKI", "2L TKI", "1L TKI")
    
    # Reorder the "state" variable based on the desired order
    util$state <- factor(util$state, levels = order)
    
    # Plot the stacked QALYs
    utility <- ggplot() +
                geom_bar(data = util, aes(x = time, y = qaly, fill = state), stat = "identity", width = 0.2) +
                geom_line(data=util_OS, aes(x = time, y = qaly, color = "Quality-adj. OS"), linewidth = 3) +
                geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth=1.2) +
                labs(x = "Time since diagnosis (years)", y = "Quality-adj. survival probability", fill = "") +
                scale_fill_manual(values = c("Death" = "gray90",
                                             "1L TKI"       = "firebrick1",
                                             "2L TKI"      = "darkorange",
                                             "3L+ TKI"      = "gold",
                                             "TKI disc" = "darkolivegreen1",
                                             "Progression" = "royalblue2",
                                             "AlloSCT" = "mediumorchid3")) +
                scale_color_manual(values = c("deepskyblue"), labels = c("Quality-adj. OS")) +
                scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), minor_breaks = seq(0, 60, by = 5), 
                                   expand = c(0, 0)) +
                scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), minor_breaks = seq(0, 1, by = 0.1), 
                                   expand = c(0, 0)) +
                theme(panel.grid = element_blank(),
                      axis.text = element_text(size = 24),
                      axis.title.x = element_text(size = 24),
                      axis.title.y = element_text(size = 24),
                      legend.title = element_blank(),
                      legend.position = 'bottom',
                      legend.text = element_text(size = 20),
                      legend.key = element_rect(fill = "white"),
                      plot.title = element_text(size = 36, hjust = 0.5)) +
                guides(color = guide_legend(nrow = 1, byrow = TRUE, order = 1),
                       fill = guide_legend(nrow = 4, byrow = TRUE, reverse = TRUE, order = 2),
                       x = "axis_minor", y = "axis_minor") +
                ggtitle("(B) QALYs in state")
    utility        
  ####################
  ## Loss in life expectancy
  LE <- sum(subset(sim$pt, ! state %in% c("AcD", "ExcD", "ExpD"))$pt) / sim$n
  LLE <- expsurv$LE_exp[expsurv$age==age & expsurv$sex==sex] - LE
  LLE <- sprintf("%.1f", LLE)
  ## FIXME: Should write a code to receive from results instead
  text_LLE <- paste0(LLE, " ", sep = "", "years")    
  
  # Claim data frame
  prevalence <- as.data.frame(sim$prev)
  
  # Calculate the stacked probabilities
  prevalence$prob <- prevalence$number / sim$n
  
  # Calculate overall survival
  prevalence_OS <- prevalence %>%
    filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3")) %>%  
    group_by(time) %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    summarise(prob = sum(prob))
  
  # Make shaded are for loss of life expectancy
  LLE_data <- expsurv_curve %>%
              mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
              filter(dxage == sim[["param"]][["dxage"]], sex == sim[["param"]][["sex"]]) %>%
              left_join(prevalence_OS, by = "time")
  
  # Plot the stacked probabilities
  lle <- ggplot() +
    geom_line(data=prevalence_OS, aes(x = time, y = prob, color = "OS"), linewidth = 3) +
    geom_line(data = subset(expsurv_curve, sex == sim[["param"]][["sex"]] & dxage == sim[["param"]][["dxage"]]), 
              aes(x = time, y = cumprob, color = "Expected survival"), linewidth = 3) +
    geom_ribbon(data = LLE_data, 
                aes(x = time, ymin = prob, ymax = cumprob, fill = "Loss of life expectancy")) + 
    geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth= 1.2) +
    labs(x = "Time since diagnosis (years)", y = "Survival probability", fill = "") +
    scale_color_manual(values = c("black", "deepskyblue"), labels = c("Expected survival", "OS")) +
    scale_fill_manual(values = c("Loss of life expectancy" = "gray70")) +
    scale_linetype_manual(values=c(1, 1)) +
    scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), minor_breaks = seq(0, 60, by = 5), 
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
          legend.key = element_rect(fill = "white"),
          plot.title = element_text(size = 36, hjust = 0.5)) +
          guides(color=guide_legend(nrow=4, byrow=TRUE, order = 1)) +
    ggtitle("(C) Loss in LE") + 
    annotate(geom="text", x=30, y=0.82, label=text_LLE, size = 12,
             color="black")
   lle
##########################################################
  # Loss of QALE
  QALE <- sum(sim$ut$utility) / sim$n                         
  LQALE <- expsurv$QALE_exp[expsurv$age==age & expsurv$sex==sex] - QALE
  LQALE <- sprintf("%.1f", LQALE)
  ## FIXME: Should write a code to receive from results instead
  text_LQALE <- paste0(LQALE, " ", sep = "", "QALYs")
   
  util <- as.data.frame(sim$ut)
  partition_by <- 0.1
  
  # Calculate the stacked qaly
  util$qaly <- util$utility*(1/partition_by) / sim$n
  
  # Calculate quality-adjusted overall survival
  util_OS <- util %>%
    filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3")) %>%  
    group_by(time) %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    summarise(qaly = sum(qaly))
  
  # Make shaded are for loss of life expectancy
  LQALE_data <- expsurv_curve %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    filter(dxage == sim[["param"]][["dxage"]], sex == sim[["param"]][["sex"]]) %>%
    left_join(util_OS, by = "time")
  
  # Plot the stacked QALYs
  qlle <- ggplot() +
    geom_line(data=util_OS, aes(x = time, y = qaly, color = "Quality-adj. OS"), linewidth = 3) +
    geom_line(data = subset(expsurv_curve, sex == sim[["param"]][["sex"]] & dxage == sim[["param"]][["dxage"]]), 
              aes(x = time, y = q_cumprob, color = "Quality-adj. expected survival"), linewidth = 3) +
    geom_ribbon(data = LQALE_data, 
                aes(x = time, ymin = qaly, ymax = q_cumprob, fill = "Loss of QALE"))+ 
    geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth= 1.2) +
    labs(x = "Time since diagnosis (years)", y = "Quality-adj. survival probability", fill = "") +
    scale_color_manual(values = c("black", "deepskyblue"), labels = c("Quality-adj. expected survival", "Quality-adj. OS")) +
    scale_fill_manual(values = c("Loss of QALE" = "pink")) +
    scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), minor_breaks = seq(0, 60, by = 5), 
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
          legend.key = element_rect(colour = NA, fill = NA),
          plot.title = element_text(size = 36, hjust = 0.5)) +
    guides(color = guide_legend(nrow = 4, ncol=1, byrow=TRUE, order = 1),
           fill = guide_legend(nrow = 1, ncol=1, byrow = TRUE, order = 2)) +
    ggtitle("(D) Loss in QALE") +
    annotate(geom="text", x=20, y=0.75, label=text_LQALE, size = 12,
             color="black")
  qlle
###############################  
  # Plot four figures together
  plot <- ggarrange(prev, lle, utility, qlle, ncol = 2, nrow = 2,
                    common.legend = FALSE, legend="bottom")
  annotate_figure(plot, top = text_grob(paste0("Females ", age, " years"), face = "bold", size = 36))
  
  ggsave(paste0("../Output/11_plot_stack_and_AUC_LLE.png"), width = 16, height = 20, dpi = 1000)
  
  