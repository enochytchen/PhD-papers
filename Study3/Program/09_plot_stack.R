## Updated: 2024-04-19 Enoch Chen: Plot the fig after fixing clock-ret code
##          2023-11-26 Enoch Chen: Remove (quality-adjusted) expected survival curves
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

##########################################
##======================================##
## Run the microsimulation model for CML##
##======================================##
##########################################
source("04_microsim_cml.R") 

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
## For ages 45, 55 65 75 85; sexes 1=Males, 0=Females
ages <- c(45, 55, 65, 75, 85)
sexes <- c(1,0)

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
  }, mc.cores = 6)

# Access the sim for s = 1 and s = 0
sim_m <- sim[[1]]
sim_f <- sim[[2]]

#########################################################
# prev means no. of patients in that stay
calculateAndPlot_prev <- function(sim) {
  
  # Claim data frame
  prevalence <- as.data.frame(sim$prev)
  
  # Calculate the stacked probabilities
  prevalence$prob <- prevalence$number / sim$n
  
  prevalence_OS <- prevalence %>%
    filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3")) %>%  
    group_by(time) %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    summarise(prob = sum(prob))
  
  prevalence <- prevalence %>%
    mutate(state = str_replace(state, "Tfr1", "TKI discontinuation"),
           state = str_replace(state, "Tfr2", "TKI discontinuation"),
           state = str_replace(state, "Tfr3", "TKI discontinuation"),
           state = str_replace(state, "Prog", "Progression"),
           state = str_replace(state, "Sct", "AlloSCT"),
           state = str_replace(state, "Tki1", "1L TKI"),
           state = str_replace(state, "Tki2", "2L TKI"),
           state = str_replace(state, "Tki3", "3L+ TKI"),
           state = str_replace(state, "AcD", "All-cause death"),
           state = str_replace(state, "ExcD", "Excess death"),
           state = str_replace(state, "ExpD", "Expected death"))
  
  # Define the desired order of the "state" variable
  order <- c("Expected death", "Excess death", "AlloSCT", "Progression", "TKI discontinuation", "3L+ TKI", "2L TKI", "1L TKI")
  
  # Reorder the "state" variable based on the desired order
  prevalence$state <- factor(prevalence$state, levels = order)

  # Plot the stacked probabilities
  ggplot() +
    geom_bar(data = prevalence, aes(x = time, y = prob, fill = state), stat = "identity", width = 0.2) +
    geom_line(data = prevalence_OS, aes(x = time, y = prob, color = "Overall survival"), linewidth = 1.2) +
    geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth=1.2) +
    labs(x = "Time since 1L TKI entry (years)", y = "Survival probability", fill = "") +
    scale_color_manual(values = c("blue3"), labels = c("Overall survival")) +
    scale_fill_manual(values = c("Excess death" = "gray30", 
                                 "Expected death" = "grey70",
                                 "1L TKI"       = "firebrick1",
                                 "2L TKI"      = "darkorange",
                                 "3L+ TKI"      = "gold",
                                 "TKI discontinuation" = "darkolivegreen1",
                                 "Progression" = "royalblue2",
                                 "AlloSCT" = "mediumorchid3")) +
    scale_linetype_manual(values=c(1, 1)) +
    scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 61), minor_breaks = seq(0, 60, by = 5), 
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), minor_breaks = seq(0, 1, by = 0.1), 
                       expand = c(0, 0)) +
    ggtitle("(A) Probabilities at states") + 
    theme(panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.text = element_text(size = 24),
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 10),
          legend.key = element_rect(colour = NA, fill = NA),
          plot.title = element_text(size = 24, hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, order = 1),
           fill = guide_legend(nrow = 3, byrow = TRUE, reverse = TRUE, order = 2),
           x = "axis_minor", y = "axis_minor")
  }

prevplotlist <- lapply(list(sim_m, sim_f), function(sim) {
  calculateAndPlot_prev(sim)
})

##########################################################
## Utility
calculateAndPlot_ut <- function(sim) {
  
  util <- as.data.frame(sim$ut)
  partition_by <- 0.1
  
  # Calculate the stacked probabilities
  util$qaly <- util$utility*(1/partition_by) / sim$n
  
  util_OS <- util %>%
    filter(state %in% c("Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "Tki1", "Tki2", "Tki3")) %>%  
    group_by(time) %>%
    mutate(time = round(time, 1)) %>% # time variable should be 0.1 instead of 0.100
    summarise(qaly = sum(qaly))
  
  util <- util %>%
    mutate(state = str_replace(state, "Tfr1", "TKI discontinuation"),
           state = str_replace(state, "Tfr2", "TKI discontinuation"),
           state = str_replace(state, "Tfr3", "TKI discontinuation"),
           state = str_replace(state, "Prog", "Progression"),
           state = str_replace(state, "Sct", "AlloSCT"),
           state = str_replace(state, "Tki1", "1L TKI"),
           state = str_replace(state, "Tki2", "2L TKI"),
           state = str_replace(state, "Tki3", "3L+ TKI"),
           state = str_replace(state, "AcD", "1 - quality-adjusted overall survival"),
           state = str_replace(state, "ExcD", "1 - quality-adjusted overall survival"),
           state = str_replace(state, "ExpD", "1 - quality-adjusted overall survival"))
  
  # Define the desired order of the "state" variable
  order <- c("1 - quality-adjusted overall survival", "AlloSCT", "Progression", "TKI discontinuation", "3L+ TKI", "2L TKI", "1L TKI")
  
  # Reorder the "state" variable based on the desired order
  util$state <- factor(util$state, levels = order)
  
  # Plot the stacked QALYs
    ggplot() +
    geom_bar(data = util, aes(x = time, y = qaly, fill = state), stat = "identity", width = 0.2) +
    geom_line(data=util_OS, aes(x = time, y = qaly, color = "Quality-adjusted overall survival"), linewidth = 1.2) +
    geom_vline(xintercept = 11.87, linetype = "dashed", color = "black", linewidth=1.2) +
    labs(x = "Time since 1L TKI entry (years)", y = "Quality-adjusted survival probability", fill = "") +
    scale_color_manual(values = c("darkorange2"), labels = c("Quality-adjusted overall survival")) +
    scale_fill_manual(values = c("1 - quality-adjusted overall survival" = "gray90",
                                 "1L TKI"       = "firebrick1",
                                 "2L TKI"      = "darkorange",
                                 "3L+ TKI"      = "gold",
                                 "TKI discontinuation" = "darkolivegreen1",
                                 "Progression" = "royalblue2",
                                 "AlloSCT" = "mediumorchid3")) +
    scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 61), minor_breaks = seq(0, 60, by = 5), 
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), minor_breaks = seq(0, 1, by = 0.1), 
                       expand = c(0, 0)) +
    ggtitle("(B) QALYs at states") + 
    theme(panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.text = element_text(size = 24),
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 10),
          legend.key = element_rect(colour = NA, fill = NA),
          plot.title = element_text(size = 24, hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, order = 1),
           fill = guide_legend(nrow = 3, byrow = TRUE, reverse = TRUE, order = 2),
           x = "axis_minor", y = "axis_minor") + 
           theme(legend.position = "none") ## Remove the legend for put the figures together
    }  

utilityplotlist <- lapply(list(sim_m, sim_f), function(sim) {
  calculateAndPlot_ut(sim)
})
####################
prev_m <- prevplotlist[[1]]
prev_m

utility_m <- utilityplotlist[[1]]
utility_m

# Plot two figures together
plot <- ggarrange(prev_m, utility_m, ncol = 2,
                  common.legend = FALSE, legend="bottom")
annotate_figure(plot, top = text_grob(paste0("Males ", age, " years"), face = "bold", size = 36))
ggsave(paste0("../Output/09_plot_stack_m", age, ".pdf"), width = 16, height = 9, dpi = 1000)

## Female
prev_f <- prevplotlist[[2]]
prev_f

utility_f <- utilityplotlist[[2]]
utility_f

# Plot two figures together
plot <- ggarrange(prev_f, utility_f, ncol = 2,
          common.legend = FALSE, legend="bottom")
annotate_figure(plot, top = text_grob(paste0("Females ", age, " years"), face = "bold", size = 36))
ggsave(paste0("../Output/09_plot_stack_f", age, ".pdf"), width = 16, height = 9, dpi = 1000)
}
################################################################

