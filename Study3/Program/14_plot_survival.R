## Updated: 2023-11-17 Enoch Chen: Plot the survival and compare with the stack plot

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

## Set decimals 
options("scipen"=100, "digits"=4)

## Read packages
library(readstata13)
library(rstpm2)
library(dplyr)
library(survival)
library(tidyverse)
library(gridExtra)
library(ggpubr)
###############
##===========##
## Fit a FPM ##
##===========##
###############
## Read data
cml <- read.dta13("../Data/studypop.dta", nonint.factors = TRUE)

## Manage data
cml1 <- cml %>%
        mutate(start = as.Date(date_tki1, origin = "1960-01-01"), 
               t_death  = as.Date(deathdate, origin = "1960-01-01"),
               t_exit   = as.Date(date_exit, origin = "1960-01-01"),
               stop = pmin(t_death, t_exit, na.rm = TRUE),
               death  = if_else(death %in% c(1), 1, 0),
               sex   = as.numeric(sex=="male"))

cml2 <- cml1 %>%
        mutate(t0 = 0,
               t  = (as.numeric(stop)-as.numeric(start))/365.24) 

## Fit a survival model
fit <- stpm2(Surv(t0, t, death == 1) ~ dxage + sex, data = cml2, na.action = na.pass)

# Define parameters
ages <- c(55, 65, 75, 85)
sexes <- c(1, 0)

# Initialize an empty list to store data frames
plot_data_list <- list()

# Loop through ages and sexes to create data frames
for (age in ages) {
  for (sex in sexes) {
    # Create the data frame with a unique name
    plot_data <- expand.grid(dxage = age, sex = sex, t = seq(0, 50, length.out = 600))
    
    # Store the data frame in the list with its unique name
    plot_data_list[[ paste(age, ifelse(sex == 1, "m", "f"), sep = "")]] <- plot_data
  }
}

# Make predictions using the stored data frames
stpm2_list <- list()

# Loop through the predictions list
for (plot_data_name in names(plot_data_list)) {
  # Make predictions
  predictions <- predict(fit, newdata = plot_data_list[[plot_data_name]], type = "surv")
  
  # Extract necessary columns for the data frame
  prediction_df <- data.frame(time = plot_data_list[[plot_data_name]]$t, 
                              prob = predictions)
  
  # Store the data frame in the list
  stpm2_list[[paste(gsub("plotdata", "", plot_data_name), sep = "")]] <- prediction_df
}


#########################
###### Microsim #########
#########################
## Run microsim model
source("04_microsim_cml.R")

# For ages 55, 65, 75, 85; sexes 1=Males, 0=Females
ages <- c(55, 65, 75, 85)
sexes <- c(1, 0)

sim_list <- list()

for (age in ages) {
  for (sex in sexes) {
    # Create data frame for current age and sex
    ndata <- data.frame(dxage = age, sex = sex)
    
    # Define parameters based on ndata
    param <- list(
      mu0 = getRates_cohort(ndata$sex, 2010 - ndata$dxage)$rate,
      ages = getRates_cohort(ndata$sex, 2010 - ndata$dxage)$age,
      partitionBy = 0.1,
      debug = FALSE,
      dxage = ndata$dxage,
      sex = ndata$sex,
      m1 = gsm_design(m1, newdata = ndata),
      m2 = gsm_design(m2, newdata = ndata),
      m3 = gsm_design(m3, newdata = ndata),
      m4rate = m4$rate,
      m5 = gsm_design(m5, newdata = ndata),
      m6 = gsm_design(m6, newdata = ndata),
      m7 = gsm_design(m7, newdata = ndata),
      m8 = gsm_design(m8, newdata = ndata),
      m9 = gsm_design(m9, newdata = ndata),
      m10 = gsm_design(m10, newdata = ndata),
      m11 = gsm_design(m11, newdata = ndata),
      m12 = gsm_design(m12, newdata = ndata),
      m13 = gsm_design(m13, newdata = ndata),
      m14 = gsm_design(m14, newdata = ndata),
      m15rate = m15$rate,
      m18 = gsm_design(m18, newdata = ndata),
      m19 = gsm_design(m19, newdata = ndata),
      m20rate = m20$rate
    )
    
    # Run simulations
    sim_result <- simulations(1e5, param, indivp = FALSE)
    
    # Store the result in sim_list
    if (is.null(sim_list[[as.character(age)]])) {
      sim_list[[as.character(age)]] <- list()
    }
    sim_list[[as.character(age)]][[as.character(sex)]] <- sim_result
  }
}

# Accessing a specific result, for example, age 55 and sex 0
sim_list[["55"]][["0"]]
sim_list[["55"]][["1"]]
sim_list[["65"]][["0"]]

#########################################################
 # Create an empty list to store prevalence_OS data frames
prev_list <- list()

ages <- c(55, 65, 75, 85)
sexes <- c(1, 0)

# Process simulations for all age and sex groups
for (age in ages) {
  for (s in sexes) {
    sim <- sim_list[[as.character(age)]][[as.character(s)]]
    
    # Claim data frame
    prevalence <- as.data.frame(sim$prev)
    
    # Calculate the probabilities
    prevalence$prob <- prevalence$number / sim$n
    
    # Calculate overall survival for the specific age group
    prevalence_OS <- prevalence %>%
      filter(state %in% c("Prog", "Sct", "Tfr1", "Tfr2", "Tfr3", "Tki1", "Tki2", "Tki3")) %>%  
      group_by(time) %>%                       
      summarise(prob = sum(prob))
    
    prevalence_name <- paste(age, ifelse(s == 1, "m", "f"), sep = "")
    prev_list[[prevalence_name]] <- prevalence_OS
  }
}


####  
surv_55 <- ggplot() + 
    geom_line(data=prev_list[["55m"]], aes(x = time, y = prob, color = "Microsim: Males"), inherit.aes = FALSE,
              linetype = "dashed", linewidth = 1, alpha = 0.9) +
    geom_line(data=prev_list[["55f"]], aes(x = time, y = prob, color = "Microsim: Females"), inherit.aes = FALSE,
              linetype = "dashed", linewidth = 1, alpha = 0.9) + 
    geom_line(data=stpm2_list[["55m"]], aes(x = time, y = prob, color = "stpm2: Males"), inherit.aes = FALSE,
              linetype = "solid", linewidth = 1, alpha = 0.9) + 
    geom_line(data=stpm2_list[["55f"]], aes(x = time, y = prob, color = "stpm2: Females"), inherit.aes = FALSE,
              linetype = "solid", linewidth = 1, alpha = 0.9) +
    geom_vline(xintercept = 12, linetype = "dashed", color = "black", linewidth = 1) +
    labs(x="Time (years)", y = "Overall survival")+
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 18),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.key.size = unit(3,"lines"),
          legend.position="bottom", 
          legend.box="vertical",
          legend.key = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0.5, size = 24)) + 
    # Legend
    scale_color_manual(values = c("Microsim: Males" = "red", 
                                  "Microsim: Females" = "black",
                                  "stpm2: Males" = "red",
                                  "stpm2: Females" = "black")) +
    guides(fill = guide_legend(title = "", nrow = 1), 
           color = guide_legend(title = "", nrow = 2, override.aes = list(linetype = c("dashed", "dashed", "solid", "solid")))) +
    ggtitle("Age 55")
  
surv_55  

##
surv_65 <- ggplot() + 
  geom_line(data=prev_list[["65m"]], aes(x = time, y = prob, color = "Microsim: Males"), inherit.aes = FALSE,
            linetype = "dashed", linewidth = 1, alpha = 0.9) +
  geom_line(data=prev_list[["65f"]], aes(x = time, y = prob, color = "Microsim: Females"), inherit.aes = FALSE,
            linetype = "dashed", linewidth = 1, alpha = 0.9) + 
  geom_line(data=stpm2_list[["65m"]], aes(x = time, y = prob, color = "stpm2: Males"), inherit.aes = FALSE,
            linetype = "solid", linewidth = 1, alpha = 0.9) + 
  geom_line(data=stpm2_list[["65f"]], aes(x = time, y = prob, color = "stpm2: Females"), inherit.aes = FALSE,
            linetype = "solid", linewidth = 1, alpha = 0.9) +
  geom_vline(xintercept = 12, linetype = "dashed", color = "black", linewidth = 1) +
  labs(x="Time (years)", y = "Overall survival")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(3,"lines"),
        legend.position="bottom", 
        legend.box="vertical",
        legend.key = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 24)) + 
  # Legend
  scale_color_manual(values = c("Microsim: Males" = "red", 
                                "Microsim: Females" = "black",
                                "stpm2: Males" = "red",
                                "stpm2: Females" = "black")) +
  guides(fill = guide_legend(title = "", nrow = 1), 
         color = guide_legend(title = "", nrow = 2, override.aes = list(linetype = c("dashed", "dashed", "solid", "solid")))) +
  ggtitle("Age 65")

surv_65

##
surv_75 <- ggplot() + 
  geom_line(data=prev_list[["75m"]], aes(x = time, y = prob, color = "Microsim: Males"), inherit.aes = FALSE,
            linetype = "dashed", linewidth = 1, alpha = 0.9) +
  geom_line(data=prev_list[["75f"]], aes(x = time, y = prob, color = "Microsim: Females"), inherit.aes = FALSE,
            linetype = "dashed", linewidth = 1, alpha = 0.9) + 
  geom_line(data=stpm2_list[["75m"]], aes(x = time, y = prob, color = "stpm2: Males"), inherit.aes = FALSE,
            linetype = "solid", linewidth = 1, alpha = 0.9) + 
  geom_line(data=stpm2_list[["75f"]], aes(x = time, y = prob, color = "stpm2: Females"), inherit.aes = FALSE,
            linetype = "solid", linewidth = 1, alpha = 0.9) +
  geom_vline(xintercept = 12, linetype = "dashed", color = "black", linewidth = 1) +
  labs(x="Time (years)", y = "Overall survival")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(3,"lines"),
        legend.position="bottom", 
        legend.box="vertical",
        legend.key = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 24)) + 
  # Legend
  scale_color_manual(values = c("Microsim: Males" = "red", 
                                "Microsim: Females" = "black",
                                "stpm2: Males" = "red",
                                "stpm2: Females" = "black")) +
  guides(fill = guide_legend(title = "", nrow = 1), 
         color = guide_legend(title = "", nrow = 2, override.aes = list(linetype = c("dashed", "dashed", "solid", "solid")))) +
  ggtitle("Age 75")

surv_75

##
surv_85 <- ggplot() + 
  geom_line(data=prev_list[["85m"]], aes(x = time, y = prob, color = "Microsim: Males"), inherit.aes = FALSE,
            linetype = "dashed", linewidth = 1, alpha = 0.9) +
  geom_line(data=prev_list[["85f"]], aes(x = time, y = prob, color = "Microsim: Females"), inherit.aes = FALSE,
            linetype = "dashed", linewidth = 1, alpha = 0.9) + 
  geom_line(data=stpm2_list[["85m"]], aes(x = time, y = prob, color = "stpm2: Males"), inherit.aes = FALSE,
            linetype = "solid", linewidth = 1, alpha = 0.9) + 
  geom_line(data=stpm2_list[["85f"]], aes(x = time, y = prob, color = "stpm2: Females"), inherit.aes = FALSE,
            linetype = "solid", linewidth = 1, alpha = 0.9) +
  geom_vline(xintercept = 12, linetype = "dashed", color = "black", linewidth = 1) +
  labs(x="Time (years)", y = "Overall survival")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(3,"lines"),
        legend.position="bottom", 
        legend.box="vertical",
        legend.key = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, size = 24)) + 
  # Legend
  scale_color_manual(values = c("Microsim: Males" = "red", 
                                "Microsim: Females" = "black",
                                "stpm2: Males" = "red",
                                "stpm2: Females" = "black")) +
  guides(fill = guide_legend(title = "", nrow = 1), 
         color = guide_legend(title = "", nrow = 2, override.aes = list(linetype = c("dashed", "dashed", "solid", "solid")))) +
  ggtitle("Age 85")

surv_85

plot <- ggarrange(surv_55, surv_65, surv_75, surv_85, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

ggsave(("../Output/09_plot_survival.png"), width = 16, height = 9, dpi = 500)
