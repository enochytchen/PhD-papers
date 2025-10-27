## Updated: 20240409 Plot 10-year overall surival (Question from A. Hochhaus)
## Purpose: Plot overall survival by age group

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

## Set decimals 
options("scipen"=100, "digits"=4)

## Read packages
library(readstata13)
library(dplyr)
library(ggplot2)
library(pammtools)
library(ggh4x)
library(survival)
library(ggpubr)
##############################################################
##============================================================
## Plot to check the data
##============================================================
##############################################################
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

##############################################################
##============================================================
## Estimate K-M survival
##============================================================
##############################################################
## Write a function to turn survfit object into a dataframe
df.survfit <- function(survfit.data) {
  data.frame(time = survfit.data$time,
             n.risk = survfit.data$n.risk,
             n.event = survfit.data$n.event,
             n.censor = survfit.data$n.censor,
             surv = survfit.data$surv,
             std.err = survfit.data$std.err,
             upper = survfit.data$upper,
             lower = survfit.data$lower,
             strata = rep(names(survfit.data$strata), survfit.data$strata))
}

## K-M survival estimates by age group and sex
## Males
fit_m <- survfit(Surv(t0, t, death == 1) ~ agegroup, data = subset(cml2, sex==1))
km_m <- df.survfit(fit_m)
km_m <- subset(km_m, km_m$time<=10) 

## Add time 0's surv - 1
time_0 <- data.frame(
  time = 0,
  n.risk = NA,
  n.event = NA,
  n.censor = NA,
  surv = 1,
  std.err = NA,
  upper = NA,
  lower = NA,
  strata = unique(km_m$strata)
)
km_m <- rbind(time_0, km_m)

## Females
fit_f <- survfit(Surv(t0, t, death == 1) ~ agegroup, data = subset(cml2, sex==0))
km_f <- df.survfit(fit_f)
km_f <- subset(km_f, km_f$time<=10)
km_f <- rbind(time_0, km_f)

## Males
plot_m <- ggplot() +
          geom_step(data = subset(km_m, strata =="agegroup=0-49"), aes(x = time, y = surv*100, color = "ag1", linetype = "ag1"), linewidth = 1.2) +
          geom_step(data = subset(km_m, strata =="agegroup=50-59"), aes(x = time, y = surv*100, color = "ag2", linetype = "ag2"), linewidth = 1.2) +
          geom_step(data = subset(km_m, strata =="agegroup=60-69"), aes(x = time, y = surv*100, color = "ag3", linetype = "ag3"), linewidth = 1.2) +
          geom_step(data = subset(km_m, strata =="agegroup=70-79"), aes(x = time, y = surv*100, color = "ag4", linetype = "ag4"), linewidth = 1.2) +
          geom_step(data = subset(km_m, strata =="agegroup=80+"), aes(x = time, y = surv*100, color = "ag5", linetype = "ag5"), linewidth = 1.2) + 
          
          scale_x_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10),
                             labels = seq(0, 10, by = 2), minor_breaks = seq(0, 10, by = 1)) +
          scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100),
                             labels = seq(0, 100, by = 20), minor_breaks = seq(0, 100, by = 10)) +
          labs(x = "Time since diagnosis (years)", y = "Survival probability (%)", title = "") +
          scale_color_manual(values = c("ag1" = "darkolivegreen3", "ag2" = "cornflowerblue",
                                        "ag3" = "mediumorchid3", "ag4" = "firebrick1", "ag5" = "gold"), 
                             labels = c("18-49", "50-59", "60-69", "70-79", "80-99"), 
                             name = "Age group (years)") +
          scale_linetype_manual(values = c("ag1" = "solid", "ag2" = "dashed", "ag3" = "twodash", "ag4" = "dotdash", "ag5" = "longdash"), 
                                labels = c("18-49", "50-59", "60-69", "70-79", "80-99"),
                                name = "Age group (years)") +
          guides(x = "axis_minor", y = "axis_minor",
                 color = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)), linetype = guide_legend(nrow = 2), keyheight = unit(2, "lines")) +
          theme(legend.position = "bottom",
                legend.title = element_text(size = 24),
                legend.key = element_rect(colour = NA, fill = NA),
                legend.key.width = unit(1.5, "cm"),
                panel.grid = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color = "black"),
                plot.title = element_text(hjust = 0.5, size = 36),
                axis.text = element_text(size = 24),
                axis.title.x = element_text(size = 24),
                axis.title.y = element_text(size = 24),
                legend.text = element_text(size = 24)) +
          ggtitle("Males")
plot_m


## Females
plot_f <- ggplot() +
          geom_step(data = subset(km_f, strata =="agegroup=0-49"), aes(x = time, y = surv*100, color = "ag1", linetype = "ag1"), linewidth = 1.2) +
          geom_step(data = subset(km_f, strata =="agegroup=50-59"), aes(x = time, y = surv*100, color = "ag2", linetype = "ag2"), linewidth = 1.2) +
          geom_step(data = subset(km_f, strata =="agegroup=60-69"), aes(x = time, y = surv*100, color = "ag3", linetype = "ag3"), linewidth = 1.2) +
          geom_step(data = subset(km_f, strata =="agegroup=70-79"), aes(x = time, y = surv*100, color = "ag4", linetype = "ag4"), linewidth = 1.2) +
          geom_step(data = subset(km_f, strata =="agegroup=80+"), aes(x = time, y = surv*100, color = "ag5", linetype = "ag5"), linewidth = 1.2) + 
          
          scale_x_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10),
                             labels = seq(0, 10, by = 2), minor_breaks = seq(0, 10, by = 1)) +
          scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100),
                             labels = seq(0, 100, by = 20), minor_breaks = seq(0, 100, by = 10)) +
          labs(x = "Time since diagnosis (years)", y = "Survival probability (%)", title = "") +
          scale_color_manual(values = c("ag1" = "darkolivegreen3", "ag2" = "cornflowerblue",
                                        "ag3" = "mediumorchid3", "ag4" = "firebrick1", "ag5" = "gold"), 
                             labels = c("18-49", "50-59", "60-69", "70-79", "80-99"), 
                             name = "Age group (years)") +
          scale_linetype_manual(values = c("ag1" = "solid", "ag2" = "dashed", "ag3" = "twodash", "ag4" = "dotdash", "ag5" = "longdash"), 
                                labels = c("18-49", "50-59", "60-69", "70-79", "80-99"),
                                name = "Age group (years)") +
          guides(x = "axis_minor", y = "axis_minor",
                 color = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)), linetype = guide_legend(nrow = 2), keyheight = unit(2, "lines")) +
          theme(legend.position = "bottom",
                legend.title = element_text(size = 24),
                legend.key = element_rect(colour = NA, fill = NA),
                legend.key.width = unit(1.5, "cm"),
                panel.grid = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color = "black"),
                plot.title = element_text(hjust = 0.5, size = 36),
                axis.text = element_text(size = 24),
                axis.title.x = element_text(size = 24),
                axis.title.y = element_text(size = 24),
                legend.text = element_text(size = 24)) +
          ggtitle("Females")
plot_f

plot <- ggarrange(plot_m, plot_f, ncol = 2,
                  common.legend = TRUE, legend="bottom")
plot
ggsave(paste0("../Output/03_overallsurv.png"), width = 16, height = 9, dpi = 1000)
#################################

