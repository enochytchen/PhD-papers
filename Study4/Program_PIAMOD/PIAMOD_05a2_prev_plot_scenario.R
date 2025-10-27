## File name: PIAMOD_05a2_prev_plot_scenario.R
## Updated:
## Project: CML Cost
## Purpose: Plot the total prevalence cases for two scenarios

###################
## Read packages ##
###################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggh4x)

# Read piamod output
piamod_constantRS <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
piamod_dynRS <- read.csv(file="../Data/PIAMOD_03_piamod_dynRS_dynINC.csv", header = TRUE)

# Only keep all ages
piamod_constantRS <- piamod_constantRS %>% filter(AGE_GROUP == "0-99")
piamod_dynRS <- piamod_dynRS %>% filter(AGE_GROUP == "0-99")

###############################
####### Prevalent CASES #######
###############################
# Plot
plot <- ggplot()+
  
  # Plot Total Population prevalence cases
  geom_line(data = subset(piamod_constantRS, SEX == 3),
            aes(x = YEAR, y = PREV.CASES, color = "Scenario 1: fixed survival", linetype = "Scenario 1: fixed survival"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod_constantRS, SEX == 3),
            aes(x = YEAR, y = PREV.CASES.lb), color = "black", linetype = "dotted") +
  geom_line(data = subset(piamod_constantRS, SEX == 3),
            aes(x = YEAR, y = PREV.CASES.ub), color = "black", linetype = "dotted") +
  
  geom_line(data = subset(piamod_dynRS, SEX == 3),
            aes(x = YEAR, y = PREV.CASES, color = "Scenario 2: dynamic survival", linetype = "Scenario 2: dynamic survival"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod_dynRS, SEX == 3),
            aes(x = YEAR, y = PREV.CASES.lb), color = "red", linetype = "dotted") +
  geom_line(data = subset(piamod_dynRS, SEX == 3),
            aes(x = YEAR, y = PREV.CASES.ub), color = "red", linetype = "dotted") +
  geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
  
  # Manual color definitions for each group
  scale_color_manual(name = "Group", 
                     values = c("Scenario 1: fixed survival" = "black", 
                                "Scenario 2: dynamic survival" = "red")) +
  
  # Additional plot aesthetics
  theme_minimal() +
  labs(color = "Group", linetype = "Period", y = "Prevalent cases", x = "Year") +
  
  scale_y_continuous(limits = c(0, 2500), breaks = seq(0, 2500, 250), labels = seq(0, 2500, 250), expand = c(0, 0.5)) +
  scale_x_continuous(breaks = c(1973, seq(1980, 2030, 10)), 
                     limits = c(1973, 2030), 
                     minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0.5)) +
  
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
        panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18),
        axis.title.x = element_text(color = "black", size = 18),
        axis.title.y = element_text(color = "black", size = 18),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.spacing.y = unit(2, "cm")) + 
  guides(linetype = "none")
plot


# Define timestamp
timestamp <- format(Sys.time(), "%Y%m%d")

# Save plot
ggsave(paste0("../Output/PIAMOD_05a2_prev_plot_scenario_", timestamp, ".png"), plot, width = 16, height = 10, bg = "white", dpi = 300)
