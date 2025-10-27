## File name: Cost_04b1_prev_dist_dynRS_plot.R
## Updated: 2024-12-09 Enoch Chen: Plot prevalence cases distributions 

## Project: CML Cost
## Purpose: Plot prevalence estimated by PIAMOD, by sex

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggh4x)

# Define timestamp
timestamp <- format(Sys.time(), "%Y%m%d")

# Read piamod
prev_dist <- read.csv(file="../Data/Cost_04b_prev_dist_dynRS.csv", header = TRUE)
head(prev_dist)
table(prev_dist$state)

# Only keep YEAR >= 2015
prev_dist <- prev_dist |>
             filter(YEAR >= 2015) |>
             mutate(state = str_replace(state, "Tfr", "TFR"),
                    state = str_replace(state, "Prog", "Progression"),
                    state = str_replace(state, "Sct_1y", "AlloSCT, 1st year"),
                    state = str_replace(state, "Sct_2y", "AlloSCT, 2nd year+"),
                    state = str_replace(state, "Tki1", "1L TKI"),
                    state = str_replace(state, "Tki2", "2L TKI"),
                    state = str_replace(state, "Tki3", "3L+ TKI"))
table(prev_dist$state)
order <- c("1L TKI", "2L TKI", "3L+ TKI", "Progression", 
           "AlloSCT, 1st year", "AlloSCT, 2nd year+", "TFR")
prev_dist$state <- factor(prev_dist$state, levels = order)

###############################
####### Prevalence CASES #######
###############################
plot_A <- ggplot(prev_dist, aes(x = YEAR, y = PREV.CASES, fill = state)) +
                 geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(name = "", 
                             values = c("1L TKI" = "cadetblue1",
                                        "2L TKI" = "cornflowerblue",
                                        "3L+ TKI" = "mediumorchid3",
                                        "Progression" = "firebrick1", 
                                        "TFR" = "darkolivegreen3",
                                        "AlloSCT, 1st year" = "gold",
                                        "AlloSCT, 2nd year+" = "darkorange")) +
                 labs(x = "Year", y = "Number of Cases") +
          theme_minimal() +
          labs(color = "Group", linetype = "Period", y = "Prevalence cases", x = "Year") +
          
          scale_y_continuous(limits = c(0, 2500), 
                             breaks = seq(0, 2500, 500), 
                             labels = seq(0, 2500, 500), 
                             minor_breaks = seq(0, 2500, 100),
                             expand = c(0, 0)) +
          scale_x_continuous(breaks = c(2015, 2020, 2025, 2030), 
                             minor_breaks = seq(2015, 2030, by = 1),
                             expand = c(0,0.5)) +
    
          # Add title
          ggtitle("(A) Prevalence cases by state") +
                
          theme(plot.title = element_text(size = 18, hjust = 0.5),
                panel.background = element_blank(),
                panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
                panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
                axis.text.x = element_text(color = "black", size = 14),
                axis.text.y = element_text(color = "black", size = 14),
                axis.title.x = element_text(color = "black", size = 14),
                axis.title.y = element_text(color = "black", size = 14),
                axis.line.x = element_line(color = "black", linewidth = 0.5),
                axis.line.y = element_line(color = "black", linewidth = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 18),
                legend.spacing.y = unit(2, "cm")) + 
            guides(linetype = "none")
plot_A

# Save plot
ggsave(paste0("../Output/Cost_04b1_prev_dist_dynRS_plot_", timestamp, ".png"), plot_A, width = 10, height = 6, bg = "white", dpi = 300)
