## File name: Cost_04a1_prev_dist_plot.R
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
prev_dist <- read.csv(file="../Data/Cost_04a_prev_dist.csv", header = TRUE)
head(prev_dist)
table(prev_dist$state)

# Read the piamod output for retrieving the CIs
piamod <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
piamod_ALL <- piamod[piamod$SEX == 3 & piamod$AGE_GROUP == "0-99" & piamod$YEAR >= 2000,] |>
  dplyr::select(c("YEAR", "PREV.CASES", "PREV.CASES.lb", "PREV.CASES.ub")) |>
  filter(YEAR >= 2015) |>
  mutate(PREV.CASES.lb.ratio = PREV.CASES.lb/PREV.CASES,
         PREV.CASES.ub.ratio = PREV.CASES.ub/PREV.CASES) |>
  dplyr::select(c("YEAR", "PREV.CASES.lb.ratio", "PREV.CASES.ub.ratio"))

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

# Put CI into prev_cost
prev_dist_CI <- prev_dist |>
  left_join(piamod_ALL, by = "YEAR") |>
  group_by(YEAR) |>
  summarise(PREV.CASES.lb = sum(PREV.CASES * PREV.CASES.lb.ratio, na.rm = TRUE),
            PREV.CASES.ub = sum(PREV.CASES * PREV.CASES.ub.ratio, na.rm = TRUE))

###############################
####### Prevalence CASES #######
###############################
plot_A <- ggplot(prev_dist, aes(x = YEAR, y = PREV.CASES, fill = state)) +
                 geom_bar(stat = "identity", position = "stack") +
                #geom_errorbar(
                #  data = prev_dist_CI,
                #  aes(x = YEAR, ymin = PREV.CASES.lb, ymax = PREV.CASES.ub),
                #  color = "gray70",
                #  width = 0.5,
                #  inherit.aes = FALSE
                # ) +
          scale_fill_manual(name = "", 
                             values = c("1L TKI" = "cadetblue1",
                                        "2L TKI" = "cornflowerblue",
                                        "3L+ TKI" = "mediumorchid3",
                                        "Progression" = "firebrick1", 
                                        "TFR" = "darkolivegreen3",
                                        "AlloSCT, 1st year" = "gold",
                                        "AlloSCT, 2nd year+" = "darkorange")) +
                 labs(x = "Year", y = "Number of individuals") +
          theme_minimal() +
          labs(color = "Group", linetype = "Period", y = "Number of individuals", x = "Year") +
          
          scale_y_continuous(limits = c(0, 5000), 
                             breaks = seq(0, 5000, 1000), 
                             labels = seq(0, 5000, 1000), 
                             expand = c(0, 0)) +
          scale_x_continuous(breaks = c(2015, 2020, 2025, 2030), 
                             minor_breaks = seq(2015, 2030, by = 1),
                             expand = c(0,0.5)) +
    
          theme(plot.title = element_text(size = 18, hjust = 0.5),
                panel.background = element_blank(),
                panel.grid.major.x = element_line(color = "grey80", linewidth = 0.2),
                panel.grid.minor.x = element_line(color = "grey80", linewidth = 0.2), # added
                panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
                panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
                axis.text.x = element_text(color = "black", size = 12),
                axis.text.y = element_text(color = "black", size = 12),
                axis.title.x = element_text(color = "black", size = 12),
                axis.title.y = element_text(color = "black", size = 12),
                axis.line.x = element_line(color = "black", linewidth = 0.5),
                axis.line.y = element_line(color = "black", linewidth = 0.5),
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                legend.spacing.y = unit(2, "cm")) + 
            theme(legend.position = "bottom") +
            guides(linetype = "none",
                    color = guide_legend(nrow = 2))

plot_A

# Save plot
ggsave(paste0("../Output/Cost_04a1_prev_dist_plot_", timestamp, ".png"), plot_A, width = 10, height = 6, bg = "white", dpi = 300)
