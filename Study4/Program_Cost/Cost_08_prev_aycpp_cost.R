## File name: Cost_08_prev_aycpp_cost.R
## Updated: 2025-08-13 Enoch Chen: (i) crude prevalent cases of CML, 
##                                 (ii) average yearly cost per patient
##                                 (iii) total prevalence costs.

## Project: CML Cost
## Purpose: Plot prevalence costs
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggh4x)


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

# Put CI into prev_cost
prev_dist_CI <- prev_dist |>
                left_join(piamod_ALL, by = "YEAR") |>
                group_by(YEAR) |>
                summarise(PREV.CASES.lb = sum(PREV.CASES * PREV.CASES.lb.ratio, na.rm = TRUE),
                          PREV.CASES.ub = sum(PREV.CASES * PREV.CASES.ub.ratio, na.rm = TRUE),
                          PREV.CASES = sum(PREV.CASES, na.rm = TRUE))


############################################
##  (ii) average yearly cost per patient  ##
############################################
# Cost calculations are all retrieved from this fil
source("Cost_05b_prev_costs_tab.R")

# Average YCPP
weighted_cpp_dis0

aycpp_df <- data.frame(
  YEAR = as.integer(names(weighted_cpp_dis0)),
  aycpp = as.integer(round(weighted_cpp_dis0, 0))
)

###################################
##  (iii) Total prevalence costs ##
###################################

# Read prevalence costs output
prev_cost3_dis0_wide
print(prev_cost3_dis0_wide$Item)

# Estimate
estimate <- prev_cost3_dis0_wide |>
  filter(str_ends(Item, "prev_cost_dis0"))
estimate_long <- estimate |>
  pivot_longer(cols = -Item,
               names_to = "year",
               values_to = "cost") |>
  mutate(year = as.numeric(year)) |>
  mutate(state = case_when(
    str_detect(Item, "tki_1L") ~ "1L TKI",
    str_detect(Item, "tki_2L") ~ "2L TKI",
    str_detect(Item, "tki_3L") ~ "3L+ TKI",
    str_detect(Item, "prog") ~ "Progression",
    str_detect(Item, "alloSCT_1y") ~ "AlloSCT, 1st year",
    str_detect(Item, "alloSCT_2y") ~ "AlloSCT, 2nd year+",
    str_detect(Item, "tfr") ~ "TFR")) |>
  mutate(state = factor(state, levels = c("1L TKI", 
                                          "2L TKI", 
                                          "3L+ TKI", 
                                          "Progression", 
                                          "AlloSCT, 1st year", 
                                          "AlloSCT, 2nd year+", 
                                          "TFR"))) |>
  select(-Item)



total_prev_cost <- total_prev_cost3 |>
  pivot_longer(cols = -Item, names_to = "year", values_to = "cost") |>
  mutate(year = as.numeric(year))|>
  mutate(type = case_when(str_ends(Item, "dis0_ub_USD") ~ "ub",
                          str_ends(Item, "dis0_lb_USD") ~ "lb",
                          str_ends(Item, "dis0_USD")    ~ "est"))|>
  filter(!is.na(type)) |>
  dplyr::select(year, type, cost) |>
  pivot_wider(names_from = type, values_from = cost)


##########
## PLOT ##
##########
library(cowplot)
library(patchwork)

theme <- theme(plot.title = element_text(size = 18, hjust = 0.5),
               panel.background = element_blank(),
               panel.grid.major.x = element_line(color = "grey80", linewidth = 0.2),
               panel.grid.minor.x = element_line(color = "grey80", linewidth = 0.2), # added
               panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
               panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
               axis.text.x = element_text(color = "black", size = 12),
               axis.text.y = element_text(color = "black", size = 12),
               axis.title.x = element_text(color = "black", size = 12),
               axis.title.y = element_text(color = "black", size = 12, margin = margin(l = 10)),
               axis.line.x = element_line(color = "black", linewidth = 0.5),
               axis.line.y = element_line(color = "black", linewidth = 0.5),
               axis.ticks.y.right = element_line(),
               axis.ticks.y.left = element_line(),
               legend.title = element_blank(),
               legend.text = element_text(size = 12),
               legend.spacing.y = unit(2, "cm")) 

# Set desired secondary axis max
sec_max <- 50

# Compute scale factor based on desired secondary max
scale_factor <- 0.1

# Create the main plot
main_plot <- ggplot() + 
  # (i) crude prevalent cases
  geom_bar(
    data = prev_dist_CI, 
    aes(x = YEAR, y = PREV.CASES, fill = "Prevalent Cases"),
    stat="identity"
  ) +
  
  # (ii) average yearly cost per patient (scaled to fit on plot)
  geom_line(
    data = aycpp_df, 
    aes(x = YEAR, y = aycpp * scale_factor, color = "Average Yearly Cost per Patient (USD)"),
    linetype = "solid",
  ) +
  geom_point(
    data = aycpp_df, 
    aes(x = YEAR, y = aycpp * scale_factor, color = "Average Yearly Cost per Patient (USD)",
        shape = "Average Yearly Cost per Patient (USD)"),
    size = 5) +

  # (iii) total prevalence costs (scaled for secondary axis)
  geom_line(
    data = total_prev_cost, 
    aes(x = year, y =  est * 100, color = "Prevalence Cost (USD thousand)"),
    linetype = "solid"
  ) +
  geom_point(
    data = total_prev_cost,
    aes(x = year, y = est * 100, 
        color = "Prevalence Cost (USD thousand)", shape = "Prevalence Cost (USD thousand)"),
    size = 5,
  ) +
  scale_y_continuous(
    name = "Number of Individuals",
    limits = c(0, 5000),
    breaks = seq(0, 5000, by = 1000),
    expand = c(0, 0),
    sec.axis = sec_axis(
      transform = ~ . / scale_factor,
      name = "USD/USD thousand",
      breaks = seq(0, 50000, 10000),
    )
  ) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2025, 2030), 
    minor_breaks = seq(2015, 2030, by = 1),
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    name = NULL, # Removes legend title
    values = c(
      "Average Yearly Cost per Patient (USD)" = "red",
      "Prevalence Cost (USD thousand)" = "blue"
    )
  ) +
  scale_shape_manual(
    name = NULL, # Removes legend title
    values = c(
      "Average Yearly Cost per Patient (USD)" = 16,
      "Prevalence Cost (USD thousand)" = 17
    )
  ) +
  scale_fill_manual(
    name = NULL, # Removes legend title
    values = c("Prevalent Cases" = "grey30")
  ) +
  labs(x = "Year") +
  theme +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 3,
      override.aes = list(
        shape = c(16, 17))))

main_plot

# Define timestamp
timestamp <- format(Sys.time(), "%Y%m%d")

# Save the plot as eps
ggsave(
  filename = paste0("../Output/Cost_08_prev_aycpp_cost_", timestamp, ".eps"),
  plot = main_plot,
  device = cairo_ps,
  width = 16,
  height = 9,
  bg = "white",
  dpi = 600
)

# Save plot as png
ggsave(paste0("../Output/Cost_08_prev_aycpp_cost_", timestamp, ".png"), main_plot, width = 10, height = 6, bg = "white", dpi = 600)
