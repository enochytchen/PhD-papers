## File name: Cost_05a1_prev_distcosts_plot.R
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
# Set desired secondary axis max
sec_max <- 50

# Compute scale factor based on desired secondary max
scale_factor <- (max(prev_dist_CI$PREV.CASES, na.rm = TRUE) * 2) / 
  max(total_prev_cost$est, na.rm = TRUE)

# Match primary axis max so secondary axis max = 50
primary_max <- sec_max * scale_factor

ggplot() + 
  # (i) crude prevalent cases
  geom_errorbar(
    data = prev_dist_CI,
    aes(x = YEAR, ymin = PREV.CASES.lb, ymax = PREV.CASES.ub),
    color = "black", width = 0.5
  ) +
  geom_point(
    data = prev_dist_CI, 
    aes(x = YEAR, y = PREV.CASES), 
    color = "black", size = 3
  ) +
  
  # (ii) average yearly cost per patient
  geom_point(
    data = aycpp_df, aes(x = YEAR, y = aycpp),
    color = "black", size = 3) +

  # (iii) total prevalence costs (scaled for secondary axis)
  geom_point(
    data = total_prev_cost,
    aes(x = year, y = est * scale_factor),
    color = "blue", size = 3
  ) +
  geom_errorbar(
    data = total_prev_cost,
    aes(x = year, ymin = lb * scale_factor, ymax = ub * scale_factor),
    width = 0.2, color = "blue"
  ) +
  
  scale_y_continuous(
    name = "Prevalence Cases",
    limits = c(0, primary_max),
    breaks = seq(0, primary_max, by = 500),
    expand = c(0, 0),
    sec.axis = sec_axis(
      trans = ~ . / scale_factor, 
      name = "Total Prevalence Cost (USD million)",
      breaks = seq(0, sec_max, 10)
    )
  ) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2025, 2030), 
    minor_breaks = seq(2015, 2030, by = 1),
    expand = c(0, 0.5)
  ) +
  theme_minimal()

# Define timestamp
timestamp <- format(Sys.time(), "%Y%m%d")

# Save plot
ggsave(paste0("../Output/Cost_05a1_prev_distcosts_plot_", timestamp, ".png"), plot_C, width = 10, height = 6, bg = "white", dpi = 300)
