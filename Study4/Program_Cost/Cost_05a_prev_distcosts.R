## File name: Cost_05a_prev_distcosts.R
## Updated: 2025-01-15 Enoch Chen: Update after adding cpp for TFR, Progression, AlloSCT
##          2024-12-09 Enoch Chen: Calculate prevalent costs distributions 
## Project: CML Cost

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
}
setwd(wd)

###################
## Read packages ##
###################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggh4x)

###############
## Read data ##
###############
# Read prevalent cases distributions
prev_dist <- read.csv(file="../Data/Cost_04a_prev_dist.csv", header = TRUE) |>
             mutate(YEAR = as.numeric(YEAR)) |>
             filter(YEAR >= 2015) |>
             dplyr::select(state, YEAR, PREV.CASES)|>
             rename(year = YEAR)
head(prev_dist)

state_names <- c("Tki1" = "cpp_tki_1L_total", 
                 "Tki2" = "cpp_tki_2L_total", 
                 "Tki3" = "cpp_tki_3L_total", 
                 "Prog" = "cpp_prog", 
                 "Tfr" = "cpp_tfr",
                 "Sct_1y" = "cpp_alloSCT_1y",
                 "Sct_2y" = "cpp_alloSCT_2y")

prev_dist$state <- recode(prev_dist$state, !!!state_names)

# Read cost data
cost <- readRDS("../Data/Cost_02a_yearly_cpp.rds")
tail(cost)

cost_list <- lapply(c("cpp_tki_1L_total", "cpp_tki_2L_total", "cpp_tki_3L_total", 
                      "cpp_prog", "cpp_tfr", "cpp_alloSCT_1y", "cpp_alloSCT_2y"), 
             function(col) {
                        data.frame(
                        year = cost$year,
                        state = col,
                        cpp = cost[[col]])
                    }
                  )

# Append all the lists into one dataframe
cost2 <- do.call(rbind, cost_list)

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost2, by = c("year", "state")) 

head(prev_cost)

# Deal with discounting
prev_cost2 <- prev_cost |>
              mutate(
              cpp_dis = case_when(
              year >= 2025 ~ cpp / (1 + 0.03)^(year - 2024),
              TRUE ~ cpp
              )) 

# Calculate the total costs by state
prev_cost3 <- prev_cost2 |>
              mutate(prev_cost_dis = cpp_dis * PREV.CASES,
                     prev_cost_nodis = cpp * PREV.CASES) 

# Save the data
# saveRDS(prev_cost3, "../Data/Cost_05a_prev_distcosts.rds")
