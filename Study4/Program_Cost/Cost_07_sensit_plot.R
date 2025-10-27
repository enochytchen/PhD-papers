## File name: Cost_07_sensit_plot.R
## Updated: 2025-05-07 Enoch Chen: Base case now it's without discounting, and make figure tonardo!
##          2025-04-30 Enoch Chen: deleted (I) Total cost +- 25% in 2025, and 2030 
##                                 due to uncertainty of future cost projections
## Project: CML Cost
## Notes: We conducted sensitivity analyses for the following scenarios:
## A. Base case: Discounting 3% from 2025
## B. Scenario 2: increasing relative survival
## C. Discounting 0%, 5% from 2025
##
## D. 3L+ TKI yearly costs increase by 25% (one-time adjustment) in 2024
##    due to asciminib entered the market as 3L TKI in 2023-03-23
## E. 3L+ TKI yearly costs decrease by 25%  in 2025 
##    due to the entry of generic bosutinib in 2024
## F. 1L TKI yearly cost +- 25% in 2029
## G. 2L TKI yearly cost +- 25% in 2029
## H. 3L TKI yearly cost +- 25% in 2029
##     Due to potential generic nilotinib and ponatinib in 2029

source("Cost_05b_prev_costs_tab.R")

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
library(ggpubr)

########################################################
## Scenario B: Scenrio 2 increasing relative survival ##
########################################################
# Read prevalent cases distributions
prev_dist <- read.csv(file="../Data/Cost_04a_prev_dist_dynRS.csv", header = TRUE) |>
             mutate(YEAR = as.numeric(YEAR)) |>
             filter(YEAR >= 2015) |>
             dplyr::select(state, YEAR, PREV.CASES)|>
             rename(year = YEAR)|>
             mutate(PREV.CASES = round(PREV.CASES, 1)) 
head(prev_dist)

state_names <- c("Tki1" = "cpp_tki_1L_total", 
                 "Tki2" = "cpp_tki_2L_total", 
                 "Tki3" = "cpp_tki_3L_total", 
                 "Prog" = "cpp_prog", 
                 "Tfr" = "cpp_tfr",
                 "Sct_1y" = "cpp_alloSCT_1y",
                 "Sct_2y" = "cpp_alloSCT_2y")

prev_dist$state <- recode(prev_dist$state, !!!state_names)

# Read cost data of lines of TKI
cost <- readRDS("../Data/Cost_02a_yearly_cpp.rds")
tail(cost)

cost_list <- lapply(c("cpp_tki_1L_total", "cpp_tki_2L_total", "cpp_tki_3L_total", 
                      "cpp_prog", "cpp_tfr", "cpp_alloSCT_1y", "cpp_alloSCT_2y"),
                    function(col) {
                      data.frame(
                        year = cost$year,
                        state = col,
                        cpp = cost[[col]])})

# Append all the lists into one dataframe
cost2 <- do.call(rbind, cost_list)

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost2, by = c("year", "state")) 

head(prev_cost)

# No discounting
prev_cost_disc0_dynRS <- prev_cost |>
                        filter(year %in% c(2025, 2030)) |>
                        mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
                        group_by(year) |>
                        summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

##############################################
## Scenario A, C, D: 0%, 3%, 5% discounting ##
##############################################
# Read prevalent cases distributions
prev_dist <- read.csv(file="../Data/Cost_04a_prev_dist.csv", header = TRUE) |>
             mutate(YEAR = as.numeric(YEAR)) |>
             filter(YEAR >= 2015) |>
             dplyr::select(state, YEAR, PREV.CASES)|>
             rename(year = YEAR)|>
             mutate(PREV.CASES = round(PREV.CASES, 1)) 

head(prev_dist)

state_names <- c("Tki1" = "cpp_tki_1L_total", 
                 "Tki2" = "cpp_tki_2L_total", 
                 "Tki3" = "cpp_tki_3L_total", 
                 "Prog" = "cpp_prog", 
                 "Tfr" = "cpp_tfr",
                 "Sct_1y" = "cpp_alloSCT_1y",
                 "Sct_2y" = "cpp_alloSCT_2y")

prev_dist$state <- recode(prev_dist$state, !!!state_names)

# Read cost data of lines of TKI
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
prev_cost_disc0 <- prev_cost3_dis0_wide_space1 |>
                  dplyr::filter(Item == "Total prevalence costs (USD millions)") |>
                  dplyr::select(`\`2025\``, `\`2030\``)|>
                  tidyr::pivot_longer(cols = everything(), names_to = "year", values_to = "text") |>
                  dplyr::mutate(
                    year = stringr::str_remove_all(year, "`"),
                    year = as.numeric(year),
                    total_cost = stringr::str_extract(text, "^\\d+\\.\\d{1,2}"),
                    total_cost = as.numeric(total_cost)
                  ) |>
                  dplyr::select(year, total_cost)

prev_cost_disc3 <- prev_cost3_dis3_wide_space1 |>
                   dplyr::filter(Item == "Total prevalence costs (USD millions)") |>
                   dplyr::select(`\`2025\``, `\`2030\``)|>
                   tidyr::pivot_longer(cols = everything(), names_to = "year", values_to = "text") |>
                   dplyr::mutate(
                    year = stringr::str_remove_all(year, "`"),
                    year = as.numeric(year),
                    total_cost = stringr::str_extract(text, "^\\d+\\.\\d{1,2}"),
                    total_cost = as.numeric(total_cost)
                   ) |>
                   dplyr::select(year, total_cost)

prev_cost_disc5 <- prev_cost3_dis5_wide_space1 |>
                   dplyr::filter(Item == "Total prevalence costs (USD millions)") |>
                   dplyr::select(`\`2025\``, `\`2030\``)|>
                   tidyr::pivot_longer(cols = everything(), names_to = "year", values_to = "text") |>
                   dplyr::mutate(
                     year = stringr::str_remove_all(year, "`"),
                     year = as.numeric(year),
                      total_cost = stringr::str_extract(text, "^\\d+\\.\\d{1,2}"),
                      total_cost = as.numeric(total_cost)
                    ) |>
                   dplyr::select(year, total_cost)

prev_cost_disc0
prev_cost_disc3
prev_cost_disc5

#########################################################################
# D. In 2024, 3L TKI yearly cost increases by 25% (one-time adjustment) #
#########################################################################
cost3 <- cost2 |>
         mutate(cpp = case_when(
                state == "cpp_tki_3L_total" & year >= 2024 ~ cpp * 1.25,
                TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_3Lplus25_2024 <- prev_cost |>
                                 filter(year %in% c(2025, 2030)) |>
                                 mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
                                 group_by(year) |>
                                 summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

##############################################################
# E. In 2025, 3L TKI yearly cost - 25% (one-time adjustment) #
##############################################################
cost3 <- cost2 |> mutate(cpp = case_when(
                         state == "cpp_tki_3L_total" & year >= 2025 ~ cpp * 0.75,
                         TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_3Lminus25_2025 <- prev_cost |>
                                  filter(year %in% c(2025, 2030)) |>
                                  mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
                                  group_by(year) |>
                                  summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

##############################################################
# F. 1L TKI yearly cost +- 25% (one-time adjustment) in 2029 #
##############################################################
cost3 <- cost2 |>
         mutate(cpp = case_when(
         state == "cpp_tki_1L_total" & year >= 2029 ~ cpp * 0.75,
         TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_1Lminus25_2029 <- prev_cost |>
                                  filter(year %in% c(2025, 2030)) |>
                                  mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
                                  group_by(year) |>
                                  summarise(total_cost = sum(prev_cost_mill), .groups = "drop")
####
cost3 <- cost2 |>
         mutate(cpp = case_when(
                state == "cpp_tki_1L_total" & year >= 2030 ~ cpp * 1.25,
         TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_1Lplus25_2029 <- prev_cost |>
                                 filter(year %in% c(2025, 2030)) |>
                                 mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
                                 group_by(year) |>
                                 summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

##############################################################
# G. 2L TKI yearly cost +- 25% (one-time adjustment) in 2029 #
##############################################################
cost3 <- cost2 |>
         mutate(cpp = case_when(
                state == "cpp_tki_2L_total" & year >= 2029 ~ cpp * 0.75,
                TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_2Lminus25_2029 <- prev_cost |>
                                  filter(year %in% c(2025, 2030)) |>
                                  mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
                                  group_by(year) |>
                                  summarise(total_cost = sum(prev_cost_mill), .groups = "drop")
#####
cost3 <- cost2 |>
         mutate(cpp = case_when(
                state == "cpp_tki_2L_total" & year >= 2030 ~ cpp * 1.25,
                TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
  left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_2Lplus25_2029 <- prev_cost |>
  filter(year %in% c(2025, 2030)) |>
  mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
  group_by(year) |>
  summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

##############################################################
# H. 3L TKI yearly cost +- 25% (one-time adjustment) in 2029 #
##############################################################
cost3 <- cost2 |>
         mutate(cpp = case_when(
                state == "cpp_tki_3L_total" & year >= 2029 ~ cpp * 0.75,
                TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
             left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_3Lminus25_2029 <- prev_cost |>
  filter(year %in% c(2025, 2030)) |>
  mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
  group_by(year) |>
  summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

#################################
cost3 <- cost2 |>
  mutate(cpp = case_when(
    state == "cpp_tki_3L_total" & year >= 2030 ~ cpp * 1.25,
    TRUE ~ cpp))

# Merge cost data with prevalent cases distributions
prev_cost <- prev_dist |>
  left_join(cost3, by = c("year", "state")) 

head(prev_cost)

prev_cost_disc0_3Lplus25_2029 <- prev_cost |>
  filter(year %in% c(2025, 2030)) |>
  mutate(prev_cost_mill = cpp * PREV.CASES / 1e6) |>
  group_by(year) |>
  summarise(total_cost = sum(prev_cost_mill), .groups = "drop")

#####################################################################################
# Rbind the following data frames
final_data_2025 <- bind_rows(prev_cost_disc0 %>% mutate(item = "Base case"),
                             prev_cost_disc0_dynRS %>% mutate(item = "Scenario 2: dynamic survival"),
                             prev_cost_disc3 %>% mutate(item = "Discounting 3% from 2025"),
                             prev_cost_disc5 %>% mutate(item = "Discounting 5% from 2025"),
                             prev_cost_disc0_3Lplus25_2024 %>% mutate(item = "3L+ TKI increases by 25% in 2024"),
                             prev_cost_disc0_3Lminus25_2025 %>% mutate(item = "3L+ TKI decreases by 25% in 2025")) |>
                    mutate(total_cost = round(total_cost, 2)) |>
                    filter(year == 2025) 

# Define base cost for 2025
base_cost_2025 <- final_data_2025$total_cost[final_data_2025$item == "Base case"]

# Process 2025 data
nfinal_data_2025 <- final_data_2025 |> 
  mutate(group = case_when(
          item == "Base case" ~ "Base case",
          item == "Scenario 2: dynamic survival" ~ "Dynamic survival",
          item == "Discounting 3% from 2025" ~ "Discounting: 3% from 2025",
          item == "Discounting 5% from 2025" ~ "Discounting: 5% from 2025",
          item == "3L+ TKI increases by 25% in 2024" ~ "3L+ TKI +25% in 2024",
          item == "3L+ TKI decreases by 25% in 2025" ~ "3L+ TKI -25% in 2025"))

summary_data_2025 <- nfinal_data_2025 |> 
                     group_by(group) |> 
                     dplyr::summarise(min_cost = min(total_cost), max_cost = max(total_cost), .groups = "drop") |> 
                     mutate(
                      min_dev = min_cost - base_cost_2025,
                      max_dev = max_cost - base_cost_2025
                     )

summary_data_2025 <- summary_data_2025 |> 
  mutate(group = factor(group, levels = rev(c(
   "3L+ TKI -25% in 2025",  
   "3L+ TKI +25% in 2024", 
   "Discounting: 5% from 2025", 
   "Discounting: 3% from 2025", 
   "Dynamic survival", 
   "Base case"
  ))))


final_data_2030 <- bind_rows(prev_cost_disc0 %>% mutate(item = "Base case"),
                             prev_cost_disc0_dynRS %>% mutate(item = "Scenario 2: dynamic survival"),
                             prev_cost_disc3 %>% mutate(item = "Discounting 3% from 2025"),
                             prev_cost_disc5 %>% mutate(item = "Discounting 5% from 2025"),
                             prev_cost_disc0_3Lplus25_2024 %>% mutate(item = "3L+ TKI increases by 25% in 2024"),
                             prev_cost_disc0_3Lminus25_2025 %>% mutate(item = "3L+ TKI decreases by 25% in 2025"),
                             prev_cost_disc0_1Lminus25_2029 %>% mutate(item = "1L TKI decreases by 25% in 2029"),
                             prev_cost_disc0_1Lplus25_2029 %>% mutate(item = "1L TKI increases by 25% in 2029"),
                             prev_cost_disc0_2Lminus25_2029 %>% mutate(item = "2L TKI decreases by 25% in 2029"),
                             prev_cost_disc0_2Lplus25_2029 %>% mutate(item = "2L TKI increases by 25% in 2029"),
                             prev_cost_disc0_3Lminus25_2029 %>% mutate(item = "3L+ TKI decreases by 25% in 2029"),
                             prev_cost_disc0_3Lplus25_2029 %>% mutate(item = "3L+ TKI increases by 25% in 2029")) |>
                    mutate(total_cost = round(total_cost, 2)) |>
                    filter(year == 2030)

# Define base cost for 2030
base_cost_2030 <- final_data_2030$total_cost[final_data_2030$item == "Base case"]

# Process 2030 data
nfinal_data_2030 <- final_data_2030 |> 
  mutate(group = case_when(
    item == "Base case" ~ "Base case",
    item == "Scenario 2: dynamic survival" ~ "Dynamic survival",
    item == "Discounting 3% from 2025" ~ "Discounting: 3% from 2025",
    item == "Discounting 5% from 2025" ~ "Discounting: 5% from 2025",
    item == "3L+ TKI increases by 25% in 2024" ~ "3L+ TKI +25% in 2024",
    item == "3L+ TKI decreases by 25% in 2025" ~ "3L+ TKI -25% in 2025",
    item %in% c("1L TKI decreases by 25% in 2029", "1L TKI increases by 25% in 2029") ~ "1L TKI ±25% in 2029",
    item %in% c("2L TKI decreases by 25% in 2029", "2L TKI increases by 25% in 2029") ~ "2L TKI ±25% in 2029",
    item %in% c("3L+ TKI decreases by 25% in 2029", "3L+ TKI increases by 25% in 2029") ~ "3L+ TKI ±25% in 2029"
  ))

summary_data_2030 <- nfinal_data_2030 |> 
  group_by(group) |> 
  summarise(min_cost = min(total_cost), max_cost = max(total_cost), .groups = "drop") |> 
  mutate(
    min_dev = min_cost - base_cost_2030,
    max_dev = max_cost - base_cost_2030)

summary_data_2030 <- summary_data_2030 |> 
  mutate(group = factor(group, levels = rev(c(
    "Discounting: 5% from 2025",
    "Discounting: 3% from 2025",
    "2L TKI ±25% in 2029",
    "3L+ TKI ±25% in 2029", 
    "3L+ TKI -25% in 2025",
    "3L+ TKI +25% in 2024", 
    "1L TKI ±25% in 2029", 
    "Dynamic survival", 
    "Base case"
  ))))

# Function to create a tornado plot
create_tornado_plot <- function(data, title, base_cost) {
  ggplot(data, aes(x = group, fill = factor(sign(max_dev)))) +
    geom_bar(aes(y = max_dev), stat = "identity") +
    geom_bar(aes(y = min_dev, fill = factor(sign(min_dev))), stat = "identity") +
    coord_flip() +
    geom_hline(yintercept = 0, color = "black", linewidth = 1) +
    scale_y_continuous(
      limits = c(-10, 10), 
      breaks = seq(-10, 10, 1),
      minor_breaks = seq(-10, 10, .5),
      sec.axis = sec_axis(
        transform = ~ . + base_cost, 
        name = "Total prevalence costs (USD million)", 
        breaks = seq(16, 42, 2)
      )
    ) +
    scale_fill_manual(
      values = c("-1" = "black", "1" = "red"),
      labels = c("-1" = "Lower costs", "1" = "Higher costs"),
      name = ""
    ) +
    labs(
      title = title,
      x = "Scenario",
      y = "Difference from the base case (USD million)"
    ) +
    theme_bw() +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 12),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_text(color = "black", size = 12),
          axis.title.y = element_text(color = "black", size = 12),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          legend.spacing.y = unit(2, "cm"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom", legend.justification = c(.65, 0))
}

# Create plots
plot_2025 <- create_tornado_plot(summary_data_2025, "2025", base_cost_2025) +
             annotate(
                "text", x = 1, y = 0.7, 
                label = paste0("← ", base_cost_2025), 
                size = 5, fontface = "bold")

plot_2030 <- create_tornado_plot(summary_data_2030, "2030", base_cost_2030) +
             annotate(
               "text", x = 1, y = 0.7, 
               label = paste0("← ", base_cost_2030), 
               size = 5, fontface = "bold")

# Combine plots with shared legend
final_plot <- ggarrange(plot_2025, NA, plot_2030, 
                        ncol = 1, heights = c(2, 0.2, 3),
                        common.legend = TRUE, legend = "bottom")

# Display the plot
final_plot
timestamp <- format(Sys.time(), "%Y%m%d")
fname <- paste0("../Output/Cost_07_sensit_plot_", timestamp, ".eps")

# Save the plot as eps
ggsave(
  filename = fname,
  plot = final_plot,
  device = cairo_ps,
  width = 16,
  height = 9,
  bg = "white",
  dpi = 300
)


# Save the plot as png
ggsave(
  filename = paste0("../Output/Cost_07_sensit_plot_", timestamp, ".png"),
  plot = final_plot,
  width = 16,
  height = 9,
  bg = "white",
  dpi = 300
)
