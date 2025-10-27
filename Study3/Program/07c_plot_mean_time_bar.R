## Updated: 2024-04-19 Enoch Chen: Plot the fig after fixing clock-ret code
##          2023-11-20 Enoch Chen: Redefine the studypop dx 2007-2017 and hence multistate structure changed
##          2023-10-10 Enoch Chen: plot length of stay and QALY by state
## Purpose: Plot length of stay and QALY by state

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

###################
##===============##
## Read packages ##
##===============##
###################
library(ggplot2)
library(ggh4x)
library(ggpubr)
library(readstata13)
library(dplyr)
library(tidyr)
library(stringr)

###########################
##=======================##
## Read the results data ##
##=======================##
###########################
results<- readRDS("../Data/06_bootstrap_results.rds")

results <- results %>%
           mutate(sex = ifelse(sex==1, "Males", "Females"),
                  group = paste(sex, age, "y"))

selected_groups <- c("Males 85 y", "Females 85 y", 
                     "Males 75 y", "Females 75 y", 
                     "Males 65 y", "Females 65 y", 
                     "Males 55 y", "Females 55 y",
                     "Males 45 y", "Females 45 y")

results_subset <- results %>%
                  filter(group %in% selected_groups)

#################################################
##=============================================##
## Prepare the data for plotting length of stay##
##=============================================##
#################################################
## Set the group order
results_subset$group <- factor(results_subset$group, levels = selected_groups)

## Transpose the data
output2_LoS <- results_subset %>%
               dplyr::select(group, LoS_Tfr, LoS_Prog, LoS_Sct, LoS_Tki1, LoS_Tki2, LoS_Tki3, LLE_mean) %>%
               mutate(LoS_LLE_mean = LLE_mean) %>%
               pivot_longer(cols = starts_with("LoS_"), names_to = "variable", values_to = "value")

## Add one empty variable. Just for legend
new_rows <- output2_LoS %>%
            distinct(group) %>%
            mutate(variable = "QLoS_LQALE_mean",
                   value = 0)

# Add the new rows to the original dataset
output2_LoS <- bind_rows(output2_LoS, new_rows)

## Rename the variables for a beautiful legend
output2_LoS <- output2_LoS %>%
                mutate(variable = str_replace(variable, "LoS_Tfr", "TKI discontinuation"),
                       variable = str_replace(variable, "LoS_Prog", "Progression"),
                       variable = str_replace(variable, "LoS_Sct",  "AlloSCT"),
                       variable = str_replace(variable, "LoS_Tki1", "1L TKI"),
                       variable = str_replace(variable, "LoS_Tki2", "2L TKI"),
                       variable = str_replace(variable, "LoS_Tki3", "3L+ TKI"),
                       variable = str_replace(variable, "LoS_LLE_mean", "Loss in LE"),
                       variable = str_replace(variable, "QLoS_LQALE_mean", "Loss in QALE")) ## Just for legend

# Define the desired order of the variables
order_Q <- c("Loss in LE", "Loss in QALE", "AlloSCT", "Progression", "TKI discontinuation", "3L+ TKI", "2L TKI", "1L TKI")

# Reorder the "state" variable based on the desired order
output2_LoS$variable <- factor(output2_LoS$variable, levels = order_Q)

# Label to show loss in LE
output2_LoS <- output2_LoS %>%
               group_by(group) %>% 
               mutate(lab = if_else(variable == "Loss in LE", 
                      sprintf("%.1f", value), 
                      NA_character_))

## Plot the stacked bar chart
LY <- ggplot(output2_LoS, aes(x = factor(group), y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(x = factor(group), y = value, label = lab),
                size = 6, position = position_stack(vjust = 0.5)) +
      coord_flip() +
      scale_y_continuous(breaks = seq(0, 45, 5), limits = c(0, 45), expand = c(0, 0)) +
      labs(title = "(C) Predicted mean time in state",
           y = "Years", x = "",
           fill = "Variable") +
      scale_fill_manual(values = c("TKI discontinuation" = "darkolivegreen1", "Progression" = "cornflowerblue", 
                                   "AlloSCT" = "mediumorchid3", "1L TKI" = "firebrick1", "2L TKI" = "darkorange", "3L+ TKI" = "gold", 
                                   "Loss in LE"="gray70", "Loss in QALE"="pink")) +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            plot.margin = unit(c(1,1,1,1), "cm"), 
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 16),
            legend.key = element_rect(colour = NA, fill = NA))+
     guides(fill=guide_legend(nrow=8, byrow=TRUE, reverse = TRUE),
             x = "axis_minor", y = "axis_minor")
LY
LY <- LY + theme(legend.position = "none") ## Remove the legend for put the figures together

##################################################################
##==============================================================##
## Prepare the data for plotting QAULITY-ADJUSTED length of stay##
##==============================================================##
##################################################################
output2_QLoS <- results_subset %>%
                dplyr::select(group, QLoS_Tfr, QLoS_Prog, 
                              QLoS_Sct, QLoS_Tki1, QLoS_Tki2, QLoS_Tki3, LQALE_mean) %>%
                mutate(QLoS_LQALE_mean = LQALE_mean) %>%
                pivot_longer(cols = starts_with("QLoS_"), names_to = "variable", values_to = "value") 

## Add one empty variable. Just for legend
new_rows <- output2_QLoS %>%
            distinct(group) %>%
            mutate(variable = "LoS_LLE_mean",
                   value = 0)

# Add the new rows to the original dataset
output2_QLoS <- bind_rows(output2_QLoS, new_rows)

## Rename the variables for a beautiful legend
output2_QLoS <- output2_QLoS %>%
                mutate(variable = str_replace(variable, "QLoS_Tfr", "TKI discontinuation"),
                       variable = str_replace(variable, "QLoS_Prog", "Progression"),
                       variable = str_replace(variable, "QLoS_Sct",  "AlloSCT"),
                       variable = str_replace(variable, "QLoS_Tki1", "1L TKI"),
                       variable = str_replace(variable, "QLoS_Tki2", "2L TKI"),
                       variable = str_replace(variable, "QLoS_Tki3", "3L+ TKI"),
                       variable = str_replace(variable, "QLoS_LQALE_mean", "Loss in QALE"),
                       variable = str_replace(variable, "LoS_LLE_mean", "Loss in LE")) ## Just for legend

# Define the desired order of the variables
order_Q <- c("Loss in LE", "Loss in QALE", "AlloSCT", "Progression", "TKI discontinuation", "3L+ TKI", "2L TKI", "1L TKI")

# Reorder the "state" variable based on the desired order
output2_QLoS$variable <- factor(output2_QLoS$variable, levels = order_Q)

# Label to show loss in QALE
output2_QLoS <- output2_QLoS%>%
                group_by(group) %>% 
                mutate(lab = if_else(variable == "Loss in QALE", 
                                     sprintf("%.1f", value), 
                                     NA_character_))

## Plot the stacked bar chart (quality-adjusted)
QALY <- ggplot(data=output2_QLoS) +
        geom_bar(aes(x = factor(group), y = value, fill = variable), stat = "identity") +
        geom_text(aes(x = factor(group), y = value, label = lab),
                  size = 6, position = position_stack(vjust = 0.5)) +
        coord_flip() +
        scale_y_continuous(breaks = seq(0, 45, 5), limits = c(0, 45), expand = c(0, 0)) +
        labs(title = "(D) Predicted mean QALYs in state",
             y = "QALYs", x = "",
             fill = "Variable") +
        scale_fill_manual(values = c("TKI discontinuation" = "darkolivegreen1", "Progression" = "cornflowerblue", 
                                     "AlloSCT" = "mediumorchid3", "1L TKI" = "firebrick1", "2L TKI" = "darkorange", "3L+ TKI" = "gold", 
                                     "Loss in LE"="gray70", "Loss in QALE"="pink")) + 
        theme(panel.grid = element_blank(),
              panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              plot.margin = unit(c(1,1,1,1), "cm"), 
              plot.title = element_text(hjust = 0.5, size = 16),
              axis.ticks.y = element_blank(),
              axis.text = element_text(size = 16),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              legend.title = element_blank(),
              legend.position = 'bottom',
              legend.text = element_text(size = 16),
              legend.key = element_rect(colour = NA, fill = NA)) +
        guides(fill=guide_legend(nrow=8, byrow=TRUE, reverse = TRUE),
               x = "axis_minor", y = "axis_minor")
QALY

QALY <- QALY + theme(axis.text.y = element_blank()) # Removes y-axis labels for plotting figures together
QALY

###############################
##===========================##
## Plot two figures together ##
##===========================##
###############################
plot_mean_time_bar <- ggarrange(LY, QALY, 
                      ncol = 2, nrow = 1, 
                      common.legend = TRUE, legend="right",
                      widths=c(1.2,1))
plot_mean_time_bar

ggsave(filename = "../Output/07c_plot_mean_time_bar.png", plot_mean_time_bar, width = 16, height = 9, dpi = 200)
