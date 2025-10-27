## File name: PIAMOD_04a1_inc_apc_plot.R
## Updated: 2025-02-15 Enoch Chen: Now I define "PIAMOD_03_piamod_constantRS_dynINC" as the first scenario
##          2025-02-02 Enoch Chen: Plot crude incidence rates by age group and sex
## 
## Project: CML Cost
## Purpose: Plot incidence estimated by PIAMOD, by sex

###################
##===============##
## Read packages ##
##===============##
###################
library(ggplot2)
library(ggh4x)
library(ggpubr)

####################
# Define timestamp #
####################
timestamp <- format(Sys.time(), "%Y%m%d")

######################
# Read piamod output #
######################
piamod <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
head(piamod)

# Males, by age group
plot_agp_m <- ggplot(data = piamod, aes(x = YEAR)) +
  
  # 20-44
  #geom_point(data = subset(piamod, AGE_GROUP == "20-44" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
  #           aes(x = YEAR, y = OBS.INCID., color = "20-44"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "20-44" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "20-44"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "20-44" & YEAR >= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "20-44"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "20-44" & SEX == 1), 
            aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#1f77b4", alpha = 0.2) +
  
  # 45-54
  #geom_point(data = subset(piamod, AGE_GROUP == "45-54" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
  #           aes(x = YEAR, y = OBS.INCID., color = "45-54"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "45-54" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "45-54"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "45-54" & YEAR >= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "45-54"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "45-54" & SEX == 1), 
            aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#ff7f0e", alpha = 0.2) +
  
  # 55-64
  #geom_point(data = subset(piamod, AGE_GROUP == "55-64" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
  #           aes(x = YEAR, y = OBS.INCID., color = "55-64"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "55-64" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "55-64"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "55-64" & YEAR >= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "55-64"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "55-64" & SEX == 1), 
            aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill="#2ca02c", alpha = 0.2) +
  
  # 65-74
  #geom_point(data = subset(piamod, AGE_GROUP == "65-74" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
  #           aes(x = YEAR, y = OBS.INCID., color = "65-74"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "65-74" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "65-74"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "65-74" & YEAR >= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "65-74"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "65-74" & SEX == 1), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#d62728", alpha = 0.2) +
  
  # 75-99
  #geom_point(data = subset(piamod, AGE_GROUP == "75-99" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
  #           aes(x = YEAR, y = OBS.INCID., color = "75-99"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "75-99" & YEAR >= 1973 & YEAR <= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "75-99"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "75-99" & YEAR >= 2019 & SEX == 1),
            aes(y = EXP.INCID., color = "75-99"), linewidth = 1, linetype = "longdash") + 
  geom_ribbon(data = subset(piamod, AGE_GROUP == "75-99" & SEX == 1), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#9467bd", alpha = 0.2) +
  
  # Vertical line at 2019 to mark transition year
  geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
  
  scale_color_manual(name = "Age group", 
                     values = c("20-44" = "#1f77b4",  # Blue
                                "45-54" = "#ff7f0e",  # Orange
                                "55-64" = "#2ca02c",  # Green
                                "65-74" = "#d62728",  # Red
                                "75-99" = "#9467bd")) + # Purple
  
  # Additional plot aesthetics
  theme_minimal() +
  labs(linetype = "Period", y = "Incidence rate (per 100 000 person-years)", x = "Year") +
  
  scale_y_continuous(limits = c(0, 8.1), breaks = seq(0, 8, 1), labels = seq(0, 8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = c(1973, seq(1980, 2030, 10)), 
                     limits = c(1973, 2030), 
                     minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0.5)) +
  # Add title
  ggtitle("Males") +
  
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
        panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(2, "cm")) + 
  guides(linetype = "none")
plot_agp_m

###########################
## Females, by age group ##
###########################
plot_agp_f <- ggplot(data = piamod, aes(x = YEAR)) +
  
  # 20-44
  #geom_point(data = subset(piamod, AGE_GROUP == "20-44" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
  #           aes(x = YEAR, y = OBS.INCID., color = "20-44"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "20-44" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "20-44"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "20-44" & YEAR >= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "20-44"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "20-44" & SEX == 2), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#1f77b4", alpha = 0.2) +
  
  # 45-54
  #geom_point(data = subset(piamod, AGE_GROUP == "45-54" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
  #           aes(x = YEAR, y = OBS.INCID., color = "45-54"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "45-54" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "45-54"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "45-54" & YEAR >= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "45-54"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "45-54" & SEX == 2), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#ff7f0e", alpha = 0.2) +
  
  # 55-64
  #geom_point(data = subset(piamod, AGE_GROUP == "55-64" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
  #           aes(x = YEAR, y = OBS.INCID., color = "55-64"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "55-64" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "55-64"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "55-64" & YEAR >= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "55-64"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "55-64" & SEX == 2), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill="#2ca02c", alpha = 0.2) +
  
  # 65-74
  #geom_point(data = subset(piamod, AGE_GROUP == "65-74" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
  #           aes(x = YEAR, y = OBS.INCID., color = "65-74"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "65-74" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "65-74"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "65-74" & YEAR >= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "65-74"), linewidth = 1, linetype = "longdash") +
  geom_ribbon(data = subset(piamod, AGE_GROUP == "65-74" & SEX == 2), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#d62728", alpha = 0.2) +
  
  # 75-99
  #geom_point(data = subset(piamod, AGE_GROUP == "75-99" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
  #           aes(x = YEAR, y = OBS.INCID., color = "75-99"), size = 4, shape = 16) +
  geom_line(data = subset(piamod, AGE_GROUP == "75-99" & YEAR >= 1973 & YEAR <= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "75-99"), linewidth = 1, linetype = "solid") +
  geom_line(data = subset(piamod, AGE_GROUP == "75-99" & YEAR >= 2019 & SEX == 2),
            aes(y = EXP.INCID., color = "75-99"), linewidth = 1, linetype = "longdash") + 
  geom_ribbon(data = subset(piamod, AGE_GROUP == "75-99" & SEX == 2), 
              aes(ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "#9467bd", alpha = 0.2) +
  
  # Vertical line at 2019 to mark transition year
  geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
  
  scale_color_manual(name = "Age group", 
                     values = c("20-44" = "#1f77b4",  # Blue
                                "45-54" = "#ff7f0e",  # Orange
                                "55-64" = "#2ca02c",  # Green
                                "65-74" = "#d62728",  # Red
                                "75-99" = "#9467bd")) + # Purple
  
  # Additional plot aesthetics
  theme_minimal() +
  labs(linetype = "Period", y = "", x = "Year") +
  
# labs(linetype = "Period", y = "Incidence rate (per 100 000 person-years)", x = "Year") +
  
  scale_y_continuous(limits = c(0, 8.1), breaks = seq(0, 8, 1), labels = seq(0, 8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = c(1973, seq(1980, 2030, 10)), 
                     limits = c(1973, 2030), 
                     minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0.5)) +
  # Add title
  ggtitle("Females") +
  
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
        panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(2, "cm")) + 
  guides(linetype = "none")
plot_agp_f

plot_agp <- ggarrange(plot_agp_m, plot_agp_f, ncol = 2,
                      common.legend = TRUE, legend="right") 
plot_agp

ggsave(paste0("../Output/PIAMOD_04a1_inc_apc_plot_", timestamp, ".png"), plot_agp, width = 10, height = 6, bg = "white", dpi = 300)