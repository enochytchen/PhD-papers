## Updated: 2024-08-15 Enoch Chen: remove Bower 2016
##          2024-04-19 Enoch Chen: Plot the fig after fixing clock-ret code
##          2023-11-25 Enoch Chen
## Purpose: Plot Proportion of Loss in LE and QALE

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}
## Set wd as the Data folder
wd_data = "../Data"
setwd(wd_data)

###################
##===============##
## Read packages ##
##===============##
###################
library(ggplot2)
library(ggh4x)
library(ggpubr)
library(readstata13)
library(cowplot)

###########################
##=======================##
## Read the results data ##
##=======================##
###########################
results <- readRDS("06_bootstrap_results.rds")
results <- results[results$age %in% c(45, 55, 65, 75, 85), ]
males   <- results[results$sex == 1, ]
females <- results[results$sex == 0, ]

######################################
##==================================##
##   Proportion of LLE and LQALE    ##
##==================================##
######################################
## Males 
plot_m <- ggplot() +
  geom_errorbar(data = males, aes(x = age + 1, ymin = PLLE_lower*100, ymax = PLLE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age + 1, y = PLLE_mean*100, color = "PLLE_mean"), size = 8) +
  geom_errorbar(data = males, aes(x = age - 1, ymin = PLQALE_lower*100, ymax = PLQALE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 1, y = PLQALE_mean*100, color = "PLQALE_mean"), shape = 17, size = 8) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87.5),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), 
                     minor_breaks = seq(0, 100, by = 10),
                     expand=expansion()) +
  labs(x = "Age (years)", y = "Proportion (%)", title = "") +
  scale_color_manual(values = c("PLQALE_mean" = "darkorange2", "PLLE_mean" = "blue3"), 
                     labels = c("PLQALE", "PLLE"),
                     breaks = c("PLQALE_mean", "PLLE_mean"),
                     name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(17, 16))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_line( linewidth=.3, color="grey"),
        plot.title = element_text(hjust = 0.5, size = 36),
        axis.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        legend.text = element_text(size = 24),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        panel.spacing = unit(0, "lines"),
        plot.background = element_blank(),
        legend.background = element_rect(fill = "white", color = NA)) +
  ggtitle("Males")
plot_m

## Females 
plot_f <- ggplot() +
  geom_errorbar(data = females, aes(x = age + 1, ymin = PLLE_lower*100, ymax = PLLE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age + 1, y = PLLE_mean*100, color = "PLLE_mean"), size = 8) +
  geom_errorbar(data = females, aes(x = age - 1, ymin = PLQALE_lower*100, ymax = PLQALE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age - 1, y = PLQALE_mean*100, color = "PLQALE_mean"), shape = 17, size = 8) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87.5),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), 
                     minor_breaks = seq(0, 100, by = 10),
                     expand=expansion()) +
  labs(x = "Age (years)", y = "Proportion (%)", title = "") +
  scale_color_manual(values = c("PLQALE_mean" = "darkorange2", "PLLE_mean" = "blue3"), 
                     labels = c("PLQALE", "PLLE"),
                     breaks = c("PLQALE_mean", "PLLE_mean"),
                     name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(17, 16))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_line( linewidth=.3, color="grey"),
        plot.title = element_text(hjust = 0.5, size = 36),
        axis.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        legend.text = element_text(size = 24),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        panel.spacing = unit(0, "lines"),
        plot.background = element_blank(),
        legend.background = element_rect(fill = "white", color = NA)) +
  ggtitle("Females")
plot_f

text <- text_grob("(B) Proportion of loss in life expectancy or quality-adjusted life expectancy", 
                   size = 36)

plot_PLLE_PLQALE <- ggarrange(plot_m, plot_f, ncol = 2,
                              common.legend = TRUE, legend="right") 
plot_PLLE_PLQALE2 <- ggarrange(text, plot_PLLE_PLQALE, ncol = 1, nrow = 2, heights=c(0.5, 5))
plot_PLLE_PLQALE2 <- cowplot::ggdraw(plot_PLLE_PLQALE2) + 
                     theme(plot.background = element_rect(fill="white", color = NA))
plot_PLLE_PLQALE2

ggsave(paste0("../Output/07b_plot_PLLE_PLQALE.png"), width = 16, height = 9, dpi = 300)
################################################################
