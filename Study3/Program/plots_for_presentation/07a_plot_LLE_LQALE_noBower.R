## Updated: 2024-01-15 Enoch Chen
## Purpose: Plot Loss in LE and QALE

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

## Set wd as the Data folder
wd_data = "../../Data"
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

###########################
##=======================##
## Read the results data ##
##=======================##
###########################
results <- readRDS("06_bootstrap_results.rds")
results <- results[results$age %in% c(45, 55, 65, 75, 85), ]
males   <- results[results$sex == 1, ]
females <- results[results$sex == 0, ]

LLE_est <- results$LLE_mean[results$age==45 & results$sex==0]
LQALE_est <- results$LQALE_mean[results$age==45 & results$sex==0]
#####################################
##==================================##
## Loss in life expectancy and QALE ##
##==================================##
######################################
## Males 
plot_m <- ggplot() +
  geom_errorbar(data = males, aes(x = age - 0.5, ymin = LLE_lower, ymax = LLE_upper), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 0.5, y = LLE_mean, color = "LLE_mean"), size = 10) +
  geom_errorbar(data = males, aes(x = age - 2, ymin = LQALE_lower, ymax = LQALE_upper), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 2, y = LQALE_mean, color = "LQALE_mean"), shape = 17, size = 10) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 21),
                     minor_breaks = seq(0, 21, by = 1),
                     labels = seq(0, 20, by = 5)) +
  labs(x = "Age at diagnosis (years)", y = "Loss in LE/QALE (Years/QALYs)", title = "") +
  scale_color_manual(values = c("LQALE_mean" = "orange", "LLE_mean" = "blue3"), 
                     labels = c("Loss in QALE", "Loss in LE"), 
                     breaks = c("LQALE_mean", "LLE_mean"),
                     name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(17, 16))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 36),
        axis.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        legend.text = element_text(size = 24)) +
  ggtitle("Males")
plot_m

## Females 
plot_f <- ggplot() +
  geom_errorbar(data = females, aes(x = age - 0.5, ymin = LLE_lower, ymax = LLE_upper), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age - 0.5, y = LLE_mean, color = "LLE_mean"), size = 10) +
  geom_errorbar(data = females, aes(x = age - 2, ymin = LQALE_lower, ymax = LQALE_upper), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age - 2, y = LQALE_mean, color = "LQALE_mean"), shape = 17, size = 10) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 21),
                     minor_breaks = seq(0, 21, by = 1),
                     labels = seq(0, 20, by = 5)) +
  labs(x = "Age at diagnosis (years)", y = "Loss in LE/QALE (Years/QALYs)", title = "") +
  scale_color_manual(values = c("LQALE_mean" = "orange", "LLE_mean" = "blue3"), 
                     labels = c("Loss in QALE", "Loss in LE"), 
                     breaks = c("LQALE_mean", "LLE_mean"),
                     name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(17, 16))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 36),
        axis.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        legend.text = element_text(size = 24)) +
  ggtitle("Females")

plot_f

plot <- ggarrange(plot_m, plot_f, ncol = 2,
                  common.legend = TRUE, legend="bottom")
plot
ggsave(paste0("../Output/07a_plot_LLE_LQALE_noBower.pdf"), width = 16, height = 9, dpi = 1000)
################################################################