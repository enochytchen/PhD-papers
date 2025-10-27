## Updated: 2023-11-25 Enoch Chen
## Purpose: Plot Proportion of Loss in LE and QALE

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

PLLE_est <- results$PLLE_mean[results$age==45 & results$sex==0]
PLQALE_est <- results$PLQALE_mean[results$age==45 & results$sex==0]

######################################
##==================================##
##   Proportion of LLE and LQALE    ##
##==================================##
######################################
## Males 
plot_m <- ggplot() +
  geom_errorbar(data = males, aes(x = age - 0.5, ymin = PLLE_lower*100, ymax = PLLE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 0.5, y = PLLE_mean*100, color = "PLLE_mean"), size = 10) +
  geom_errorbar(data = males, aes(x = age - 2, ymin = PLQALE_lower*100, ymax = PLQALE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 2, y = PLQALE_mean*100, color = "PLQALE_mean"), shape = 17, size = 10) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 65), 
                     minor_breaks = seq(0, 65, by = 5)) +
  labs(x = "Age at diagnosis (years)", y = "Proportion of loss in LE/QALE (%)", title = "") +
  scale_color_manual(values = c("PLQALE_mean" = "orange", "PLLE_mean" = "blue3"), 
                     labels = c("Proportion of loss in QALE", "Proportion of loss in LE"),
                     breaks = c("PLQALE_mean", "PLLE_mean"),
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
  geom_errorbar(data = females, aes(x = age - 0.5, ymin = PLLE_lower*100, ymax = PLLE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age - 0.5, y = PLLE_mean*100, color = "PLLE_mean"), size = 10) +
  geom_errorbar(data = females, aes(x = age - 2, ymin = PLQALE_lower*100, ymax = PLQALE_upper*100), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age - 2, y = PLQALE_mean*100, color = "PLQALE_mean"), shape = 17, size = 10) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 65), 
                     minor_breaks = seq(0, 65, by = 5)) +
  labs(x = "Age at diagnosis (years)", y = "Proportion of loss in LE/QALE (%)", title = "") +
  scale_color_manual(values = c("PLQALE_mean" = "orange", "PLLE_mean" = "blue3"), 
                     labels = c("Proportion of loss in QALE", "Proportion of loss in LE"),
                     breaks = c("PLQALE_mean", "PLLE_mean"),
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
  ggtitle("Females") + 
  annotate(geom="text", x=50, y=2, label=paste0(PLLE_est*100, " ", "%", " (of loss in LE)"), size = 8,
           color="black") + 
  annotate(geom="text", x=52, y=38, label=paste0(PLQALE_est*100, " ", "%", " (of loss in QALE)"), size = 8,
           color="black")

plot_f

plot <- ggarrange(plot_m, plot_f, ncol = 2,
                  common.legend = TRUE, legend="bottom")
plot
ggsave(paste0("../Output/07b_plot_PLLE_PLQALE_noBower_withnum.pdf"), width = 16, height = 9, dpi = 1000)
################################################################
