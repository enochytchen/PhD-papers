## Updated: 2024-08-15 Enoch Chen: remove Bower 2016
##          2024-04-19 Enoch Chen: Plot the fig after fixing clock-set code
##          2024-01-15 Enoch Chen: add bower 2016
## Purpose: Plot Loss in LE and QALE

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

# Make all the LE 54.5 and all the QALE 55.5
#####################################
##==================================##
## Loss in life expectancy and QALE ##
##==================================##
######################################
## Females 
plot_f <- ggplot() +
  geom_errorbar(data = females, aes(x = age + 1, ymin = LLE_lower, ymax = LLE_upper), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age + 1, y = LLE_mean, color = "LLE_mean"), size = 10) +
  geom_errorbar(data = females, aes(x = age - 1, ymin = LQALE_lower, ymax = LQALE_upper), width = 0.3, alpha = 1) +
  geom_point(data = females, aes(x = age - 1, y = LQALE_mean, color = "LQALE_mean"), shape = 17, size = 10) +
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87.5),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 21),
                     minor_breaks = seq(0, 21, by = 1),
                     labels = seq(0, 20, by = 5),
                     expand=expansion()) +
  labs(x = "Age (years)", y = "Years/QALYs", title = "") +
  scale_color_manual(values = c("LQALE_mean" = "darkorange", "LLE_mean" = "blue3"), 
                     labels = c("LQALE", "LLE"), 
                     breaks = c("LQALE_mean", "LLE_mean"),
                     name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(17, 16))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "right",
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

text <- text_grob("Loss in LE or QALE", face = "bold", size = 36)

plot_LLE_LQALE2 <- ggarrange(text, plot_f, ncol = 1, nrow = 2, heights=c(0.5, 5))

plot_LLE_LQALE2 <- cowplot::ggdraw(plot_LLE_LQALE2) + 
                   theme(plot.background = element_rect(fill="white", color = NA))
plot_LLE_LQALE2


ggsave(paste0("../Output/07a_plot_LLE_LQALE_poster.png"), width = 10, height = 9, dpi = 300)
################################################################
