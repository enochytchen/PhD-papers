## Updated: 2024-02-01 Compare estimates with Bower

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

###########################
##=======================##
## Read the results data ##
##=======================##
###########################
# Microsimulation
results <- readRDS("06_bootstrap_results.rds")
results <- results[results$age %in% c(45, 55, 65, 75, 85), ]
males   <- results[results$sex == 1, ]
females <- results[results$sex == 0, ]

# Read Bower 2016
# Original data from /Volumes/projects/SweCan/Site_specific/CML/LEL_2015/Data/timetrendests.dta
bower_original <- read.dta13("13b_bower_timetrendests.dta", nonint.factors = TRUE)
bower_original <- bower_original[(bower_original$agediag %in% c(45, 55, 65, 75, 85)) & bower_original$yeardiag == 2010, ]

## For age 45-49
bower_age45to49 <- read.dta13("13b_bower_timetrendests_age45to49.dta", nonint.factors = TRUE)
bower_age45to49 <- bower_age45to49[(bower_age45to49$agediag %in% c(45)) & bower_age45to49$yeardiag == 2010, ]
bower <- rbind(bower_age45to49, bower_original)
head(bower)
bower_males   <- bower[bower$sex == 1, ]
bower_females <- bower[bower$sex == 2, ]

# Read Bower linear
bower_linear <- read.dta13("13b_bower_linear.dta", nonint.factors = TRUE)
bower_linear <- bower_linear[bower_linear$agediag %in% c(45, 55, 65, 75, 85) & bower_linear$yeardiag == 2010, ]
head(bower_linear)
bower_linear_m <- bower_linear[bower_linear$sex == 1, ]
bower_linear_f <- bower_linear[bower_linear$sex == 2, ]

# Read Bower model but using SweCan CML with spl age
bower_SweCanCML <- read.dta13("13d_bower_SweCanCML.dta", nonint.factors = TRUE)
bower_SweCanCML <- bower_SweCanCML[bower_SweCanCML$dxage %in% c(45, 55, 65, 75, 85), ]
head(bower_SweCanCML)
bower_SweCanCML_m <- bower_SweCanCML[bower_SweCanCML$sex == "male", ]
bower_SweCanCML_f <- bower_SweCanCML[bower_SweCanCML$sex == "female", ]

# Read Bower model but using SweCan CML with linear age
bower_SweCanCML_linear <- read.dta13("13e_bower_SweCanCML_linear.dta", nonint.factors = TRUE)
bower_SweCanCML_linear <- bower_SweCanCML_linear[bower_SweCanCML_linear$dxage %in% c(45, 55, 65, 75, 85), ]
head(bower_SweCanCML_linear)
bower_SweCanCML_linear_m <- bower_SweCanCML_linear[bower_SweCanCML_linear$sex == "male", ]
bower_SweCanCML_linear_f <- bower_SweCanCML_linear[bower_SweCanCML_linear$sex == "female", ]

#####################################
##==================================##
## Loss in life expectancy and QALE ##
##==================================##
######################################
## Males 
plot_m <- ggplot() +
  geom_errorbar(data = bower_males, aes(x = agediag + 1.5, ymin = llind_lci, ymax = llind_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_males, aes(x = agediag + 1.5, y = llind, color = "bower"), size = 6) +
  
  geom_errorbar(data = bower_linear_m, aes(x = agediag + 1, ymin = lle_lci, ymax = lle_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_linear_m, aes(x = agediag + 1, y = lle, color = "bower_linear"), size = 6) +
  
  geom_errorbar(data = bower_SweCanCML_m, aes(x = dxage + 0, ymin = lle_lci, ymax = lle_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_SweCanCML_m, aes(x = dxage + 0, y = lle, color = "bower_SweCanCML"), shape = 17, size = 6) +
  
  geom_errorbar(data = bower_SweCanCML_linear_m, aes(x = dxage - 1, ymin = lle_lci, ymax = lle_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_SweCanCML_linear_m, aes(x = dxage - 1, y = lle, color = "bower_SweCanCML_linear"), shape = 17 , size = 6) +
  
  geom_errorbar(data = males, aes(x = age - 1.5, ymin = LLE_lower, ymax = LLE_upper), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 1.5, y = LLE_mean, color = "microsim"), shape = 17, size = 6) +
  
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(-5, 20, by = 5), limits = c(-5, 20),
                     minor_breaks = seq(-5, 20, by = 1),
                     labels = seq(-5, 20, by = 5)) +
  labs(x = "Age (years)", y = "Years/QALYs", title = "") +
  scale_color_manual(values = c("bower_linear" = "green3",
                                "bower" = "indianred2",
                                "bower_SweCanCML" = "orange",
                                "bower_SweCanCML_linear" = "pink2",
                                "microsim" = "blue3"
                                ), 
                     labels = c("LLE, CML diagnosed in 2010 by Bower et al. with splined age (original)",
                                "LLE, CML diagnosed in 2010 by Bower et al. with linear age",
                                "LLE, CP-CML diagnosed during 2007-2017 by Bower model with splined age",
                                "LLE, CP-CML diagnosed during 2007-2017 by Bower model with linear age",
                                "LLE, CP-CML diagnosed during 2007-2017 by microsimulation model with linear age"
                                ), 
                     breaks = c("bower_linear", "bower", "bower_SweCanCML", "bower_SweCanCML_linear", "microsim"),
                     name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(16, 16, 17, 17, 17))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom",
        legend.key = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 18)) +
  ggtitle("Males")

plot_m

## Females 
plot_f <- ggplot() +
  geom_errorbar(data = bower_females, aes(x = agediag + 1.5, ymin = llind_lci, ymax = llind_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_females, aes(x = agediag + 1.5, y = llind, color = "bower"), size = 6) +
  
  geom_errorbar(data = bower_linear_f, aes(x = agediag + 1, ymin = lle_lci, ymax = lle_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_linear_f, aes(x = agediag + 1, y = lle, color = "bower_linear"), size = 6) +
  
  geom_errorbar(data = bower_SweCanCML_f, aes(x = dxage + 0, ymin = lle_lci, ymax = lle_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_SweCanCML_f, aes(x = dxage + 0, y = lle, color = "bower_SweCanCML"), shape = 17, size = 6) +
  
  geom_errorbar(data = bower_SweCanCML_linear_f, aes(x = dxage - 1, ymin = lle_lci, ymax = lle_uci), width = 0.3, alpha = 1) +
  geom_point(data = bower_SweCanCML_linear_f, aes(x = dxage - 1, y = lle, color = "bower_SweCanCML_linear"), shape = 17, size = 6) +
  
  geom_errorbar(data = males, aes(x = age - 1.5, ymin = LLE_lower, ymax = LLE_upper), width = 0.3, alpha = 1) +
  geom_point(data = males, aes(x = age - 1.5, y = LLE_mean, color = "microsim"), shape = 17, size = 6) +
  
  scale_x_continuous(breaks = seq(45, 85, by = 10), limits = c(42.5, 87),
                     labels = seq(45, 85, by = 10), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(-5, 20, by = 5), limits = c(-5, 20),
                     minor_breaks = seq(-5, 20, by = 1),
                     labels = seq(-5, 20, by = 5)) +
  labs(x = "Age (years)", y = "Years/QALYs", title = "") +
  scale_color_manual(values = c("bower_linear" = "green3",
                                "bower" = "indianred2",
                                "bower_SweCanCML" = "orange",
                                "bower_SweCanCML_linear" = "pink2",
                                "microsim" = "blue3"
  ), 
  labels = c("LLE, CML diagnosed in 2010 by Bower et al. with splined age (original)",
             "LLE, CML diagnosed in 2010 by Bower et al. with linear age",
             "LLE, CP-CML diagnosed during 2007-2017 by Bower model with splined age",
             "LLE, CP-CML diagnosed during 2007-2017 by Bower model with linear age",
             "LLE, CP-CML diagnosed during 2007-2017 by microsimulation model with linear age"
  ), 
  breaks = c("bower_linear", "bower", "bower_SweCanCML", "bower_SweCanCML_linear", "microsim"),
  name = NULL) +
  guides(x = "axis_minor", y = "axis_minor",
         color = guide_legend(ncol = 1, override.aes = list(shape = c(16, 16, 17, 17, 17))),
         linetype = guide_legend(ncol = 1)) +
  theme(legend.position = "bottom",
        legend.key = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 18)) +
  ggtitle("Females")

plot_f

plot <- ggarrange(plot_m, plot_f, ncol = 2,
                  common.legend = TRUE, legend="bottom")
plot
ggsave(paste0("../Output/13_compare_Bower_LLE.png"), width = 16, height = 9, dpi = 1000)
################################################################