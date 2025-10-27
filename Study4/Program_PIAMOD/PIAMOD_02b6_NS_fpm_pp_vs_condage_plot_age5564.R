## File name: PIAMOD_02b6_NS_fpm_pp_vs_condage_plot_age5564.R
## Updated:
## Project: CML Cost
## Purpose: Plot predicted relative survival from flexible parametric models conditional on age
##          Compare with non-parametric relative survival            

###################
## Read packages ##
###################
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

###############
## Read data ##
###############
# Pohar-Perme estimator
relsurv <- read.table("../Data/PIAMOD_02a1_NS_pp.txt", header = FALSE, sep = "\t")
colnames(relsurv) <- c("filterfield", "sex", "firstyear", "lastyear", 
                       "youngestage", "oldestage", "fu_6mo", "cns_pp")
relsurv$year <- relsurv$fu_6mo/2

# Flexible parametric models
preddata2 <- read.table(file = "../Data/PIAMOD_02b2_NS_fpm_condage_plot.txt", 
                        header = TRUE, sep = "\t", quote = "")

#################################################
## Duplicate the last cohort (dx 2015 to 2019) ##
## to visualise fixed survival                 ##
#################################################
ref_rows <- relsurv %>%
  filter(firstyear == 2015, lastyear == 2019)

# prepare a vector of the years you want to “repeat” into
new_years <- c(2020, 2023, 2026, 2029)

# duplicate
dup_rows <- ref_rows %>%
  # drop the originals so expand_grid can add new ones
  select(-firstyear, -lastyear) %>%
  # cross‐join with new_years
  crossing(firstyear = new_years) %>%
  # now set lastyear = firstyear
  mutate(lastyear = firstyear) %>%
  # put first/lastyear up front, if you like
  relocate(firstyear, lastyear, .before = sex)

# combine with the original data
relsurv2 <- bind_rows(relsurv, dup_rows)

# check
tail(relsurv2)

#############################################################
# Plot
sex_groups <- c(1, 2)

plot_list <- list()  # initialize plot list

for (nsex in sex_groups) {
    age_label <- "55-64"
    sex_label <- ifelse(nsex == 1, "Males", "Females")
    label <- paste0(sex_label, " ", age_label, " years")
    
    plot <- ggplot() +
      
      # Plot 1-year relative survival
      geom_line(data = subset(preddata2, stime_y == 1 & sex == nsex & agegrp == "55-64" & yeardiag <= 2019), 
                aes(x = yeardiag, y = rs, color = "Scenario 2: 1-year"), linewidth = 1) +
      geom_line(data = subset(preddata2, stime_y == 1 & sex == nsex & agegrp == "55-64" & yeardiag >= 2019), 
                aes(x = yeardiag, y = rs, color = "Scenario 2: 1-year"), linetype = "longdash", linewidth = 1) +
      geom_ribbon(data = subset(preddata2, stime_y == 1 & sex == nsex & agegrp == "55-64"), 
                  aes(x = yeardiag, ymin = rs.lb, ymax = rs.ub), fill = "gray", alpha = 0.2) +
      geom_point(data = subset(relsurv2, year == 1 & sex == nsex & youngestage == 55), 
                 aes(x = firstyear, y = cns_pp, color = "Scenario 1: 1-year"), size = 2, shape = 15, alpha = 1) +

      # Plot 5-year relative survival
      geom_line(data = subset(preddata2, stime_y == 5 & sex == nsex & agegrp == "55-64" & yeardiag <= 2019),
                aes(x = yeardiag, y = rs, color = "Scenario 2: 5-year"), linewidth = 1) +
      geom_line(data = subset(preddata2, stime_y == 5 & sex == nsex & agegrp == "55-64" & yeardiag >= 2019),
                aes(x = yeardiag, y = rs, color = "Scenario 2: 5-year"), linetype = "longdash", linewidth = 1) +
      geom_ribbon(data = subset(preddata2, stime_y == 5 & sex == nsex & agegrp == "55-64"), 
                  aes(x = yeardiag, ymin = rs.lb, ymax = rs.ub), fill = "gray", alpha = 0.2) +
      geom_point(data = subset(relsurv2, year == 5 & sex == nsex & youngestage == 55),
                 aes(x = firstyear, y = cns_pp, color = "Scenario 1: 5-year"), size = 2, shape = 16, alpha = 0.8) +
      
      scale_color_manual(name = "", 
                         values = c("Scenario 1: 1-year" = "black",
                                    "Scenario 2: 1-year" = "black",
                                    "Scenario 1: 5-year" = "red",
                                    "Scenario 2: 5-year" = "red")) +
      geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
      theme_minimal() +
      labs(y = ifelse(nsex == 2, "", "Net survival"), x = "Year") +
      scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), expand = c(0, 0)) +
      scale_x_continuous(breaks = c(1973, seq(1980, 2030, 10), 2030), 
                         limits = c(1973, 2030), 
                         minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0)) +
      ggtitle(label) +
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
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.spacing.y = unit(2, "cm")) + 
      guides(linetype = "none")
    
    plot_list[[label]] <- plot
}


# Net survival estimated by the flexible parametric models over years
plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = 2)

# Function to extract the legend
get_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend[[1]])
}

# Get the legend from one plot
shared_legend <- get_legend(plot_list[[1]])

# Remove legends from all individual plots
plots_without_legends <- lapply(plot_list, function(p) p + theme(legend.position = "none"))

# Arrange plots with the legend on the right
plot <- grid.arrange(
  do.call(gridExtra::arrangeGrob, c(plots_without_legends, ncol = 2)),
  shared_legend,
  ncol = 2, 
  widths = c(6, 1))
plot

# save
timestamp <- format(Sys.time(), "%Y%m%d")
fname <- paste0("../Output/PIAMOD_02b6_NS_fpm_pp_vs_condage_plot_age5564_", timestamp, ".eps")

# Save the plot as eps
ggsave(
  filename = fname,
  plot = plot,
  device = cairo_ps,
  width = 16,
  height = 9,
  bg = "white",
  dpi = 300
)
