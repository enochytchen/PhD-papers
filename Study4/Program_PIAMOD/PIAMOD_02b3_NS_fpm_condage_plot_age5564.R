## File name: PIAMOD_02b2_NS_fpm_condage_plot.R
## Updated: 2025-03-21 Enoch Chen: Plot model-based relative survival conditional on ages
## Project: CML Cost
## Purpose: Plot predicted relative survival from flexible parametric models

###################
## Read packages ##
###################
library(ggplot2)
library(gridExtra)

###############
## Read data ##
###############
preddata2 <- read.table(file = "../Data/PIAMOD_02b2_NS_fpm_condage_plot.txt", 
                        header = TRUE, sep = "\t", quote = "")
#############################################################
# Define unique combinations of age groups and sex
# Only 55-64 years
age_groups <- list(
  "55-64" = c(55, 64))

sex_groups <- c(1, 2)

plot_list <- list()  # initialize plot list

for (age_label in names(age_groups)) {
  for (nsex in sex_groups) {
    
    sex_label <- ifelse(nsex == 1, "Males", "Females")
    label <- paste0(sex_label, " ", age_label, " years")
    
    plot <- ggplot() +
      geom_line(data = subset(preddata2, stime_y == 1 & sex == nsex & agegrp == age_label & yeardiag <= 2019), 
                aes(x = yeardiag, y = rs, color = "1-year RS"), linewidth = 1) +
      geom_line(data = subset(preddata2, stime_y == 1 & sex == nsex & agegrp == age_label & yeardiag >= 2019), 
                aes(x = yeardiag, y = rs, color = "1-year RS"), linetype = "longdash", linewidth = 1) +
      geom_ribbon(data = subset(preddata2, stime_y == 1 & sex == nsex & agegrp == age_label), 
                  aes(x = yeardiag, ymin = rs.lb, ymax = rs.ub), fill = "skyblue", alpha = 0.2) +
      
      geom_line(data = subset(preddata2, stime_y == 5 & sex == nsex & agegrp == age_label & yeardiag <= 2019),
                aes(x = yeardiag, y = rs, color = "5-year RS"), linewidth = 1) +
      geom_line(data = subset(preddata2, stime_y == 5 & sex == nsex & agegrp == age_label & yeardiag >= 2019),
                aes(x = yeardiag, y = rs, color = "5-year RS"), linetype = "longdash", linewidth = 1) +
      geom_ribbon(data = subset(preddata2, stime_y == 5 & sex == nsex & agegrp == age_label), 
                  aes(x = yeardiag, ymin = rs.lb, ymax = rs.ub), fill = "blue2", alpha = 0.2) +
      
      geom_line(data = subset(preddata2, stime_y == 10 & sex == nsex & agegrp == age_label & yeardiag <= 2019),
                aes(x = yeardiag, y = rs, color = "10-year RS"),  linewidth = 1) +
      geom_line(data = subset(preddata2, stime_y == 10 & sex == nsex & agegrp == age_label & yeardiag >= 2019),
                aes(x = yeardiag, y = rs, color = "10-year RS"),  linetype = "longdash", linewidth = 1) +
      geom_ribbon(data = subset(preddata2, stime_y == 10 & sex == nsex & agegrp == age_label), 
                  aes(x = yeardiag, ymin = rs.lb, ymax = rs.ub), fill = "navy", alpha = 0.2) +
      
      scale_color_manual(name = "Group", 
                         values = c("1-year RS" = "skyblue", 
                                    "5-year RS" = "blue2", 
                                    "10-year RS" = "navy"),
                         breaks = c("1-year RS", "5-year RS", "10-year RS")) +
      geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
      theme_minimal() +
      labs(y = ifelse(nsex == 2, "", "Net survival"), x = "Year") +
      scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), expand = c(0, 0)) +
      scale_x_continuous(breaks = c(1973, seq(1980, 2030, 10), 2030), 
                         limits = c(1973, 2030), 
                         minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0)) +
      ggtitle(label) +
      theme(plot.title = element_text(size = 12, hjust = 0.5),
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
ggsave(paste0("../Output/PIAMOD_02b3_NS_fpm_condage_plot_age5564_", timestamp, ".png"), plot, width = 16, height = 9, bg = "white", dpi = 300)
