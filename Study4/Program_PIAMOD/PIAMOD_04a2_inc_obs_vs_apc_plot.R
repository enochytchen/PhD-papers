## File name: PIAMOD_04a2_inc_obs_vs_apc_plot.R
## Updated: 
## 
## Project: CML Cost
## Purpose: Plot observed and modelled incidence estimated by PIAMOD, by sex and age groups

###################
##===============##
## Read packages ##
##===============##
###################
library(ggplot2)
library(ggh4x)
library(ggpubr)
library(grid)
library(gridExtra)

######################
# Read piamod output #
######################
piamod <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
head(piamod)

age_groups <- c("20-44", "45-54", "55-64", "65-74", "75-99")
sex_groups <- c(1, 2) 

# Create an empty list to store plots
plot_list <- list()

# Loop over each age group and sex combination
for (age in age_groups) {
  for (nsex in sex_groups) {
    
    # Generate plot title
    sex_label <- ifelse(nsex == 1, "Males", "Females")
    age_label <- paste0(sex_label, " ", age, " years")
    
    # Subset data for this age group and sex
    data_subset <- subset(piamod, AGE_GROUP == age & SEX == nsex)
    
    # Set y-axis scale based on age group
    y_scale <- if (age %in% c("65-74", "75-99")) {
      scale_y_continuous(limits = c(0, 8.1), breaks = seq(0, 8, 2), labels = seq(0, 8, 2), expand = c(0, 0))
    } else {
      scale_y_continuous(limits = c(0, 4.1), breaks = seq(0, 4, 1), labels = seq(0, 4, 1), expand = c(0, 0))
    }
    
    # Create the plot
    plot <- ggplot() +
      geom_point(data = subset(data_subset, YEAR >= 1973 & YEAR <= 2019),
                 aes(x = YEAR, y = OBS.INCID., color = "Observed"), size = 2, shape = 16) +
      geom_line(data = subset(data_subset, YEAR >= 1973 & YEAR <= 2019),
                aes(x = YEAR, y = EXP.INCID., color = "Model"), linewidth = 1, linetype = "solid") +
      geom_line(data = subset(data_subset, YEAR > 2019),
                aes(x = YEAR, y = EXP.INCID., color = "Model"), linewidth = 1, linetype = "longdash") +
      geom_ribbon(data = data_subset, 
                  aes(x = YEAR, ymin = EXP.INCID.lb, ymax = EXP.INCID.ub), fill = "grey50", alpha = 0.2) +
      
      scale_color_manual(name = "", 
                         values = c("Observed" = "black",
                                    "Model" = "red")) +
      
      geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
      
      theme_minimal() +
      labs(y = "", x = "Year") +
      y_scale +
      scale_x_continuous(breaks = c(1973, seq(1980, 2020, 10), 2030), 
                         limits = c(1973, 2030), 
                         minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0.5)) +
      ggtitle(age_label) +
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
    
    # Store the plot in the list
    plot_list[[age_label]] <- plot
  }
}

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

# Combine plots in a 2-column layout
combined_plots <- arrangeGrob(grobs = plots_without_legends, ncol = 2)

# Add shared y-axis label
y_axis_label <- textGrob("Incidence rate (per 100 000 person-years)", 
                         rot = 90, gp = gpar(fontsize = 12))

# Arrange plots and legend
plot <- grid.arrange(
  arrangeGrob(
    y_axis_label,
    arrangeGrob(combined_plots, shared_legend, ncol = 2, widths = c(6, 1)),
    ncol = 2,
    widths = c(1, 20)  # Adjust as needed
  )
)

# Display the plot
plot

timestamp <- format(Sys.time(), "%Y%m%d")
ggsave(paste0("../Output/PIAMOD_04a2_inc_obs_vs_apc_plot_", timestamp, ".png"), plot, width = 10, height = 10, bg = "white", dpi = 300)
