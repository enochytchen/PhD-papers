## File name: PIAMOD_02a2_NS_pp_plot.R
## Updated: 20250208 Enoch Chen: plot net survival by age and sex
## Project: CML Cost
## Input:   PIAMOD_02a1_NS_pp.txt
## Output:  PIAMOD_02a2_NS_pp_plot.png

## Read packages
library(ggplot2)
library(gridExtra)
library(grid)

## Read data
relsurv <- read.table("../Data/PIAMOD_02a1_NS_pp.txt", header = FALSE, sep = "\t")
colnames(relsurv) <- c("filterfield", "sex", "firstyear", "lastyear", 
                       "youngestage", "oldestage", "fu_6mo", "cns_pp")
relsurv$year <- relsurv$fu_6mo/2

# Define unique combinations of age groups and sex
age_groups <- c(20, 45, 55, 65, 75)
sex_groups <- c(1, 2) 

# Create an empty list to store plots
plot_list <- list()

# Loop over each age group and sex combination
for (age in age_groups) {
  for (nsex in sex_groups) {
    
    # Generate plot title
    sex_label <- ifelse(nsex == 1, "Males", "Females")
    age_group_label <- case_when(
      age %in% 20:44  ~ "20-44",
      age %in% 45:54  ~ "45-54",
      age %in% 55:64  ~ "55-64",
      age %in% 65:74  ~ "65-74",
      age %in% 75:99  ~ "75-99",
      TRUE ~ as.character(age) # Fallback case
    )
    age_label <- paste0(sex_label, " ", age_group_label, " years")
    
    # Create the plot
    plot <- ggplot() +
      
      # Plot 1-year relative survival
      geom_point(data = subset(relsurv, year == 1 & sex == nsex & youngestage == age), 
                 aes(x = firstyear, y = cns_pp, color = "1-year RS"), size = 4, shape = 15, alpha = 1) +
      
      # Plot 5-year relative survival
      geom_point(data = subset(relsurv, year == 5 & sex == nsex & youngestage == age),
                 aes(x = firstyear, y = cns_pp, color = "5-year RS"), size = 4, shape = 16, alpha = 0.8) +
      
      # Plot 10-year relative survival
      geom_point(data = subset(relsurv, year == 10 & sex == nsex & youngestage == age),
                 aes(x = firstyear, y = cns_pp, color = "10-year RS"), size = 4, shape = 17, alpha = 0.8) +
      
      scale_color_manual(name = "Group", 
                         values = c("1-year RS" = "skyblue", 
                                    "5-year RS" = "blue2", 
                                    "10-year RS" = "black"),
                         breaks = c("1-year RS", "5-year RS", "10-year RS")) +
      
      # Vertical line at 2019 (last year of dx)
      geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
      
      # Additional plot aesthetics
      theme_minimal() +
      labs(y = "", x = "Year") +
      
      scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), expand = c(0, 0)) +
      scale_x_continuous(breaks = c(1973, seq(1980, 2020, 10), 2030), 
                         limits = c(1973, 2030), 
                         minor_breaks = seq(1973, 2030, by = 1), expand = c(0, 0.5)) +
      
      # Add title
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
plot <- grid.arrange(
  textGrob("Net survival", rot = 90, gp = gpar(fontsize = 12)),
  arrangeGrob(combined_plots, shared_legend, ncol = 2, widths = c(6, 1)),
  ncol = 2,
  widths = c(1, 20)  # Adjust width ratio as needed
)

# save
timestamp <- format(Sys.time(), "%Y%m%d")
ggsave(paste0("../Output/PIAMOD_02a2_NS_pp_plot_", timestamp, ".png"), plot, width = 10, height = 10, bg = "white", dpi = 300)
