##########
## PLOT ##
##########
library(cowplot)
library(patchwork)

theme <- theme(plot.title = element_text(size = 18, hjust = 0.5),
               panel.background = element_blank(),
               panel.grid.major.x = element_line(color = "grey80", linewidth = 0.2),
               panel.grid.minor.x = element_line(color = "grey80", linewidth = 0.2), # added
               panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
               panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
               axis.text.x = element_text(color = "black", size = 12),
               axis.text.y = element_text(color = "black", size = 12),
               axis.title.x = element_text(color = "black", size = 12),
               axis.title.y = element_text(color = "black", size = 12, margin = margin(l = 10)),
               axis.line.x = element_line(color = "black", linewidth = 0.5),
               axis.line.y = element_line(color = "black", linewidth = 0.5),
               axis.ticks.y.right = element_line(),
               axis.ticks.y.left = element_line(),
               legend.title = element_blank(),
               legend.text = element_text(size = 12),
               legend.spacing.y = unit(2, "cm")) 

# Set desired secondary axis max
sec_max <- 50

# Compute scale factor based on desired secondary max
scale_factor <- 0.1

# Create the main plot
main_plot <- ggplot() + 
  # (i) crude prevalent cases
  geom_bar(
    data = prev_dist_CI, 
    aes(x = YEAR, y = PREV.CASES, fill = "Prevalent Cases"),
    stat="identity"
  ) +
  
  scale_y_continuous(
    name = "Number of Individuals",
    limits = c(0, 5000),
    breaks = seq(0, 5000, by = 1000),
    expand = c(0, 0)
    ) +
  scale_x_continuous(
    breaks = c(2015, 2020, 2025, 2030), 
    minor_breaks = seq(2015, 2030, by = 1),
    expand = c(0, 0.5)
  ) +
  scale_fill_manual(
    name = NULL, # Removes legend title
    values = c("Prevalent Cases" = "grey30")
  ) +
  labs(x = "Year") +
  theme +
  theme(legend.position = "bottom")

main_plot

# Define timestamp
timestamp <- format(Sys.time(), "%Y%m%d")
ggsave(paste0("../Output/Cost_08_prevcases_", timestamp, ".png"), main_plot, width = 10, height = 6, bg = "white", dpi = 600)

#########################