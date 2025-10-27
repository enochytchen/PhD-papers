## File name: Cost_02b_yearly_cpp_plot.R
## Updated: 2024-01-14 Enoch Chen: Added cpp for TFR, Progression, AlloSCT
##          2024-12-04 Enoch Chen: Update figures after revising cost calculations
##          2024-11-21 Enoch Chen: Plot the annual cost per patient
## Project: CML cost  
## Purpose: Plot the annual cost per patient
## Notes: FIXME How do you solve that the yaxis is 0 to 40000 but there is value AlloSCT procedue that is 100000

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}

setwd(wd)

# Define timestamp
timestamp <- format(Sys.time(), "%Y%m%d")

###################
## Read packages ##
###################
library(ggplot2)
library(dplyr)
library(ggh4x)
library(patchwork)

###################
## Load the data ##
###################
c_annual_pp <- readRDS("../Data/Cost_02a_yearly_cpp.rds")
colnames(c_annual_pp)
c_annual_pp <- c_annual_pp |>
               filter(year >= 2015)

########################################
# Plot annual cost per patient by year #
########################################
# Upper plot: AlloSCT procedure (values above 110,000) with minimal x-axis display
plot_high <- ggplot(data = c_annual_pp, aes(x = year)) +
  geom_line(aes(y = cpp_alloSCT_1y, color = "AlloSCT, 1st year"), 
            linewidth = 2, alpha = 0.9) +
  scale_color_manual(name = "TKI type", 
                     values = c("AlloSCT, 1st year" = "gold")) +
  theme_minimal() +
  theme(
    # Remove x-axis text and ticks (axis line remains thin)
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Remove legend (it will be shown in the lower plot)
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
    axis.text.y = element_text(color = "black", size = 14),
    axis.line.x = element_line(color = "grey80", linewidth = 0.2),
    axis.line.y = element_line(color = "black", linewidth = 0.5)
  ) +
  scale_y_continuous(limits = c(150000, 230000), 
                     breaks = seq(150000, 230000, 20000), 
                     expand = c(0, 0.5)) +
  scale_x_continuous(expand = c(0, 0.5), minor_breaks = seq(2015, 2030, by = 1)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "gray30")

# Lower plot: other lines and AlloSCT procedure (values <= 40000) with axis labels
plot_low <- ggplot(data = c_annual_pp, aes(x = year)) +
  geom_line(aes(y = cpp_prog, color = "Progression"), linewidth = 2, alpha = 0.9) +
  geom_line(aes(y = cpp_tfr, color = "TFR"), linewidth = 2, alpha = 0.9) +
  geom_line(aes(y = cpp_alloSCT_1y, color = "AlloSCT, 1st year"), linewidth = 2, alpha = 0.9) +
  geom_line(aes(y = cpp_alloSCT_2y, color = "AlloSCT, 2nd year+"), linewidth = 2, alpha = 0.9) +
  geom_line(aes(y = cpp_tki_1L_total, color = "1L TKI"), linewidth = 2, alpha = 0.9) +
  geom_line(aes(y = cpp_tki_2L_total, color = "2L TKI"), linewidth = 2, alpha = 0.9) +
  geom_line(aes(y = cpp_tki_3L_total, color = "3L+ TKI"), linewidth = 2, alpha = 0.9) +
  labs(x = "Year", y = "USD") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.2),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    axis.title.x = element_text(color = "black", size = 14),
    axis.title.y = element_text(color = "black", size = 14, vjust = 3),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    legend.text = element_text(size = 18),
    legend.spacing.y = unit(2, "cm")
  ) +
  scale_color_manual(name = "", 
                     values = c("1L TKI" = "cadetblue1",
                                "2L TKI" = "cornflowerblue",
                                "3L+ TKI" = "mediumorchid3",
                                "Progression" = "firebrick1", 
                                "TFR" = "darkolivegreen3",
                                "AlloSCT, 1st year" = "gold",
                                "AlloSCT, 2nd year+" = "darkorange")) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "gray30") +
  scale_y_continuous(limits = c(0, 40000), 
                     breaks = seq(0, 40000, 10000), 
                     labels = seq(0, 40000, 10000),
                     expand = c(0, 0.5)) +
  scale_x_continuous(breaks = c(2015, 2018, 2020, 2025, 2030), 
                     minor_breaks = seq(2015, 2030, by = 1),
                     expand = c(0, 0.5)) +
  # Add a break indicator above the lower plot
  annotate("segment", x = 2015, xend = 2030, y = 42000, yend = 42000, color = "black", linewidth = 1) +  
  annotate("text", x = 2022, y = 43000, label = "...", size = 6) +
  coord_cartesian(clip = "off")

# Combine the two plots, making the upper plot shorter (height ratio 1:3)
final_plot <- plot_high + plot_low + 
  plot_layout(ncol = 1, heights = c(1, 3)) +
  plot_annotation(title = "(B) Yearly cost per patient", 
                  theme = theme(plot.title = element_text(size = 18, hjust = 0.75)))

# Display the final combined plot
final_plot

# Optionally, assign to plot_B
plot_B <- final_plot

# Save plot
ggsave(paste0("../Output/Cost_02b_yearly_cpp_plot_", timestamp, ".png"), plot_B, width = 10, height = 6, bg = "white", dpi = 300)
