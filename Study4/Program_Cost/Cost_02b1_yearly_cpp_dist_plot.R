## File name: Cost_02b1_yearly_cpp_dist_plot.R

## Project: CML cost  
## Purpose: To plot the composition of TKI by line over years

library(dplyr)
library(ggplot2)

# Read data
dist_all <- readRDS(file = "../Data/Cost_02a_yearly_cpp_dist.rds")

# Plot
# Define custom colors for each TKI
dist_all <- dist_all |>
  mutate(item = recode(item,
                           "IMA" = "Imatinib",
                           "DAS" = "Dasatinib",
                           "NIL" = "Nilotinib",
                           "BOS" = "Bosutinib",
                           "PON" = "Ponatinib"
  ))

# Define custom colors for full names
tki_colors <- c(
  "Imatinib"   = "#1f77b4",  # Bold blue
  "Dasatinib"  = "#ff7f0e",  # Bright orange
  "Nilotinib"  = "#2ca02c",  # Vivid green
  "Bosutinib"  = "#d62728",  # Strong red
  "Ponatinib"  = "#9467bd"   # Deep purple
)


plot_tki_dist <- ggplot(dist_all, aes(x = year, y = prop, fill = item)) +
                  geom_bar(stat = "identity", position = "stack") +
                  facet_wrap(~ line, ncol = 1) +
                  scale_fill_manual(name = "", values = tki_colors) +
                  scale_y_continuous(
                    limits = c(0, 1),
                    breaks = seq(0, 1, 0.2),
                    labels = scales::percent_format(accuracy = 1),
                    minor_breaks = seq(0, 1, 0.05),
                    expand = c(0, 0)
                  ) +
                  scale_x_continuous(
                    breaks = seq(2007, 2018, 1),
                    minor_breaks = 2007:2018,
                    expand = c(0, 0.5)
                  ) +
                  labs(
                    x = "Year",
                    y = "Proportion",
                    title = "Distribution of TKIs by Line and Year"
                  ) +
                  theme_minimal() +
                  theme(
                    plot.title = element_text(size = 18, hjust = 0.5),
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
                    legend.spacing.y = unit(2, "cm"),
                    strip.text = element_text(size = 12)
                  ) +
                  guides(linetype = "none")

plot_tki_dist

timestamp <- format(Sys.time(), "%Y%m%d")
# ggsave(paste0("../Output/Cost_02b1_yearly_cpp_dist_plot_", timestamp, ".png"), plot_tki_dist, width = 10, height = 6, bg = "white", dpi = 300)
