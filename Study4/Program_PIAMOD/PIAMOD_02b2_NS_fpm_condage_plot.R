## File name: PIAMOD_02b2_NS_fpm_condage_plot.R
## Updated: 2025-03-21 Enoch Chen: Plot model-based relative survival conditional on ages
## Project: CML Cost
## Purpose: Plot predicted relative survival from flexible parametric models

###################
## Read packages ##
###################
library(ggplot2)
library(gridExtra)
library(rstpm2)

###############
## Read data ##
###############
# Define age groups
age_groups <- list(
  "20-44" = c(20, 44),
  "45-54" = c(45, 54),
  "55-64" = c(55, 64),
  "65-74" = c(65, 74),
  "75-99" = c(75, 99)
)

# Create a data frame for prediction
preddata <- expand.grid(sex = c(1, 2),
                        yeardiag = 1973:2030,
                        agediag = c(9, 20:99),
                        stime_y = seq(0.5, 20, 0.5))

# Read the fitted model from PIAMOD_02b1_NS_fpm.R
load("../Data/PIAMOD_02b1_NS_fpm_fit.RData")
fit

# Predictions are averaged within each age group, that is, conditional on age group.
preddata2 <- bind_rows(lapply(1973:2030, function(yr) {
             bind_rows(lapply(c(1, 2), function(sx) {
             bind_rows(lapply(names(age_groups), function(agegrp) {
              age_range <- age_groups[[agegrp]]
              bind_rows(lapply(c(1, 3, 5, 10), function(st) {
        
              tmp <- preddata |>
                     filter(sex == sx, 
                            agediag >= age_range[1], 
                            agediag <= age_range[2], 
                            yeardiag == yr)
              
              if (nrow(tmp) == 0) return(NULL)  # Skip empty subsets
              
              tmp$stime_y <- st
              tmp$agegrp <- agegrp
              
              preds <- tryCatch(
                predict(fit, type = "meansurv", newdata = tmp, se.fit = TRUE),
                error = function(e) return(NULL))
              
              if (is.null(preds)) return(NULL)
              
              tmp$rs <- preds$Estimate
              tmp$rs.lb <- preds$lower
              tmp$rs.ub <- preds$upper
              
              return(tmp)
      }))
    }))
  }))
}))

# Upper bound cannot exceed 1 for plotting
write.table(preddata2, file = "../Data/PIAMOD_02b2_NS_fpm_condage_plot.txt", 
            col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")

#############################################################
# Prevent upper bound from exceeding 1
preddata2$rs.ub <- pmin(preddata2$rs.ub, 1) 

# Define unique combinations of age groups and sex
age_groups <- list(
  "20-44" = c(20, 44),
  "45-54" = c(45, 54),
  "55-64" = c(55, 64),
  "65-74" = c(65, 74),
  "75-99" = c(75, 99)
)

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
                  aes(x = yeardiag, ymin = rs.lb, ymax = rs.ub), fill = "black", alpha = 0.2) +
      
      scale_color_manual(name = "Group", 
                         values = c("1-year RS" = "skyblue", 
                                    "5-year RS" = "blue2", 
                                    "10-year RS" = "black"),
                         breaks = c("1-year RS", "5-year RS", "10-year RS")) +
      geom_vline(xintercept = 2019, linetype = "solid", color = "gray") +
      theme_minimal() +
      labs(y = "", x = "Year") +
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
ggsave(paste0("../Output/PIAMOD_02b2_NS_fpm_condage_plot_", timestamp, ".png"), plot, width = 10, height = 10, bg = "white", dpi = 300)
