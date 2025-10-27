## Updated: 2024-08-15 Enoch Chen: combine plots

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
}
setwd(wd)
source("07a_plot_LLE_LQALE.R") 

setwd(wd)
source("07b_plot_PLLE_PLQALE.R") 

comb <- ggarrange(plot_LLE_LQALE2, NULL, plot_PLLE_PLQALE2, ncol = 1, nrow = 3, heights = c(1, 0.05, 1))
comb

ggsave(paste0("../Output/07c_plot_combined.png"), width = 20, height = 16, dpi = 300)
