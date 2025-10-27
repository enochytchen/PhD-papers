## File name: Cost_00_phase_at_dx.R
## Updated: 
## Project: CML cost  
## Purpose: Extract phase at diagnosis from Swedish CML register
##          https://statistik.incanet.se/KML/

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}
setwd(wd)

## Set decimals 
options("scipen"=100, "digits"=1)

###################
## Read packages ##
###################
library(dplyr)
library(readxl)

#################
## Import data ##
#################
# Read data excel
phase <- read_xlsx("../Data/Cost_00_phase_at_dx.xlsx")

# Rename first column
names(phase)[1] <- "year"

# Convert values to numeric (excluding 'year')
phase <- phase |> 
         mutate(across(c(CP, AP, BP, Unknown), as.numeric)) |> 
         # Assume all Unknown are CP
         mutate(CP = CP + Unknown, Unknown = 0)

# Define years to replace
years_to_replace <- as.character(c(2000, 2001, 2024:2030))

# Extract last row as average
avg_row <- phase |> tail(1) |> dplyr::select(-year) 

# Create new rows for the missing years
new_rows <- tibble(
  year = years_to_replace
) |> bind_cols(avg_row)

# Remove existing rows for those years (if any), and bind new ones
phase_clean <- phase |> 
               filter(!(year %in% years_to_replace)) |> 
               bind_rows(new_rows) |>
               mutate(Prog = AP + BP) |>
               dplyr::select(year, CP, AP, BP, Prog) |>
               filter(year != "Average") |>
               mutate(YEAR = as.numeric(year)) |>
               arrange(YEAR) 

phase_clean

saveRDS(phase_clean, "../Data/Cost_00_phase_at_dx.rds")
################################################################################

