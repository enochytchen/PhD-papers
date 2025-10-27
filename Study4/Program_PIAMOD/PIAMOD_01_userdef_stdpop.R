## File name: PIAMOD_01_userdef_stdpop.R
## Updated: 2025-01-13 Enoch Chen: Standardized population should be the *total population* 
## Project: CML Cost
## Purpose: Create a standard population of Sweden 2000 for age standardization

#############################
####### Read packages #######
#############################
library(dplyr)

###########################################
####### Sweden Year 2000 population #######
###########################################
# Data downloaded from the Human Mortality Database
# https://www.mortality.org/Country/Country?cntr=SWE
# Population size: 5-year
# The second line is the header
swepop <-  read.csv("../Data/Population5.txt", sep="", header=TRUE, skip=1)
swepop_2000 <- swepop |>
               filter(swepop$Year == 2000)

# Extract `agemin` and `agemax` from `Age`
swepop_2000_clean <- swepop_2000 |>
                     mutate(
                        agemin = as.integer(gsub("-.*", "", Age)),     # Extract lower age limit
                        agemax = as.integer(gsub(".*-", "", Age))) |>  # Extract upper age limit
                     filter(agemin < 100) |> # Drop agemin >= 100
                     select(agemin, agemax, Total)

# Combine age groups 
swepop_2000_clean2 <- swepop_2000_clean |>
                     mutate(
                       agemin = ifelse(agemin >= 0 & agemin < 20, 0, agemin),
                       agemin = ifelse(agemin >= 20 & agemin < 45, 20, agemin),
                       agemin = ifelse(agemin >= 45 & agemin < 55, 45, agemin),
                       agemin = ifelse(agemin >= 55 & agemin < 65, 55, agemin),
                       agemin = ifelse(agemin >= 65 & agemin < 75, 65, agemin),
                       agemin = ifelse(agemin >= 75 & agemin < 100, 75, agemin),
                      
                       agemax = ifelse(agemin >= 0 & agemin < 20, 20, agemax),
                       agemax = ifelse(agemin >= 20 & agemin < 45, 44, agemax),
                       agemax = ifelse(agemin >= 45 & agemin < 55, 54, agemax),
                       agemax = ifelse(agemin >= 55 & agemin < 65, 64, agemax),
                       agemax = ifelse(agemin >= 65 & agemin < 75, 74, agemax),
                       agemax = ifelse(agemin >= 75 & agemin < 100, 99, agemax)) |>
                      group_by(agemin, agemax) |>
                      summarise(Total = round(sum(Total, na.rm = TRUE),0),
                                .groups = "drop") |> # Sum population
                      rename(stdpop = Total)

# Create a variable called AGE_GROUP
swepop_2000_clean2 <- swepop_2000_clean2 |>
                      mutate(AGE_GROUP = paste0(agemin, "-", agemax)) |>
                      select(AGE_GROUP, stdpop)
table(swepop_2000_clean2$AGE_GROUP)

# Add an AGE_GROUP for all ages 0-99
swepop_2000_clean2 <- swepop_2000_clean2 |>
                      bind_rows(
                        tibble(
                          AGE_GROUP = "0-99",
                          stdpop = sum(swepop_2000_clean2$stdpop, na.rm = TRUE)
                        )
                      )
                      
# save as txt file 
write.table(swepop_2000_clean2, file = "../Data/PIAMOD_01_userdef_stdpop.txt", 
            sep="\t", row.names = FALSE, quote = FALSE)
