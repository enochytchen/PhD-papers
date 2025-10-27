## File name: Cost_02a_yearly_cpp.R
## Updated: 2025-03-26 Enoch Chen: Make time-dependency by using the ratios over years
##          2025-02-25 Enoch Chen: Update cost by considering TLV databases
##          2025-02-19 Enoch Chen: Update the cost of alloSCT excluding the alloSCT itself
##          2025-01-16 Enoch Chen: Update TFR yearly price
##          2025-01-13 Enoch Chen: Added cost per patient for progression, alloSCT, and TKI discontinuation
##          2024-12-04 Enoch Chen: Make sure the cost calculation is based on real-world.
##                     Originally, I calculated the annual cost by determining the defined daily dose (DDD) 
##                     and multiplying by 365.241. 
##                     Shuang corrected me that the cost should be based on the real-world cost.
##                     the total cost of a TKI type (e.g., imatinib) for 2024 
##                     and divides it by the total number of patients using that TKI.
##          2024-11-21 Enoch Chen: Estimate TKI cost per year
## Project: CML cost  
## Purpose: To obtain average yearly cost per patient
## Output: Cost_02a_yearly_cpp_dist.rds
##         Cost_02a_yearly_cpp.rds
## FIXME: The following costs for blood test, bone marrow test, chromosome analysis,
##        and qrt-pcr are based on data in 2024 only.

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}

setwd(wd)

###################
## Read packages ##
###################
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)  
library(ggplot2)

####################
## CPI conversion ##
####################
## Data source: https://www.scb.se/en/finding-statistics/statistics-by-subject-area/prices-and-consumption/consumer-price-index/consumer-price-index-cpi/pong/tables-and-graphs/consumer-price-index-cpi/cpi-fixed-index-numbers-1980100/
## CPI 
cpi_2024 <- 414

## Make cpi_2025 to cpi_2030 same as cpi_2024
cpi_matrix <- as.data.frame(list(c(2007:2030),
                                 c(290.51, 300.61, 299.66, 303.46, 311.43, 
                                   314.20, 314.06, 313.49, 313.35, 316.43, 
                                   322.11, 328.40, 334.26, 335.92, 343.19, 
                                   371.91, 403.70, 414.00, 414.00, 414.00, 
                                   414.00, 414.00, 414.00, 414.00)),
                            col.names=c("year", "CPI"))
cpi_2024to2024 <- 1
cpi_conversion_2024_as_numerator <- cpi_2024/cpi_matrix$CPI
cpi_matrix <- cbind(cpi_matrix, cpi_2024to2024, cpi_conversion_2024_as_numerator) |>
              dplyr::select(-CPI)

########################
## SEK-USD conversion ##
########################
## Data source: https://www.riksbank.se/en-gb/statistics/interest-rates-and-exchange-rates/search-annual-and-monthly-average-exchange-rates/?a=Y&y=2024&m=Select+month&s=g130-SEKUSDPMI&fs=3#result-section
## Annual Average 2024
USD_SEK_conversion <- 10.56

###############
## Read data ##
###############
studypop <- readRDS(df, file = "../Data/Cost_01a_studypop.rds")
table(studypop$item)

##########################################################
## Drop the records after progression, alloSCT, and TFR ##
##########################################################
# Flag items to drop first
# Then we obtain all the records before progression, alloSCT, and TFR 
studypop1 <- studypop |>
             group_by(idnr) |>
             mutate(drop_flag = cumsum(item %in% c("Progress to AP/BP", "SCT", "TFR starts"))) |>
             filter(drop_flag == 0) |>
             dplyr::select(-drop_flag)

###########################################
# See the composition of each line of TKI #
###########################################
# Flag the first and last date
studypop2 <- studypop1 |>
             group_by(idnr, line) |>
             mutate(date_item_first = min(date_item), 
                    date_item_last = max(date_item)) |>
             ungroup()
head(studypop2)

# Filter data for 2007–2018 (excluding "End")
studypop2_2007_2018 <- studypop2 |>
                       filter(item != "End") |>
                       filter(year(date_item) >= 2007, year(date_item) <= 2018) |>
                       mutate(year = year(date_item))

# Count distribution by year, line (1L/2L/3L+), and item (TKI)
dist_all <- studypop2_2007_2018 |>
            group_by(year, line, item) |>
            summarise(n = n(), .groups = "drop")

# Calculate proportions within each year and line
dist_all <- dist_all |>
            group_by(year, line) |>
            mutate(prop = n / sum(n)) |>
            ungroup()

ggplot(dist_all, aes(x = year, y = prop, fill = item)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ line, ncol = 1) +
  labs(
    x = "Year",
    y = "Proportion",
    fill = "TKI",
    title = "Distribution of TKIs by Line and Year (2007–2018)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# saveRDS(dist_all, file = "../Data/Cost_02a_yearly_cpp_dist.rds")

#####################################
# Average yearly costs per patient  #
#####################################
# Generate columns called c_year from c_2007 to c_2018
studypop3 <-  studypop2 |>
              mutate(c_2007 = 0, 
                     c_2008 = 0, 
                     c_2009 = 0, 
                     c_2010 = 0, 
                     c_2011 = 0, 
                     c_2012 = 0, 
                     c_2013 = 0, 
                     c_2014 = 0, 
                     c_2015 = 0, 
                     c_2016 = 0, 
                     c_2017 = 0, 
                     c_2018 = 0)
head(studypop3)

# Replace c_2007 to c_2018 with the corresponding cost
studypop3 <- studypop3 |>
             mutate(year = format(date_item, "c_%Y")) |>
             mutate(across(starts_with("c_"), ~ if_else(cur_column() == year, cost_lan, .))) |>
             dplyr::select(-year)

##############################################
## Read price change data for TKI in Sweden ##
##############################################
## Need to find out the distribution of patients by TKI
studypop4_2018 <- studypop2 |>
  filter(item!="End") |>
  # Only include the last record of year 2018 of each indr
  group_by(idnr) |>
  filter(date_item == max(date_item)) |>
  filter(lubridate::year(date_item) == 2018)

studypop4_2018_1L <- subset(studypop4_2018, line == "1L" & lubridate::year(date_item) == 2018)

# Calculate the distribution of item
dist_1L_2018 <- prop.table(table(studypop4_2018_1L$item))
print(dist_1L_2018)
dist_1L_2018_df <- tibble(tki = names(dist_1L_2018), prop_1L_2018 = as.numeric(dist_1L_2018))

studypop4_2018_2L <- subset(studypop4_2018, line == "2L" & lubridate::year(date_item) == 2018)
dist_2L_2018 <- prop.table(table(studypop4_2018_2L$item))
print(dist_2L_2018)
dist_2L_2018_df <- tibble(tki = names(dist_2L_2018), prop_2L_2018 = as.numeric(dist_2L_2018))

studypop4_2018_3L <- subset(studypop4_2018, line == "3L+" & lubridate::year(date_item) == 2018)
dist_3L_2018 <- prop.table(table(studypop4_2018_3L$item))
print(dist_3L_2018)
dist_3L_2018_df <- tibble(tki = names(dist_3L_2018), prop_3L_2018 = as.numeric(dist_3L_2018))


# Read TLV data
setwd(wd)
tlv_sublev <- readRDS("../Data/Cost_00_tki_sublev.rds") |>
  mutate(tki = case_when(
    tki == "bosutinib" ~ "BOS",
    tki == "dasatinib" ~ "DAS",
    tki == "imatinib" ~ "IMA",
    tki == "nilotinib" ~ "NIL",
    tki == "ponatinib" ~ "PON",
    TRUE ~ tki)) |>
  mutate(price_prop = price_change + 1) |>
  dplyr::select(-price_change)

full_years <- tibble(year = as.character(2018:2030))

tlv_sublev2 <- tlv_sublev |>
  distinct(tki) |>
  crossing(full_years) |>  # Create all combinations of tki and year
  left_join(tlv_sublev, by = c("tki", "year")) |>
  arrange(tki, year)              

# By tki, if that year price is NA, then take the previous year price
tlv_sublev3 <- tlv_sublev2 |>
  group_by(tki) |>
  fill(price_prop, .direction = "down") |>
  ungroup()

# Merge with distribution of 1L, 2L, and 3L+ TKI in 2018
tlv_sublev4 <- tlv_sublev3 |>
  left_join(dist_1L_2018_df, by = "tki") |>
  left_join(dist_2L_2018_df, by = "tki") |>
  left_join(dist_3L_2018_df, by = "tki") |>
  # all NA with 0
  mutate(across(starts_with("prop"), ~ ifelse(is.na(.x), 0, .x)))

# Average price change by year
price_change_tki <- tlv_sublev4 |>
  group_by(year) |>
  summarise(
    price_prop_1L = sum(price_prop * prop_1L_2018),
    price_prop_2L = sum(price_prop * prop_2L_2018),
    price_prop_3L = sum(price_prop * prop_3L_2018))

print(price_change_tki)

price_change_tki <-  price_change_tki|>
  pivot_longer(
    cols = starts_with("price_prop_"), 
    names_to = "line", 
    names_prefix = "price_prop_",
    values_to = "cpp_prop")|>
  mutate(line = case_when(
    line == "1L" ~ "1L",
    line == "2L" ~ "2L",
    line == "3L" ~ "3L+"),
    year = as.integer(year))

## So here we have all the empirical changes in price (relative to 2018) 
## for each TKI until 2025 Spring
## We assumed that the price remains from 2025 Spring to 2030

###########################################################################
# Calculate average yearly cost per patient by substance level (each TKI) #
###########################################################################
c_annual_byidnr_tki <- studypop3 |>
                       group_by(idnr, line, item) |>
                       summarise(across(starts_with("c_"), sum, na.rm = TRUE),
                                 date_item_first = first(date_item_first),
                                 date_item_last = first(date_item_last)) |>
                       # For all the cost 0, make it NA; it will then not be considered 
                       # in the calculation of the mean
                       mutate(across(starts_with("c_"), ~ if_else(.x == 0, NA, .x))) |>
                       dplyr::select(idnr, date_item_first, date_item_last, everything()) |>
                       arrange(idnr, date_item_first) |>
                       filter(item != "End")
head(c_annual_byidnr_tki)

# Yearly costs per patient by tki
c_annual_pp_tki <- c_annual_byidnr_tki |>
                   group_by(line, item) |>
                   summarise(across(starts_with("c_"), mean, na.rm = TRUE), .groups = "drop")

# Print the result
print(c_annual_pp_tki)

# Set all the NA to 0
c_annual_pp_tki[is.na(c_annual_pp_tki)] <- 0

# Make it into dataframe by tki: year, cost
c_annual_pp_tki_long <-  c_annual_pp_tki |>
                          pivot_longer(
                            cols = starts_with("c_"),
                            names_to = "year",
                            names_prefix = "c_",
                            values_to = "cpp"
                          ) |>
                          arrange(line, item, year) |>
                          mutate(year = as.numeric(year))

c_annual_pp_tki_long

# Create new years for cost assumptions
c_annual_pp_tki_long3 <- c_annual_pp_tki_long |>
                         filter(year == 2018, 
                                line %in% c("1L", "2L", "3L+"),
                                item %in% c("IMA", "DAS", "NIL", "BOS", "PON")) |>
                         slice(rep(1:n(), each = length(2019:2030))) |>
                         mutate(year = rep(2019:2030, times = n() / length(2019:2030)),
                                cpp = 0)

# Combine the two data frames
# The following dataframe consists of average TKI price by year
# 2007 to 2018 were empirical data from the Swedish CML register
# 2019 to 2030 were zeros
c_annual_pp_tki_long4 <- rbind(c_annual_pp_tki_long, c_annual_pp_tki_long3) |>
                         arrange(line, item, year) |>
                         rename(tki = item) |>
                         mutate(year = as.character(year))

# Merge with price change using the TLV database
c_annual_pp_tki_long6 <- left_join(c_annual_pp_tki_long4, tlv_sublev3, by = c("tki", "year")) |>
                         mutate(year = as.integer(year),
                                price_prop = if_else(is.na(price_prop), 1, price_prop))

# Mutate the data frame
c_annual_pp_tki_long7 <- c_annual_pp_tki_long6 |>
                         group_by(line, tki) |>
                         mutate(cpp_2018 = cpp[year == 2018][1],
                                cpp = if_else(year >= 2019, cpp_2018, cpp),
                                ncpp = cpp * price_prop) |>
                         select(-cpp_2018) |>
                         rename(item = tki)

# Merge to get proportion 
c_annual_pp_tki_long8 <- left_join(c_annual_pp_tki_long7, dist_all, by = c("line", "item", "year")) |>
                         select(line, item, year, cpp, price_prop, ncpp, prop)

# saveRDS(c_annual_pp_tki_long8, file = "../Data/Cost_02a_yearly_cpp_tki.rds")

###################################################################################
# Calculate average yearly cost per patient by substance level (each LINE of TKI) #
###################################################################################
c_annual_byidnr_line <- studypop3 |>
                        group_by(idnr, line) |>
                        summarise(across(starts_with("c_"), sum, na.rm = TRUE),
                                  date_item_first = first(date_item_first),
                                  date_item_last = first(date_item_last)) |>
                        # For all the cost 0, make it NA; it will then not be considered 
                        # in the calculation of the mean
                        mutate(across(starts_with("c_"), ~ if_else(.x == 0, NA, .x))) |>
                        dplyr::select(idnr, date_item_first, date_item_last, everything()) |>
                        arrange(idnr, date_item_first)
head(c_annual_byidnr_line)

# Yearly costs per patient by line
c_annual_pp_line <- c_annual_byidnr_line |>
                    group_by(line) |>
                    summarise(across(starts_with("c_"), mean, na.rm = TRUE), .groups = "drop")|>
                    # drop if line = "NA"
                    filter(line != "NA")

# Print the result
print(c_annual_pp_line)

# Set all the NA to 0
c_annual_pp_line[is.na(c_annual_pp_line)] <- 0

# Make it into dataframe by tki: year, cost
c_annual_pp_line_long <-  c_annual_pp_line |>
                          pivot_longer(
                            cols = starts_with("c_"),
                            names_to = "year",
                            names_prefix = "c_",
                            values_to = "cpp"
                          ) |>
                          arrange(line, year) |>
                          mutate(year = as.numeric(year))

c_annual_pp_line_long

# Create new years for cost assumptions
c_annual_pp_line_long3 <- c_annual_pp_line_long |>
                          filter(year == 2018, line %in% c("1L", "2L", "3L+")) |>
                          slice(rep(1:n(), each = length(2019:2030))) |>
                          mutate(year = rep(2019:2030, times = n() / length(2019:2030)),
                                 cpp = 0)

# Combine the two data frames
# The following dataframe consists of average TKI price by year
# 2007 to 2018 were empirical data from the Swedish CML register
# 2019 to 2030 were zeros
c_annual_pp_line_long4 <- rbind(c_annual_pp_line_long, c_annual_pp_line_long3) |>
                          arrange(line, year)




################################################################################
## Add 2018 price to the data
c_annual_pp_line_long5 <- c_annual_pp_line_long4 |>
                          mutate(
                            cpp = case_when(
                            line == "1L" & year >= 2019 & year <= 2030 ~ cpp[line == "1L" & year == 2018],
                            line == "2L" & year >= 2019 & year <= 2030 ~ cpp[line == "2L" & year == 2018],
                            line == "3L+" & year >= 2019 & year <= 2030 ~ cpp[line == "3L+" & year == 2018],
                            TRUE ~ cpp))

# Merge with price change using the TLV database
c_annual_pp_line_long6 <- left_join(c_annual_pp_line_long5, price_change_tki, by = c("line", "year")) |>
                          mutate(cpp_prop = if_else(is.na(cpp_prop), 1, cpp_prop),
                                 ncpp = cpp * cpp_prop) 

# Mutate the data frame
c_annual_pp_line_long7 <-  c_annual_pp_line_long6 |>
                           mutate(
                              cpp_tki_1L = if_else(line == "1L", ncpp, NA_real_),
                              cpp_tki_2L = if_else(line == "2L", ncpp, NA_real_),
                              cpp_tki_3L = if_else(line == "3L+", ncpp, NA_real_)) |>
                           dplyr::select(year, cpp_tki_1L, cpp_tki_2L, cpp_tki_3L)

c_annual_pp_line_wide <-  c_annual_pp_line_long7 |>
                          group_by(year) |>
                          summarise(
                            cpp_tki_1L = sum(cpp_tki_1L, na.rm = TRUE),
                            cpp_tki_2L = sum(cpp_tki_2L, na.rm = TRUE),
                            cpp_tki_3L = sum(cpp_tki_3L, na.rm = TRUE)
                          )

####################################################
## Cost per patient for other resource use in SEK ##
####################################################
## FIXME Time dependent costs
# Here we want to make it time dependent
years <- 2007:2030

# FIXME: The following costs for blood test, bone marrow test, chromosome analysis, 
#        and qrt-pcr are based on data in 2024. 
c_bloodtest <- c(rep(0,8), rep(106, 16))
names(c_bloodtest) <- years

c_bm <- c(rep(0,8), rep(2271, 16))
names(c_bm) <- years

c_chrom <- c(rep(0,8), rep(11952, 16))
names(c_chrom) <- years

c_qrtpcr <- c(rep(0,8), rep(5777, 16))
names(c_qrtpcr) <- years

c_visit <- c(
  rep(0,8), 
  1232.00, 1264.00, 1300.00, 1350.00, 1478.00, 
  1515.00, 1560.00, 1870.00, 2061.00, 2599.00, 2560.00,
  rep(2560.00, 5) # 2026 to 2030 remains at the 2025 level
)
names(c_visit) <- years

c_conference <- c(
  rep(0,8), 
  11307.00, 11601.00, 11903.00, 9247.00, 10123.00, 
  10377.00, 10690.00, 10979.00, 11402.00, 12093.00, 11912.00,
  rep(11912.00, 5) # 2026 to 2030 remains at the 2025 level
)
names(c_conference) <- years

# TKI
# Ohm's price list
cpp_tki_others_1y <- c_bloodtest*20 + c_bm*4 + c_chrom*4 + c_qrtpcr*4 + c_visit*4
cpp_tki_others_latery <- c_bloodtest*4 + c_bm*0 + c_chrom*0 + c_qrtpcr*4 + c_visit*2
cpp_tki_others <- (cpp_tki_others_1y + cpp_tki_others_latery) / 2

cost_matrix <- cpi_matrix
cost_matrix$cpp_tki_others <- cpp_tki_others * (cpi_matrix$cpi_conversion_2024_as_numerator)

# Progression
# Hernlund 2021. The original price was reported in 2018 SEK, here I inflated to 2024 SEK already.
cpp_prog_2018 <- 208119
years <- 2007:2030

cpp_prog_ratios <- c(
  rep(0,8),        # 2007 to 2014
  0.74, 0.91, 0.93, 1.00, 1.05,  
  1.10, 1.12, 1.15, 1.25, 1.34, 1.07,
  1.07, 1.07, 1.07, 1.07, 1.07   # 2026 to 2030 remains at the 2025 level
)
names(cpp_prog_ratios) <- years
cpp_prog <- cpp_prog_2018 * cpp_prog_ratios
cost_matrix$cpp_prog <- cpp_prog * (cpi_matrix$cpi_conversion_2024_as_numerator)

# TFR
# ESMO guideline (2017) suggested: 
# blood tests every 3 months, 
# qRT-PCR 1-6 months: monthly, 7-12 months: every six weeks, 13+ months: every 3 months
# Swedish guideline gave the same suggestions: https://kunskapsbanken.cancercentrum.se/diagnoser/kml/vardprogram/forsok-att-avsluta-tki-behandling/
# Leif Stenke: specialist meeting if rising BCR::ABL1 levels
# I therefore assumed the following:
cpp_tfr <- c_bloodtest*4 + c_bm*0 + c_chrom*0 + c_qrtpcr*8 + c_visit*2 + c_conference*1
cost_matrix$cpp_tfr <- cpp_tfr * (cpi_matrix$cpi_conversion_2024_as_numerator)

# AlloSCT 1st year
# FIXME: Use instead: Stamcellstransplantation, allogen vuxen
cpp_alloSCT_1y_2018 <- 1385100
cpp_alloSCT_ratios  <- c(rep(0, 8),  # 2007 to 2014
                         0.92, 0.94, 0.97, 1.00, 1.09, 
                         1.12, 1.16, 1.50, 1.56, 1.65, 1.61,
                         rep(1.61, 5))          # 2026 to 2030 remains at the 2025 level
names(cpp_alloSCT_ratios) <- years
cpp_alloSCT_1y <- cpp_alloSCT_1y_2018 * cpp_alloSCT_ratios
cost_matrix$cpp_alloSCT_1y <- cpp_alloSCT_1y * (cpi_matrix$cpi_conversion_2024_as_numerator)

# AlloSCT 2nd+ year
# 2025-02-18 Update: The average yearly cost excluding the alloSCT procedure
#                    Previously I included even sick leave and early retirement,
#                    which was wrong, since this study uses a healthcare sector perspective.
cpp_alloSCT_2y_2018 <- 82080
cpp_alloSCT_2y <- cpp_alloSCT_2y_2018 * cpp_alloSCT_ratios
cost_matrix$cpp_alloSCT_2y <- cpp_alloSCT_2y * (cpi_matrix$cpi_conversion_2024_as_numerator)

#########################################################################
# Merge with CPI to account for inflation and currency conversion      ##
# Caution: Discounting was NOT considered here but on the stacked plot ##
#########################################################################
c_annual_pp_line_adj <- merge(c_annual_pp_line_wide, cost_matrix, by.y = "year") |>
                        mutate(cpp_tki_1L_total = cpp_tki_1L*cpi_conversion_2024_as_numerator + cpp_tki_others,
                               cpp_tki_2L_total = cpp_tki_2L*cpi_conversion_2024_as_numerator + cpp_tki_others,
                               cpp_tki_3L_total = cpp_tki_3L*cpi_conversion_2024_as_numerator + cpp_tki_others
                               ) |>
                        dplyr::select(-c("cpi_2024to2024", "cpi_conversion_2024_as_numerator")) |>
                        # Convert all the cost variables (beginning with cpp_ or c_) to USD
                        mutate(across(starts_with("cpp_") | starts_with("c_"), ~ round(.x/USD_SEK_conversion, 0)))

# Save the data frame
# saveRDS(c_annual_pp_line_adj, file = "../Data/Cost_02a_yearly_cpp.rds")
