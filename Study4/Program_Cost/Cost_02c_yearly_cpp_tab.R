## File name: Cost_02c_yearly_cpp_tab.R
## Project: CML cost  
## Purpose: To obtain average yearly cost per patient in a tabulated format
## Output: Cost_02c_yearly_cpp_tab.docx

# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}

setwd(wd)

# Read the orginal cost calculations
source("Cost_02a_yearly_cpp.R")

###################
## Read packages ##
###################
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)  
library(tibble)
library(flextable)

# Create an empty data frame 
# First I want a data frame with item and 2015 to 2030 as colnames

df <- data.frame(matrix(ncol = 17, nrow = 0))
colname <- c("item", "`2015`", "`2016`", "`2017`", "`2018`", "`2019`", "`2020`", 
             "`2021`", "`2022`", "`2023`", "`2024`", "`2025`", "`2026`", 
             "`2027`", "`2028`", "`2029`", "`2030`")
colnames(df) <- colname

# CPI
cpi_matrix <- data.frame(
  item = "CPI (1980=100)",
  `2015` = 313.35, `2016` = 316.43, `2017` = 322.11, `2018` = 328.40,
  `2019` = 334.26, `2020` = 335.92, `2021` = 343.19, `2022` = 371.91,
  `2023` = 403.70, `2024` = 414.00, `2025` = 414.00, `2026` = 414.00,
  `2027` = 414.00, `2028` = 414.00, `2029` = 414.00, `2030` = 414.00
)
colnames(cpi_matrix) <- colname

############
## 1L TKI ##
############
# 1L TKI treatment
ncpp_tki1 <- c_annual_pp_line_long6 |>
             filter(line == "1L" & year >= 2015) |>
             select(line, year, ncpp) |>
             pivot_wider(names_from = year, values_from = ncpp) |>
             rename(item = line) |>
             as.data.frame() |>
             mutate(across(-item, ~ round(.x, 0)),
                    item = "TKI (SEK)") 
colnames(ncpp_tki1) <- colname
print(ncpp_tki1)

# 1L TKI ratio relative to 2018
cppratio_tki1 <- c_annual_pp_line_long6 |>
                 filter(line == "1L" & year >= 2015) |>
                 select(line, year, cpp_prop) |>
                 mutate(cpp_prop = ifelse(year <= 2017, NA, cpp_prop)) |>
                 pivot_wider(names_from = year, values_from = cpp_prop) |>
                 rename(item = line) |>
                 as.data.frame() |>
                 mutate(across(-item, ~ round(.x, 2))) |>
                 mutate(item = "Ratio to 2018")
colnames(cppratio_tki1) <- colname
print(cppratio_tki1)


# TKI others (resource use)
cpp_tki_others_filtered <- cpp_tki_others[as.integer(names(cpp_tki_others)) >= 2015]

cpp_tki_others_df <- as.data.frame(t(cpp_tki_others_filtered)) |>
                     mutate(item = "Resource use (SEK)") |>
                     select(item, everything()) |>
                     mutate(across(-item, ~ round(.x, 0)))
colnames(cpp_tki_others_df) <- colname
print(cpp_tki_others_df)

# TKI total (TKI + resource use)
ncpp_tki1_total <- ncpp_tki1[,-1] + cpp_tki_others_df[,-1]
ncpp_tki1_total <- ncpp_tki1_total|> mutate(item = "Total (SEK)") |> select(item, everything())

# TKI total inflated to 2024 price usign CPI  
ncpp_tki1_total_2024 <- ncpp_tki1_total[-1]*414 / cpi_matrix[-1] 
ncpp_tki1_total_2024 <- ncpp_tki1_total_2024 |> mutate(item = "Total (2024 SEK)")

# TKI total inflated to 2024 price in USD
ncpp_tki1_total_2024_USD <- ncpp_tki1_total_2024 |> 
                            mutate(item = "Total (2024 USD)") |>
                            mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

############
## 2L TKI ##
############
# 2L TKI treatment
ncpp_tki2 <- c_annual_pp_line_long6 |>
             filter(line == "2L" & year >= 2015) |>
             select(line, year, ncpp) |>
             pivot_wider(names_from = year, values_from = ncpp) |>
             rename(item = line) |>
             as.data.frame() |>
             mutate(across(-item, ~ round(.x, 0)),
                    item = "TKI (SEK)") 
colnames(ncpp_tki2) <- colname
print(ncpp_tki2)

# 2L TKI ratio relative to 2018
cppratio_tki2 <- c_annual_pp_line_long6 |>
                 filter(line == "2L" & year >= 2015) |>
                 select(line, year, cpp_prop) |>
                 mutate(cpp_prop = ifelse(year <= 2017, NA, cpp_prop)) |>
                 pivot_wider(names_from = year, values_from = cpp_prop) |>
                 rename(item = line) |>
                 as.data.frame() |>
                 mutate(across(-item, ~ round(.x, 2))) |>
                 mutate(item = "Ratio to 2018")
colnames(cppratio_tki2) <- colname
print(cppratio_tki2)

# TKI others (resource use)
cpp_tki_others_filtered <- cpp_tki_others[as.integer(names(cpp_tki_others)) >= 2015]

cpp_tki_others_df <- as.data.frame(t(cpp_tki_others_filtered)) |>
                     mutate(item = "Resource use (SEK)") |>
                     select(item, everything()) |>
                     mutate(across(-item, ~ round(.x, 0)))
colnames(cpp_tki_others_df) <- colname
print(cpp_tki_others_df)

# TKI total (TKI + resource use)
ncpp_tki2_total <- ncpp_tki2[,-1] + cpp_tki_others_df[,-1]
ncpp_tki2_total <- ncpp_tki2_total|> mutate(item = "Total (SEK)") |> select(item, everything())

# TKI total inflated to 2024 price usign CPI  
ncpp_tki2_total_2024 <- ncpp_tki2_total[-1]*414 / cpi_matrix[-1] 
ncpp_tki2_total_2024 <- ncpp_tki2_total_2024 |> mutate(item = "Total (2024 SEK)")

# TKI total inflated to 2024 price in USD
ncpp_tki2_total_2024_USD <- ncpp_tki2_total_2024 |> 
                            mutate(item = "Total (2024 USD)") |>
                            mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

############
## 3L TKI ##
############
# 3L TKI treatment
ncpp_tki3 <- c_annual_pp_line_long6 |>
             filter(line == "3L+" & year >= 2015) |>
             select(line, year, ncpp) |>
             pivot_wider(names_from = year, values_from = ncpp) |>
             rename(item = line) |>
             as.data.frame() |>
             mutate(across(-item, ~ round(.x, 0)),
                    item = "TKI (SEK)") 
colnames(ncpp_tki3) <- colname
print(ncpp_tki3)

# 3L TKI ratio relative to 2018
cppratio_tki3 <- c_annual_pp_line_long6 |>
                 filter(line == "3L+" & year >= 2015) |>
                 select(line, year, cpp_prop) |>
                 mutate(cpp_prop = ifelse(year <= 2017, NA, cpp_prop)) |>
                 pivot_wider(names_from = year, values_from = cpp_prop) |>
                 rename(item = line) |>
                 as.data.frame() |>
                 mutate(across(-item, ~ round(.x, 2))) |>
                 mutate(item = "Ratio to 2018")
colnames(cppratio_tki3) <- colname
print(cppratio_tki3)

cpp_tki_others_filtered <- cpp_tki_others[as.integer(names(cpp_tki_others)) >= 2015]

# TKI others (resource use)
cpp_tki_others_df <- as.data.frame(t(cpp_tki_others_filtered)) |>
                     mutate(item = "Resource use (SEK)") |>
                     select(item, everything()) |>
                     mutate(across(-item, ~ round(.x, 0)))
colnames(cpp_tki_others_df) <- colname
print(cpp_tki_others_df)

# TKI total (TKI + resource use)
ncpp_tki3_total <- ncpp_tki3[,-1] + cpp_tki_others_df[,-1]
ncpp_tki3_total <- ncpp_tki3_total|> mutate(item = "Total (SEK)") |> select(item, everything())

# TKI total inflated to 2024 price usign CPI  
ncpp_tki3_total_2024 <- ncpp_tki3_total[-1]*414 / cpi_matrix[-1] 
ncpp_tki3_total_2024 <- ncpp_tki3_total_2024 |> mutate(item = "Total (2024 SEK)")

# TKI total inflated to 2024 price in USD
ncpp_tki3_total_2024_USD <- ncpp_tki3_total_2024 |> 
                            mutate(item = "Total (2024 USD)") |>
                            mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

#################
## Progression ##
#################
cppratio_prog <- as.data.frame(cpp_prog_ratios) |>
                 rownames_to_column(var = "year") |>
                 filter(year >= 2015) |>
                 pivot_wider(names_from = year, values_from = cpp_prog_ratios) |>
                 mutate(across(everything(), ~ round(.x, 2)),
                        item = "Ratio to 2018") |>
                 select(item, everything()) |>
                 as.data.frame()
colnames(cppratio_prog) <- colname
ncpp_prog <- as.data.frame(t(c("Aggregate costs", rep(as.numeric(NA), 3), cpp_prog_2018, rep(as.numeric(NA), 12))))
colnames(ncpp_prog) <- colname
ncpp_prog[ , -1] <- lapply(ncpp_prog[ , -1], function(x) as.numeric(x))

ncpp_prog_total <- cpp_prog_2018*cppratio_prog[,-1] 
ncpp_prog_total <- ncpp_prog_total |> mutate(item = "Total (SEK)") |> select(item, everything())                 
ncpp_prog_total_2024 <- ncpp_prog_total[-1]*414 / cpi_matrix[-1] 
ncpp_prog_total_2024 <- ncpp_prog_total_2024 |> mutate(item = "Total (2024 SEK)")
ncpp_prog_total_2024_USD <- ncpp_prog_total_2024 |> 
                            mutate(item = "Total (2024 USD)") |>
                            mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

################
## AlloSCT 1y ##
################
cppratio_alloSCT <- as.data.frame(cpp_alloSCT_ratios) |>
                 rownames_to_column(var = "year") |>
                 filter(year >= 2015) |>
                 pivot_wider(names_from = year, values_from = cpp_alloSCT_ratios) |>
                 mutate(across(everything(), ~ round(.x, 2)),
                        item = "Ratio to 2018") |>
                 select(item, everything()) |>
                 as.data.frame()
colnames(cppratio_alloSCT) <- colname

ncpp_alloSCT_1y <- as.data.frame(t(c("Aggregate costs", rep(as.numeric(NA), 3), cpp_alloSCT_1y_2018, rep(as.numeric(NA), 12))))
colnames(ncpp_alloSCT_1y) <- colname
ncpp_alloSCT_1y[ , -1] <- lapply(ncpp_alloSCT_1y[ , -1], function(x) as.numeric(x))

ncpp_alloSCT_1y_total <- cpp_alloSCT_1y_2018*cppratio_alloSCT[,-1] 
ncpp_alloSCT_1y_total <- ncpp_alloSCT_1y_total |> mutate(item = "Total (SEK)") |> select(item, everything())                 
ncpp_alloSCT_1y_total_2024 <- ncpp_alloSCT_1y_total[-1]*414 / cpi_matrix[-1] 
ncpp_alloSCT_1y_total_2024 <- ncpp_alloSCT_1y_total_2024 |> mutate(item = "Total (2024 SEK)")
ncpp_alloSCT_1y_total_2024_USD <- ncpp_alloSCT_1y_total_2024 |> 
                                  mutate(item = "Total (2024 USD)") |>
                                  mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

################
## AlloSCT 2y ##
################
ncpp_alloSCT_2y <- as.data.frame(t(c("Aggregate costs", rep(as.numeric(NA), 3), cpp_alloSCT_2y_2018, 
                                     rep(as.numeric(NA), 12))))
colnames(ncpp_alloSCT_2y) <- colname
ncpp_alloSCT_2y[ , -1] <- lapply(ncpp_alloSCT_2y[ , -1], function(x) as.numeric(x))

ncpp_alloSCT_2y_total <- cpp_alloSCT_2y_2018*cppratio_alloSCT[,-1] 
ncpp_alloSCT_2y_total <- ncpp_alloSCT_2y_total |> mutate(item = "Total (SEK)") |> select(item, everything())                 
ncpp_alloSCT_2y_total_2024 <- ncpp_alloSCT_2y_total[-1]*414 / cpi_matrix[-1] 
ncpp_alloSCT_2y_total_2024 <- ncpp_alloSCT_2y_total_2024 |> mutate(item = "Total (2024 SEK)")
ncpp_alloSCT_2y_total_2024_USD <- ncpp_alloSCT_2y_total_2024 |> 
                                  mutate(item = "Total (2024 USD)") |>
                                  mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

#########
## TFR ##
#########
ncpp_tfr_total <- as.data.frame(cpp_tfr) |> 
                  rownames_to_column(var = "year") |>
                  filter(year >= 2015) |>
                  pivot_wider(names_from = year, values_from = cpp_tfr) |>
                  mutate(across(everything(), ~ round(.x, 2))) |>
                  as.data.frame()

ncpp_tfr_total <- ncpp_tfr_total |> mutate(item = "Total (SEK)") |> select(item, everything())
colnames(ncpp_tfr_total) <- colname

ncpp_tfr_total_2024 <- ncpp_tfr_total[-1]*414 / cpi_matrix[-1] 
ncpp_tfr_total_2024 <- ncpp_tfr_total_2024 |> mutate(item = "Total (2024 SEK)")
ncpp_tfr_total_2024_USD <- ncpp_tfr_total_2024 |> 
                           mutate(item = "Total (2024 USD)") |>
                           mutate(dplyr::across(-item, ~ round(.x / USD_SEK_conversion, 0)))

########################
## Create empty names ##
########################
name_tki1 <- setNames(as.data.frame(t(c("1L TKI", rep(NA_real_, 16)))), colname)
name_tki2 <- setNames(as.data.frame(t(c("2L TKI", rep(NA_real_, 16)))), colname)
name_tki3 <- setNames(as.data.frame(t(c("3L+ TKI", rep(NA_real_, 16)))), colname)
name_prog <- setNames(as.data.frame(t(c("Progression", rep(NA_real_, 16)))), colname)
name_alloSCT_1y <- setNames(as.data.frame(t(c("AlloSCT, 1st year", rep(NA_real_, 16)))), colname)
name_alloSCT_2y <- setNames(as.data.frame(t(c("AlloSCT, 2nd year+", rep(NA_real_, 16)))), colname)
name_TFR <- setNames(as.data.frame(t(c("TFR", rep(NA_real_, 16)))), colname)

name_tki1[ , -1] <- lapply(name_tki1[ , -1], as.numeric)
name_tki2[ , -1] <- lapply(name_tki2[ , -1], as.numeric)
name_tki3[ , -1] <- lapply(name_tki3[ , -1], as.numeric)
name_prog[ , -1] <- lapply(name_prog[ , -1], as.numeric)
name_alloSCT_1y[ , -1] <- lapply(name_alloSCT_1y[ , -1], as.numeric)
name_alloSCT_2y[ , -1] <- lapply(name_alloSCT_2y[ , -1], as.numeric)
name_TFR[ , -1] <- lapply(name_TFR[ , -1], as.numeric)

df_new <- bind_rows(cpi_matrix, 
                    name_tki1,
                    ncpp_tki1, cppratio_tki1, cpp_tki_others_df, 
                    ncpp_tki1_total,
                    ncpp_tki1_total_2024, ncpp_tki1_total_2024_USD,
                    name_tki2,
                    ncpp_tki2, cppratio_tki2, cpp_tki_others_df, 
                    ncpp_tki2_total,
                    ncpp_tki2_total_2024, ncpp_tki2_total_2024_USD,
                    name_tki3,
                    ncpp_tki3, cppratio_tki3, cpp_tki_others_df, 
                    ncpp_tki3_total,
                    ncpp_tki3_total_2024, ncpp_tki3_total_2024_USD,
                    name_prog,
                    ncpp_prog,
                    cppratio_prog, ncpp_prog_total,
                    ncpp_prog_total_2024, ncpp_prog_total_2024_USD,
                    name_alloSCT_1y,
                    ncpp_alloSCT_1y,
                    cppratio_alloSCT, ncpp_alloSCT_1y_total,
                    ncpp_alloSCT_1y_total_2024, ncpp_alloSCT_1y_total_2024_USD,
                    name_alloSCT_2y,
                    ncpp_alloSCT_2y,
                    cppratio_alloSCT, ncpp_alloSCT_2y_total,
                    ncpp_alloSCT_2y_total_2024, ncpp_alloSCT_2y_total_2024_USD,
                    name_TFR,
                    ncpp_tfr_total,
                    ncpp_tfr_total_2024, ncpp_tfr_total_2024_USD) |>
                    mutate(across(-item, ~ ifelse(item == "Ratio to 2018", round(., 2), round(., 0))))
colnames(df_new) <- gsub("`", "", colnames(df_new))

print(df_new)
tail(c_annual_pp_line_adj)

ft <- flextable(df_new) |> 
      autofit() |>
      set_header_labels(
        item = "Item",
        `2015` = "2015", `2016` = "2016", `2017` = "2017", `2018` = "2018", `2019` = "2019",
        `2020` = "2020", `2021` = "2021", `2022` = "2022", `2023` = "2023", `2024` = "2024",
        `2025` = "2025", `2026` = "2026", `2027` = "2027", `2028` = "2028", `2029` = "2029",
        `2030` = "2030") |>
       colformat_num(j = -1, i = df_new$item != "Ratio to 2018", big.mark = " ", decimal.mark = ".", digits = 0) |>
       colformat_num(j = -1, i = df_new$item == "Ratio to 2018", big.mark = " ", decimal.mark = ".", digits = 2) |>
       align(align = c("right"), j = 1:16, part = "header") |>
       align(align = c("right"), j = 1:16, part = "body") |>
       align(align = c("left"), i = 1:44, j = 1, part = "body") |>
       bold(i = c(1,2,9,16,23,29,35,41), j = 1, bold = TRUE, part = "body") |>
       add_footer_lines("CPI, customer price index; 1L, first-line; 2L, second-line; 3L+, third-line and later; TKI, tyrosine kinase inhibitor; AlloSCT, allogeneic stem cell transplantation.")

ft    

# Save the data frame
# saveRDS
# saveRDS(df_new, file="../Data/Cost_02c_yearly_cpp_tab.rds")
# save_as_docx(ft, path="../Output/Cost_02c_yearly_cpp_tab.docx")

