## File name: Cost_02c1_yearly_cpp_resource_tab.R
## Project: CML cost  
## Purpose: To obtain average yearly cost per patient in resource use for 1L to 3L+ TKI, and TFR
## Output: Cost_02c1_yearly_cpp_resource_tab.docx

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

cpi_matrix

##########################
## Reosurce use list 
c_bloodtest
c_bm
c_chrom
c_qrtpcr
c_visit
c_conference
df_var <- bind_rows(
            c_bloodtest, c_bm, c_chrom, c_qrtpcr, c_visit, c_conference,
          ) |>
            mutate(item = c("Blood test", "Bone marrow", "Chromosome analysis",
                            "qRT-PCR", "Specialist visit", "Specialist conference")) |>
            select(item, `2015`:`2030`) |>
            as.data.frame()
colnames(df_var) <- colname

## Resource use for any line of TKI
cpp_tki_others_1y # c_bloodtest*20 + c_bm*4 + c_chrom*4 + c_qrtpcr*4 + c_visit*4
cpp_tki_others_latery # c_bloodtest*4 + c_bm*0 + c_chrom*0 + c_qrtpcr*4 + c_visit*2
cpp_tki_others # Mean of above 2 variables: (cpp_tki_others_1y + cpp_tki_others_latery) / 2
name_year1 <- setNames(as.data.frame(t(c(rep(NA_real_, 16)))), colname[-1]) |> lapply(as.numeric)
name_year2 <- setNames(as.data.frame(t(c(rep(NA_real_, 16)))), colname[-1]) |> lapply(as.numeric)

df_tki <- bind_rows(
          name_year1, cpp_tki_others_1y, 
          name_year2, cpp_tki_others_latery, 
          cpp_tki_others) |>
          mutate(item = c("Year 1 formula", "Year 1 (SEK)", "Year 2+ formula", "Year 2+ (SEK)", "Resource use (SEK)")) |>
          select(item, `2015`:`2030`) |>
          as.data.frame()
colnames(df_tki) <- colname

## Resource use for TFR
cpp_tfr 
name_tfrf <- setNames(as.data.frame(t(c(rep(NA_real_, 16)))), colname[-1]) |> lapply(as.numeric)

df_tfr <- bind_rows(name_tfrf, cpp_tfr) |>
          mutate(item = c("TFR formula", "Total (SEK)")) |>
          select(item, `2015`:`2030`) |>
          as.data.frame()
colnames(df_tfr) <- colname

# Empty rows
name_var <- setNames(as.data.frame(t(c("Price (SEK)", rep(NA_real_, 16)))), colname)
name_var[, -1] <- lapply(name_var[ , -1], as.numeric)
name_tki <- setNames(as.data.frame(t(c("1L, 2L, or 3L+ TKI", rep(NA_real_, 16)))), colname)
name_tki[, -1] <- lapply(name_var[ , -1], as.numeric)
name_tfr <- setNames(as.data.frame(t(c("TFR", rep(NA_real_, 16)))), colname)
name_tfr[, -1] <- lapply(name_tfr[ , -1], as.numeric)


# Bind all together
df_new <- bind_rows(
          name_var,
          df_var,
          name_tki,
          df_tki,
          name_tfr,
          df_tfr)
colnames(df_new) <- gsub("`", "", colnames(df_new))

ft <- flextable(df_new) |> 
      autofit() |>
      set_header_labels(
        item = "Item",
        `2015` = "2015", `2016` = "2016", `2017` = "2017", `2018` = "2018", `2019` = "2019",
        `2020` = "2020", `2021` = "2021", `2022` = "2022", `2023` = "2023", `2024` = "2024",
        `2025` = "2025", `2026` = "2026", `2027` = "2027", `2028` = "2028", `2029` = "2029",
        `2030` = "2030") |>
       colformat_num(j = -1, big.mark = " ", decimal.mark = ".", digits = 0) |>
       align(align = c("right"), j = 1:16, part = "header") |>
       align(align = c("right"), j = 1:16, part = "body") |>
       bold(i = ~ item == "Price (SEK)", j = "item", bold = TRUE, part = "body") |>
       bold(i = ~ item == "1L, 2L, or 3L+ TKI", j = "item", bold = TRUE, part = "body") |>
       bold(i = ~ item == "TFR", j = "item", bold = TRUE, part = "body") |>
       align(align = c("left"), i = c(1, 8, 14), j = 1, part = "body")
  
row_formula_year1 <- which(df_new$item == "Year 1 formula")
row_formula_year2 <- which(df_new$item == "Year 2+ formula")
row_formula_tfr <- which(df_new$item == "TFR formula")

ft <- ft |>
      merge_at(i = row_formula_year1, j = 2:ncol(df_new), part = "body") |>
      merge_at(i = row_formula_year2, j = 2:ncol(df_new), part = "body") |>
      merge_at(i = row_formula_tfr, j = 2:ncol(df_new), part = "body")
  

# 2) overwrite the merged cell (now in column 1) with the full text
ft <- ft |>
      compose(i = row_formula_year1, j = 2,
              value = as_paragraph("Blood test*20 + Bone marrow*4 + Chromosome analysis*4 + qRT-PCR*4 + Specialist visit*4"),
              part  = "body") |>
      align(i = row_formula_year1, j = 2, align = "left", part  = "body")|>
      compose(i = row_formula_year2, j = 2,
              value = as_paragraph("Blood test*4 + Bone marrow*0 + Chromosome analysis*0 + qRT-PCR*4 + Specialist visit*2"),
              part  = "body") |>
      align(i = row_formula_year2, j = 2, align = "left", part  = "body")|>
      compose(i = row_formula_tfr, j = 2,
              value = as_paragraph("Blood test*4 + Bone marrow*0 + Chromosome analysis*0 + qRT-PCR*8 + Specialist visit*2 + Specialist conference*1"),
              part  = "body") |>
      align(i = row_formula_tfr, j = 2, align = "left", part  = "body")

ft    

# Save the data frame
save_as_docx(ft, path="../Output/Cost_02c1_yearly_cpp_resource_tab.docx")

