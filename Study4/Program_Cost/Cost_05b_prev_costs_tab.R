## File name: Cost_05b_prev_costs_tab.R
## Updated:
## Project: CML Cost

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
}
setwd(wd)

###################
## Read packages ##
###################
library(tidyverse)
library(dplyr)
library(tidyr)
library(flextable)
library(stringr)
library(glue)

colname <- c("Item", "`2015`", "`2016`", "`2017`", "`2018`", "`2019`", "`2020`", 
             "`2021`", "`2022`", "`2023`", "`2024`", "`2025`", "`2026`", 
             "`2027`", "`2028`", "`2029`", "`2030`")

############
## PIAMOD ##
############

# Read the piamod output for retrieving the CIs
piamod <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
piamod_ALL <- piamod[piamod$SEX == 3 & piamod$AGE_GROUP == "0-99" & piamod$YEAR >= 2000,] |>
              dplyr::select(c("YEAR", "PREV.CASES", "PREV.CASES.lb", "PREV.CASES.ub")) |>
              filter(YEAR >= 2015) |>
              mutate(PREV.CASES_lb.ratio = PREV.CASES.lb/PREV.CASES,
                     PREV.CASES_ub.ratio = PREV.CASES.ub/PREV.CASES) |>
              dplyr::select(c("YEAR", "PREV.CASES_lb.ratio", "PREV.CASES_ub.ratio"))

piamod_TOTAL <- piamod[piamod$SEX == 3 & piamod$AGE_GROUP == "0-99" & piamod$YEAR >= 2015,] |>
                dplyr::select(c("YEAR", "PREV.CASES", "PREV.CASES.lb", "PREV.CASES.ub")) 

piamod_TOTAL <- piamod_TOTAL |>
                mutate(across(c(PREV.CASES, PREV.CASES.lb, PREV.CASES.ub), ~ round(.x, 1))) |>
                mutate(label = paste0(PREV.CASES, " (", PREV.CASES.lb, ", ", PREV.CASES.ub, ")")) |>
                select(YEAR, label) |>
                pivot_wider(names_from = YEAR, values_from = label) |>
                mutate(Item = "PREV.CASES") |>
                select(Item, everything())

colnames(piamod_TOTAL) <- colname

################
## PREV.CASES ##
################

# Read prevalent cases disributions
prev_dis <- read.csv(file="../Data/Cost_04a_prev_dist.csv", header = TRUE) |>
             mutate(YEAR = as.numeric(YEAR)) |>
             filter(YEAR >= 2015) |>
             dplyr::select(state, YEAR, PREV.CASES)|>
             rename(year = YEAR)
prev_dis <- prev_dis |>
             left_join(piamod_ALL, by = c("year" = "YEAR")) |>
             mutate(PREV.CASES_ub = PREV.CASES * PREV.CASES_ub.ratio,
                    PREV.CASES_lb = PREV.CASES * PREV.CASES_lb.ratio) |>
             dplyr::select(-PREV.CASES_lb.ratio, -PREV.CASES_ub.ratio) |>
             mutate(PREV.CASES = round(PREV.CASES, 1),
                    PREV.CASES_lb = round(PREV.CASES_lb, 1),
                    PREV.CASES_ub = round(PREV.CASES_ub, 1)) 

state_names <- c("Tki1" = "cpp_tki_1L_total", 
                 "Tki2" = "cpp_tki_2L_total", 
                 "Tki3" = "cpp_tki_3L_total", 
                 "Prog" = "cpp_prog", 
                 "Tfr" = "cpp_tfr",
                 "Sct_1y" = "cpp_alloSCT_1y",
                 "Sct_2y" = "cpp_alloSCT_2y")

prev_dis$state <- recode(prev_dis$state, !!!state_names)

###############
## COST DATA ##
###############

cost <- readRDS("../Data/Cost_02a_yearly_cpp.rds")
tail(cost)

cost_list <- lapply(c("cpp_tki_1L_total", "cpp_tki_2L_total", "cpp_tki_3L_total", 
                      "cpp_prog", "cpp_tfr", "cpp_alloSCT_1y", "cpp_alloSCT_2y"), 
                    function(col) {
                      data.frame(
                        year = cost$year,
                        state = col,
                        cpp = cost[[col]])
                    }
)

# Append all the lists into one dataframe
cost2 <- do.call(rbind, cost_list)

# Merge cost data with prevalent cases disributions
prev_cost <- prev_dis |>
             left_join(cost2, by = c("year", "state")) 

head(prev_cost)

# Deal with discounting
prev_cost2 <- prev_cost |>
              mutate(cpp_dis3 = case_when(
                year >= 2025 ~ round(cpp / (1 + 0.03)^(year - 2024), 0),
                TRUE ~ round(cpp, 0))) |>
              mutate(cpp_dis5 = case_when(
                year >= 2025 ~ round(cpp / (1 + 0.05)^(year - 2024), 0),
                TRUE ~ round(cpp, 0)))

############################
## TOTAL PREVALENCE COSTS ##
############################

prev_cost3 <- prev_cost2 |>
              mutate(prev_cost_dis3 = round(cpp_dis3 * PREV.CASES/1e6, 2),
                     prev_cost_dis3_ub = round(cpp_dis3 * PREV.CASES_ub/1e6, 2),
                     prev_cost_dis3_lb = round(cpp_dis3 * PREV.CASES_lb/1e6, 2),
                     prev_cost_dis5 = round(cpp_dis5 * PREV.CASES/1e6, 2),
                     prev_cost_dis5_ub = round(cpp_dis5 * PREV.CASES_ub/1e6, 2),
                     prev_cost_dis5_lb = round(cpp_dis5 * PREV.CASES_lb/1e6, 2),
                     prev_cost_dis0 = round(cpp * PREV.CASES/1e6, 2),
                     prev_cost_dis0_ub = round(cpp * PREV.CASES_ub/1e6, 2),
                     prev_cost_dis0_lb = round(cpp * PREV.CASES_lb/1e6, 2))

total_prev_cost3 <- prev_cost3|>
                    group_by(year) |>
                    summarise(total_prev_cost_dis3_USD = sum(prev_cost_dis3, na.rm = TRUE),
                              total_prev_cost_dis3_ub_USD = sum(prev_cost_dis3_ub, na.rm = TRUE),
                              total_prev_cost_dis3_lb_USD = sum(prev_cost_dis3_lb, na.rm = TRUE),
                              total_prev_cost_dis5_USD = sum(prev_cost_dis5, na.rm = TRUE),
                              total_prev_cost_dis5_ub_USD = sum(prev_cost_dis5_ub, na.rm = TRUE),
                              total_prev_cost_dis5_lb_USD = sum(prev_cost_dis5_lb, na.rm = TRUE),
                              total_prev_cost_dis0_USD = sum(prev_cost_dis0, na.rm = TRUE),
                              total_prev_cost_dis0_ub_USD = sum(prev_cost_dis0_ub, na.rm = TRUE),
                              total_prev_cost_dis0_lb_USD = sum(prev_cost_dis0_lb, na.rm = TRUE)
                              ) |>
                    pivot_longer(-year, names_to = "Item", values_to = "value") |>
                    pivot_wider(names_from = year, values_from = value)

total_cost4 <- total_prev_cost3 |>
  filter(!str_detect(Item, "_lb_") & !str_detect(Item, "_ub_")) |>
  rowwise() |>
  mutate(
    lb_item = str_replace(Item, "_USD$", "_lb_USD"),
    ub_item = str_replace(Item, "_USD$", "_ub_USD"),
    across(
      .cols = where(is.numeric),  # year columns
      .fns = ~ {
        val <- .x
        colname <- cur_column()
        lb_val <- total_prev_cost3 |> filter(Item == lb_item) |> pull(all_of(colname))
        ub_val <- total_prev_cost3 |> filter(Item == ub_item) |> pull(all_of(colname))
        if (length(lb_val) == 1 && length(ub_val) == 1 && !is.na(val) && !is.na(lb_val) && !is.na(ub_val)) {
          glue("{round(val, 2)} ({round(lb_val, 2)}, {round(ub_val, 2)})") |> as.character()
        } else {
          as.character(round(val, 2))
        }
      }
    )
  ) |>
  ungroup() |>
  select(-lb_item, -ub_item)

colnames(total_cost4) <- colname

########################
## Discounting rate 0 ##
########################
prev_cost3_dis0 <- prev_cost3 |>
                   dplyr::select(state, year, 
                                 PREV.CASES, PREV.CASES_ub, PREV.CASES_lb, 
                                 cpp,
                                 prev_cost_dis0, prev_cost_dis0_ub, prev_cost_dis0_lb)

prev_cost3_dis0_long <- prev_cost3_dis0 |>
                        pivot_longer(cols = c(PREV.CASES, PREV.CASES_ub, PREV.CASES_lb, 
                                               cpp, prev_cost_dis0, prev_cost_dis0_ub, prev_cost_dis0_lb),
                                      names_to = "variable",
                                      values_to = "value")

prev_cost3_dis0_wide <- prev_cost3_dis0_long |>
                        unite("Item", state, variable) |>
                        pivot_wider(names_from = year, values_from = value) 

main_items <- prev_cost3_dis0_wide |>
  filter(!str_ends(Item, "_lb") & !str_ends(Item, "_ub")) |>
  rowwise() |>
  mutate(
    lb_item = paste0(Item, "_lb"),
    ub_item = paste0(Item, "_ub"),
    across(
      .cols = where(is.numeric),  # Apply only to year columns
      .fns = ~ {
        val <- .x
        colname <- cur_column()
        lb_val <- prev_cost3_dis0_wide |> filter(Item == lb_item) |> pull(all_of(colname))
        ub_val <- prev_cost3_dis0_wide |> filter(Item == ub_item) |> pull(all_of(colname))
        
        digits <- case_when(
          str_detect(Item, "PREV\\.CASES") ~ 1,
          str_detect(Item, "_cpp") ~ 0,
          TRUE ~ 2
        )
        
        if (length(lb_val) == 1 && length(ub_val) == 1 && !is.na(val) && !is.na(lb_val) && !is.na(ub_val)) {
          val_fmt <- formatC(val, format = "f", digits = digits)
          lb_fmt  <- formatC(lb_val, format = "f", digits = digits)
          ub_fmt  <- formatC(ub_val, format = "f", digits = digits)
          glue("{val_fmt} ({lb_fmt}, {ub_fmt})") |> as.character()
        } else {
          formatC(val, format = "f", digits = digits)
        }
      }
    )
  ) |>
  ungroup() |>
  select(-lb_item, -ub_item)

spacers <- tibble::tibble(
           Item = c("1L TKI", "2L TKI", "3L TKI", "Progression", "AlloSCT, 1st year", 
                             "AlloSCT, 2nd year+", "TFR"),
            `2015` = NA, `2016` = NA, `2017` = NA, `2018` = NA, `2019` = NA,
            `2020` = NA, `2021` = NA, `2022` = NA, `2023` = NA, `2024` = NA,
            `2025` = NA, `2026` = NA, `2027` = NA, `2028` = NA, `2029` = NA,
            `2030` = NA
          )

# Bind spacer rows to the original data
prev_cost3_dis0_wide_space <- bind_rows(spacers, main_items)

order <- c(
  "1L TKI",
  "cpp_tki_1L_total_PREV.CASES",
  "cpp_tki_1L_total_cpp",
  "cpp_tki_1L_total_prev_cost_dis0",
  
  "2L TKI",
  "cpp_tki_2L_total_PREV.CASES",
  "cpp_tki_2L_total_cpp",
  "cpp_tki_2L_total_prev_cost_dis0",
  
  "3L TKI",
  "cpp_tki_3L_total_PREV.CASES",
  "cpp_tki_3L_total_cpp",
  "cpp_tki_3L_total_prev_cost_dis0",
  
  "Progression",
  "cpp_prog_PREV.CASES",
  "cpp_prog_cpp",
  "cpp_prog_prev_cost_dis0",
  
  "AlloSCT, 1st year",
  "cpp_alloSCT_1y_PREV.CASES",
  "cpp_alloSCT_1y_cpp",
  "cpp_alloSCT_1y_prev_cost_dis0",
  
  "AlloSCT, 2nd year+",
  "cpp_alloSCT_2y_PREV.CASES",
  "cpp_alloSCT_2y_cpp",
  "cpp_alloSCT_2y_prev_cost_dis0",
  
  "TFR",
  "cpp_tfr_PREV.CASES",
  "cpp_tfr_cpp",
  "cpp_tfr_prev_cost_dis0"
)

renamed_items <- c(
  "1L TKI" = "1L TKI",
  "cpp_tki_1L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_1L_total_cpp" = "Yearly costs per patient (USD)",
  "cpp_tki_1L_total_prev_cost_dis0" = "Prevalence costs (USD millions)",
  
  "2L TKI" = "2L TKI",
  "cpp_tki_2L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_2L_total_cpp" = "Yearly costs per patient (USD)",
  "cpp_tki_2L_total_prev_cost_dis0" = "Prevalence costs (USD millions)",
  
  "3L TKI" = "3L TKI",
  "cpp_tki_3L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_3L_total_cpp" = "Yearly costs per patient (USD)",
  "cpp_tki_3L_total_prev_cost_dis0" = "Prevalence costs (USD millions)",
  
  "Progression" = "Progression",
  "cpp_prog_PREV.CASES" = "Prevalent cases",
  "cpp_prog_cpp" = "Yearly costs per patient (USD)",
  "cpp_prog_prev_cost_dis0" = "Prevalence costs (USD millions)",
  
  "AlloSCT, 1st year" = "AlloSCT, 1st year",
  "cpp_alloSCT_1y_PREV.CASES" = "Prevalent cases",
  "cpp_alloSCT_1y_cpp" = "Yearly costs per patient (USD)",
  "cpp_alloSCT_1y_prev_cost_dis0" = "Prevalence costs (USD millions)",
  
  "AlloSCT, 2nd year+" = "AlloSCT, 2nd year+",
  "cpp_alloSCT_2y_PREV.CASES" = "Prevalent cases",
  "cpp_alloSCT_2y_cpp" = "Yearly costs per patient (USD)",
  "cpp_alloSCT_2y_prev_cost_dis0" = "Prevalence costs (USD millions)",
  
  "TFR" = "TFR",
  "cpp_tfr_PREV.CASES" = "Prevalent cases",
  "cpp_tfr_cpp" = "Yearly costs per patient (USD)",
  "cpp_tfr_prev_cost_dis0" = "Prevalence costs (USD millions)"
)

prev_cost3_dis0_wide_space <- prev_cost3_dis0_wide_space |>
                               mutate(Item = factor(Item, levels = order)) |>
                               arrange(Item) |>
                               mutate(Item = renamed_items[as.character(Item)]) 
colnames(prev_cost3_dis0_wide_space) <- colname

###########################################################
# Keep only rows where the 'Item' column ends with either "_cpp" or "_PREV.CASES"
prev_cost3_dis0_cpp <- prev_cost3_dis0_wide |>
                       filter(str_detect(Item, "_cpp$|_PREV\\.CASES$"))
# Define matching pairs
pairs <- list(
  c("cpp_prog_PREV.CASES", "cpp_prog_cpp"),
  c("cpp_alloSCT_1y_PREV.CASES", "cpp_alloSCT_1y_cpp"),
  c("cpp_alloSCT_2y_PREV.CASES", "cpp_alloSCT_2y_cpp"),
  c("cpp_tfr_PREV.CASES", "cpp_tfr_cpp"),
  c("cpp_tki_1L_total_PREV.CASES", "cpp_tki_1L_total_cpp"),
  c("cpp_tki_2L_total_PREV.CASES", "cpp_tki_2L_total_cpp"),
  c("cpp_tki_3L_total_PREV.CASES", "cpp_tki_3L_total_cpp")
)

# Calculate weighted cpp for each year
weighted_cpp_dis0 <- sapply(names(prev_cost3_dis0_cpp)[-1], function(year) {
  total_prev <- 0
  weighted_sum <- 0
  for (pair in pairs) {
    prev_val <- prev_cost3_dis0_cpp %>% filter(Item == pair[1]) %>% pull(!!sym(year))
    cpp_val  <- prev_cost3_dis0_cpp %>% filter(Item == pair[2]) %>% pull(!!sym(year))
    total_prev <- total_prev + prev_val
    weighted_sum <- weighted_sum + prev_val * cpp_val
  }
  weighted_sum / total_prev
})

# round to integers and convert to character
weighted_cpp_chr_dis0 <- setNames(as.list(sprintf("%.0f", weighted_cpp_dis0)), names(weighted_cpp_dis0))

# one-row tibble with Item first, called AYCPP, all values are character
weighted_cpp_row_dis0 <- tibble::tibble_row(!!!c(list(Item = "AYCPP"), weighted_cpp_chr_dis0))

colnames(weighted_cpp_row_dis0) <- colname

################################################################################
prev_cost3_dis0_wide_space1 <- bind_rows(prev_cost3_dis0_wide_space,
                                         piamod_TOTAL,
                                         weighted_cpp_row_dis0,
                                         subset(total_cost4, Item == "total_prev_cost_dis0_USD")
                                         )|>
                                mutate(Item = recode(Item,
                                                     "total_prev_cost_dis0_USD" = "Total prevalence costs (USD millions)",
                                                     "AYCPP" = "Average yearly cost per patient (USD)",
                                                     "PREV.CASES" = "Total prevalent cases"))
df <- prev_cost3_dis0_wide_space1
names(df) <- str_remove_all(names(df), "`")   # drop the back-ticks

df2 <- df %>% 
  mutate(
    Treatment = if_else(
      Item %in% c(
        "1L TKI", "2L TKI", "3L TKI",
        "Progression",
        "AlloSCT, 1st year", "AlloSCT, 2nd year+",
        "TFR"
      ),
      Item,
      NA_character_
    )
  ) %>% 
  tidyr::fill(Treatment)          
  
long <- df2 %>% 
        pivot_longer(-c(Treatment, Item), names_to = "Year", values_to = "Value")

wide <- long %>% 
        unite("ColName", Treatment, Item, sep = " ", remove = FALSE) %>%
        select(-Treatment, -Item) %>% 
        pivot_wider(names_from = ColName, values_from = Value) %>% 
        arrange(Year) %>% 
        column_to_rownames("Year")

# Delete column 1 5 9 13 17 21 25
wide1 <- wide[, -c(1, 5, 9, 13, 17, 21, 25)]
# Add the first colum called Year and give 2015 to 2030
wide2 <- cbind(Year = as.character(seq(2015, 2030)), wide1) |>
               mutate(across(matches("\\(Yearly costs per patient (USD)?\\)$"),
                      ~ parse_number(.x, locale = locale(decimal_mark = ".", grouping_mark = " "))))
      
ft0 <- flextable(wide2) |> 
      autofit() |>
      set_header_labels(ft, values = c("Year", rep(c("Prevalent cases",
                                                      "YCPP (USD)",
                                                      "Prevalence costs (USD m)"), 7),
                                                "Total prevalent cases",
                                                "Average YCPP (USD)",
                                                "Total prevalence costs (USD m)")) |>
      add_header_row(values = c("", "1L TKI", "", "", 
                                    "2L TKI", "", "", 
                                    "3L TKI", "", "",
                                    "Progression", "", "",
                                    "AlloSCT, 1st year", "", "", 
                                    "AlloSCT, 2nd year+", "", "", 
                                    "TFR", "", "",
                                    "Total", "", ""))  |>
  align(align = "right", j = 1:24, part = "body") 
  # add_footer_lines("YCPP, yearly cost per patient; USD m, USD millions; 1L, first-line; 2L, second-line; 3L+, third-line and later; TKI, tyrosine kinase inhibitor; AlloSCT, allogeneic stem cell transplantation.")
  
ft0
#save_as_docx(ft0, path="../Output/Cost_05b_prev_costs_tab_dis0.docx")

########################
## Discounting rate 3 ##
########################
prev_cost3_dis3 <- prev_cost3 |>
                    dplyr::select(state, year, 
                                  PREV.CASES, PREV.CASES_ub, PREV.CASES_lb, 
                                  cpp_dis3,
                                  prev_cost_dis3, prev_cost_dis3_ub, prev_cost_dis3_lb)

prev_cost3_dis3_long <- prev_cost3_dis3 |>
  pivot_longer(cols = c(PREV.CASES, PREV.CASES_ub, PREV.CASES_lb, 
                        cpp_dis3, prev_cost_dis3, prev_cost_dis3_ub, prev_cost_dis3_lb),
               names_to = "variable",
               values_to = "value")

prev_cost3_dis3_wide <- prev_cost3_dis3_long |>
  unite("Item", state, variable) |>
  pivot_wider(names_from = year, values_from = value) 

main_items <- prev_cost3_dis3_wide |>
  filter(!str_ends(Item, "_lb") & !str_ends(Item, "_ub")) |>
  rowwise() |>
  mutate(
    lb_item = paste0(Item, "_lb"),
    ub_item = paste0(Item, "_ub"),
    across(
      .cols = where(is.numeric),  # Apply only to year columns
      .fns = ~ {
        val <- .x
        colname <- cur_column()
        lb_val <- prev_cost3_dis3_wide |> filter(Item == lb_item) |> pull(all_of(colname))
        ub_val <- prev_cost3_dis3_wide |> filter(Item == ub_item) |> pull(all_of(colname))
        
        digits <- case_when(
          str_detect(Item, "PREV\\.CASES") ~ 1,
          str_detect(Item, "_cpp") ~ 0,
          TRUE ~ 2
        )
        
        if (length(lb_val) == 1 && length(ub_val) == 1 && !is.na(val) && !is.na(lb_val) && !is.na(ub_val)) {
          val_fmt <- formatC(val, format = "f", digits = digits)
          lb_fmt  <- formatC(lb_val, format = "f", digits = digits)
          ub_fmt  <- formatC(ub_val, format = "f", digits = digits)
          glue("{val_fmt} ({lb_fmt}, {ub_fmt})") |> as.character()
        } else {
          formatC(val, format = "f", digits = digits)
        }
      }
    )
  ) |>
  ungroup() |>
  select(-lb_item, -ub_item)

spacers <- tibble::tibble(
  Item = c("1L TKI", "2L TKI", "3L TKI", "Progression", "AlloSCT, 1st year", 
           "AlloSCT, 2nd year+", "TFR"),
  `2015` = NA, `2016` = NA, `2017` = NA, `2018` = NA, `2019` = NA,
  `2020` = NA, `2021` = NA, `2022` = NA, `2023` = NA, `2024` = NA,
  `2025` = NA, `2026` = NA, `2027` = NA, `2028` = NA, `2029` = NA,
  `2030` = NA
)

# Bind spacer rows to the original data
prev_cost3_dis3_wide_space <- bind_rows(spacers, main_items)

order <- c(
  "1L TKI",
  "cpp_tki_1L_total_PREV.CASES",
  "cpp_tki_1L_total_cpp_dis3",
  "cpp_tki_1L_total_prev_cost_dis3",
  
  "2L TKI",
  "cpp_tki_2L_total_PREV.CASES",
  "cpp_tki_2L_total_cpp_dis3",
  "cpp_tki_2L_total_prev_cost_dis3",
  
  "3L TKI",
  "cpp_tki_3L_total_PREV.CASES",
  "cpp_tki_3L_total_cpp_dis3",
  "cpp_tki_3L_total_prev_cost_dis3",
  
  "Progression",
  "cpp_prog_PREV.CASES",
  "cpp_prog_cpp_dis3",
  "cpp_prog_prev_cost_dis3",
  
  "AlloSCT, 1st year",
  "cpp_alloSCT_1y_PREV.CASES",
  "cpp_alloSCT_1y_cpp_dis3",
  "cpp_alloSCT_1y_prev_cost_dis3",
  
  "AlloSCT, 2nd year+",
  "cpp_alloSCT_2y_PREV.CASES",
  "cpp_alloSCT_2y_cpp_dis3",
  "cpp_alloSCT_2y_prev_cost_dis3",
  
  "TFR",
  "cpp_tfr_PREV.CASES",
  "cpp_tfr_cpp_dis3",
  "cpp_tfr_prev_cost_dis3"
)

renamed_items <- c(
  "1L TKI" = "1L TKI",
  "cpp_tki_1L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_1L_total_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_tki_1L_total_prev_cost_dis3" = "Prevalence costs (USD millions)",
  
  "2L TKI" = "2L TKI",
  "cpp_tki_2L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_2L_total_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_tki_2L_total_prev_cost_dis3" = "Prevalence costs (USD millions)",
  
  "3L TKI" = "3L TKI",
  "cpp_tki_3L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_3L_total_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_tki_3L_total_prev_cost_dis3" = "Prevalence costs (USD millions)",
  
  "Progression" = "Progression",
  "cpp_prog_PREV.CASES" = "Prevalent cases",
  "cpp_prog_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_prog_prev_cost_dis3" = "Prevalence costs (USD millions)",
  
  "AlloSCT, 1st year" = "AlloSCT, 1st year",
  "cpp_alloSCT_1y_PREV.CASES" = "Prevalent cases",
  "cpp_alloSCT_1y_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_alloSCT_1y_prev_cost_dis3" = "Prevalence costs (USD millions)",
  
  "AlloSCT, 2nd year+" = "AlloSCT, 2nd year+",
  "cpp_alloSCT_2y_PREV.CASES" = "Prevalent cases",
  "cpp_alloSCT_2y_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_alloSCT_2y_prev_cost_dis3" = "Prevalence costs (USD millions)",
  
  "TFR" = "TFR",
  "cpp_tfr_PREV.CASES" = "Prevalent cases",
  "cpp_tfr_cpp_dis3" = "Yearly costs per patient (USD)",
  "cpp_tfr_prev_cost_dis3" = "Prevalence costs (USD millions)"
)

prev_cost3_dis3_wide_space <- prev_cost3_dis3_wide_space |>
  mutate(Item = factor(Item, levels = order)) |>
  arrange(Item) |>
  mutate(Item = renamed_items[as.character(Item)]) 
colnames(prev_cost3_dis3_wide_space) <- colname

###########################################################
# Keep only rows where the 'Item' column ends with either "_cpp" or "_PREV.CASES"
prev_cost3_dis3_cpp <- prev_cost3_dis3_wide |>
  filter(str_detect(Item, "_cpp_dis3$|_PREV\\.CASES$"))

# Define matching pairs
pairs <- list(
  c("cpp_prog_PREV.CASES", "cpp_prog_cpp_dis3"),
  c("cpp_alloSCT_1y_PREV.CASES", "cpp_alloSCT_1y_cpp_dis3"),
  c("cpp_alloSCT_2y_PREV.CASES", "cpp_alloSCT_2y_cpp_dis3"),
  c("cpp_tfr_PREV.CASES", "cpp_tfr_cpp_dis3"),
  c("cpp_tki_1L_total_PREV.CASES", "cpp_tki_1L_total_cpp_dis3"),
  c("cpp_tki_2L_total_PREV.CASES", "cpp_tki_2L_total_cpp_dis3"),
  c("cpp_tki_3L_total_PREV.CASES", "cpp_tki_3L_total_cpp_dis3")
)

# Calculate weighted cpp for each year
weighted_cpp_dis3 <- sapply(names(prev_cost3_dis3_cpp)[-1], function(year) {
  total_prev <- 0
  weighted_sum <- 0
  for (pair in pairs) {
    prev_val <- prev_cost3_dis3_cpp %>% filter(Item == pair[1]) %>% pull(!!sym(year))
    cpp_val  <- prev_cost3_dis3_cpp %>% filter(Item == pair[2]) %>% pull(!!sym(year))
    total_prev <- total_prev + prev_val
    weighted_sum <- weighted_sum + prev_val * cpp_val
  }
  weighted_sum / total_prev
})

# round to integers and convert to character
weighted_cpp_chr_dis3 <- setNames(as.list(sprintf("%.0f", weighted_cpp_dis3)), names(weighted_cpp_dis3))

# one-row tibble with Item first, called AYCPP, all values are character
weighted_cpp_row_dis3 <- tibble::tibble_row(!!!c(list(Item = "AYCPP"), weighted_cpp_chr_dis3))

colnames(weighted_cpp_row_dis3) <- colname
################################################################################

prev_cost3_dis3_wide_space1 <- bind_rows(prev_cost3_dis3_wide_space,
                                         piamod_TOTAL,
                                         weighted_cpp_row_dis3,
                                         subset(total_cost4, Item == "total_prev_cost_dis3_USD")
)|>
  mutate(Item = recode(Item,
                       "total_prev_cost_dis3_USD" = "Total prevalence costs (USD millions)",
                       "AYCPP" = "Average yearly cost per patient (USD)",
                       "PREV.CASES" = "Total prevalent cases"))
df <- prev_cost3_dis3_wide_space1
names(df) <- str_remove_all(names(df), "`")   # drop the back-ticks

df2 <- df %>% 
  mutate(
    Treatment = if_else(
      Item %in% c(
        "1L TKI", "2L TKI", "3L TKI",
        "Progression",
        "AlloSCT, 1st year", "AlloSCT, 2nd year+",
        "TFR"
      ),
      Item,
      NA_character_
    )
  ) %>% 
  tidyr::fill(Treatment)          

long <- df2 %>% 
  pivot_longer(-c(Treatment, Item), names_to = "Year", values_to = "Value")

wide <- long %>% 
  unite("ColName", Treatment, Item, sep = " ", remove = FALSE) %>%
  select(-Treatment, -Item) %>% 
  pivot_wider(names_from = ColName, values_from = Value) %>% 
  arrange(Year) %>% 
  column_to_rownames("Year")

# Delete column 1 5 9 13 17 21 25
wide1 <- wide[, -c(1, 5, 9, 13, 17, 21, 25)]
# Add the first colum called Year and give 2015 to 2030
wide2 <- cbind(Year = as.character(seq(2015, 2030)), wide1) |>
  mutate(across(matches("\\(Yearly costs per patient (USD)?\\)$"),
                ~ parse_number(.x, locale = locale(decimal_mark = ".", grouping_mark = " "))))

ft3 <- flextable(wide2) |> 
  autofit() |>
  set_header_labels(ft, values = c("Year", rep(c("Prevalent cases",
                                                 "YCPP (USD)",
                                                 "Prevalence costs (USD m)"), 7),
                                   "Total prevalent cases",
                                   "Average YCPP (USD)",
                                   "Total prevalence costs (USD m)")) |>
  add_header_row(values = c("", "1L TKI", "", "", 
                            "2L TKI", "", "", 
                            "3L TKI", "", "",
                            "Progression", "", "",
                            "AlloSCT, 1st year", "", "", 
                            "AlloSCT, 2nd year+", "", "", 
                            "TFR", "", "",
                            "Total", "", ""))  |>
  align(align = "right", j = 1:24, part = "body")
  # add_footer_lines("YCPP, yearly cost per patient; USD m, USD millions; 1L, first-line; 2L, second-line; 3L+, third-line and later; TKI, tyrosine kinase inhibitor; AlloSCT, allogeneic stem cell transplantation.")
  
ft3
#save_as_docx(ft3, path="../Output/Cost_05b_prev_costs_tab_dis3.docx")

######################
# Discounting rate 5 #
######################
prev_cost3_dis5 <- prev_cost3 |>
  dplyr::select(state, year, 
                PREV.CASES, PREV.CASES_ub, PREV.CASES_lb, 
                cpp_dis5,
                prev_cost_dis5, prev_cost_dis5_ub, prev_cost_dis5_lb)

prev_cost3_dis5_long <- prev_cost3_dis5 |>
  pivot_longer(cols = c(PREV.CASES, PREV.CASES_ub, PREV.CASES_lb, 
                        cpp_dis5, prev_cost_dis5, prev_cost_dis5_ub, prev_cost_dis5_lb),
               names_to = "variable",
               values_to = "value")

prev_cost3_dis5_wide <- prev_cost3_dis5_long |>
  unite("Item", state, variable) |>
  pivot_wider(names_from = year, values_from = value) 

main_items <- prev_cost3_dis5_wide |>
  filter(!str_ends(Item, "_lb") & !str_ends(Item, "_ub")) |>
  rowwise() |>
  mutate(
    lb_item = paste0(Item, "_lb"),
    ub_item = paste0(Item, "_ub"),
    across(
      .cols = where(is.numeric),  # Apply only to year columns
      .fns = ~ {
        val <- .x
        colname <- cur_column()
        lb_val <- prev_cost3_dis5_wide |> filter(Item == lb_item) |> pull(all_of(colname))
        ub_val <- prev_cost3_dis5_wide |> filter(Item == ub_item) |> pull(all_of(colname))
        
        digits <- case_when(
          str_detect(Item, "PREV\\.CASES") ~ 1,
          str_detect(Item, "_cpp") ~ 0,
          TRUE ~ 2
        )
        
        if (length(lb_val) == 1 && length(ub_val) == 1 && !is.na(val) && !is.na(lb_val) && !is.na(ub_val)) {
          val_fmt <- formatC(val, format = "f", digits = digits)
          lb_fmt  <- formatC(lb_val, format = "f", digits = digits)
          ub_fmt  <- formatC(ub_val, format = "f", digits = digits)
          glue("{val_fmt} ({lb_fmt}, {ub_fmt})") |> as.character()
        } else {
          formatC(val, format = "f", digits = digits)
        }
      }
    )
  ) |>
  ungroup() |>
  select(-lb_item, -ub_item)

spacers <- tibble::tibble(
  Item = c("1L TKI", "2L TKI", "3L TKI", "Progression", "AlloSCT, 1st year", 
           "AlloSCT, 2nd year+", "TFR"),
  `2015` = NA, `2016` = NA, `2017` = NA, `2018` = NA, `2019` = NA,
  `2020` = NA, `2021` = NA, `2022` = NA, `2023` = NA, `2024` = NA,
  `2025` = NA, `2026` = NA, `2027` = NA, `2028` = NA, `2029` = NA,
  `2030` = NA
)

# Bind spacer rows to the original data
prev_cost3_dis5_wide_space <- bind_rows(spacers, main_items)

order <- c(
  "1L TKI",
  "cpp_tki_1L_total_PREV.CASES",
  "cpp_tki_1L_total_cpp_dis5",
  "cpp_tki_1L_total_prev_cost_dis5",
  
  "2L TKI",
  "cpp_tki_2L_total_PREV.CASES",
  "cpp_tki_2L_total_cpp_dis5",
  "cpp_tki_2L_total_prev_cost_dis5",
  
  "3L TKI",
  "cpp_tki_3L_total_PREV.CASES",
  "cpp_tki_3L_total_cpp_dis5",
  "cpp_tki_3L_total_prev_cost_dis5",
  
  "Progression",
  "cpp_prog_PREV.CASES",
  "cpp_prog_cpp_dis5",
  "cpp_prog_prev_cost_dis5",
  
  "AlloSCT, 1st year",
  "cpp_alloSCT_1y_PREV.CASES",
  "cpp_alloSCT_1y_cpp_dis5",
  "cpp_alloSCT_1y_prev_cost_dis5",
  
  "AlloSCT, 2nd year+",
  "cpp_alloSCT_2y_PREV.CASES",
  "cpp_alloSCT_2y_cpp_dis5",
  "cpp_alloSCT_2y_prev_cost_dis5",
  
  "TFR",
  "cpp_tfr_PREV.CASES",
  "cpp_tfr_cpp_dis5",
  "cpp_tfr_prev_cost_dis5"
)

renamed_items <- c(
  "1L TKI" = "1L TKI",
  "cpp_tki_1L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_1L_total_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_tki_1L_total_prev_cost_dis5" = "Prevalence costs (USD millions)",
  
  "2L TKI" = "2L TKI",
  "cpp_tki_2L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_2L_total_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_tki_2L_total_prev_cost_dis5" = "Prevalence costs (USD millions)",
  
  "3L TKI" = "3L TKI",
  "cpp_tki_3L_total_PREV.CASES" = "Prevalent cases",
  "cpp_tki_3L_total_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_tki_3L_total_prev_cost_dis5" = "Prevalence costs (USD millions)",
  
  "Progression" = "Progression",
  "cpp_prog_PREV.CASES" = "Prevalent cases",
  "cpp_prog_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_prog_prev_cost_dis5" = "Prevalence costs (USD millions)",
  
  "AlloSCT, 1st year" = "AlloSCT, 1st year",
  "cpp_alloSCT_1y_PREV.CASES" = "Prevalent cases",
  "cpp_alloSCT_1y_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_alloSCT_1y_prev_cost_dis5" = "Prevalence costs (USD millions)",
  
  "AlloSCT, 2nd year+" = "AlloSCT, 2nd year+",
  "cpp_alloSCT_2y_PREV.CASES" = "Prevalent cases",
  "cpp_alloSCT_2y_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_alloSCT_2y_prev_cost_dis5" = "Prevalence costs (USD millions)",
  
  "TFR" = "TFR",
  "cpp_tfr_PREV.CASES" = "Prevalent cases",
  "cpp_tfr_cpp_dis5" = "Yearly costs per patient (USD)",
  "cpp_tfr_prev_cost_dis5" = "Prevalence costs (USD millions)"
)

prev_cost3_dis5_wide_space <- prev_cost3_dis5_wide_space |>
  mutate(Item = factor(Item, levels = order)) |>
  arrange(Item) |>
  mutate(Item = renamed_items[as.character(Item)]) 
colnames(prev_cost3_dis5_wide_space) <- colname

###########################################################
# Keep only rows where the 'Item' column ends with either "_cpp" or "_PREV.CASES"
prev_cost3_dis5_cpp <- prev_cost3_dis5_wide |>
  filter(str_detect(Item, "_cpp_dis5$|_PREV\\.CASES$"))
# Define matching pairs
pairs <- list(
  c("cpp_prog_PREV.CASES", "cpp_prog_cpp_dis5"),
  c("cpp_alloSCT_1y_PREV.CASES", "cpp_alloSCT_1y_cpp_dis5"),
  c("cpp_alloSCT_2y_PREV.CASES", "cpp_alloSCT_2y_cpp_dis5"),
  c("cpp_tfr_PREV.CASES", "cpp_tfr_cpp_dis5"),
  c("cpp_tki_1L_total_PREV.CASES", "cpp_tki_1L_total_cpp_dis5"),
  c("cpp_tki_2L_total_PREV.CASES", "cpp_tki_2L_total_cpp_dis5"),
  c("cpp_tki_3L_total_PREV.CASES", "cpp_tki_3L_total_cpp_dis5")
)

# Calculate weighted cpp for each year
weighted_cpp_dis5 <- sapply(names(prev_cost3_dis5_cpp)[-1], function(year) {
  total_prev <- 0
  weighted_sum <- 0
  for (pair in pairs) {
    prev_val <- prev_cost3_dis5_cpp %>% filter(Item == pair[1]) %>% pull(!!sym(year))
    cpp_val  <- prev_cost3_dis5_cpp %>% filter(Item == pair[2]) %>% pull(!!sym(year))
    total_prev <- total_prev + prev_val
    weighted_sum <- weighted_sum + prev_val * cpp_val
  }
  weighted_sum / total_prev
})

# round to integers and convert to character
weighted_cpp_chr_dis5 <- setNames(as.list(sprintf("%.0f", weighted_cpp_dis5)), names(weighted_cpp_dis5))

# one-row tibble with Item first, called AYCPP, all values are character
weighted_cpp_row_dis5 <- tibble::tibble_row(!!!c(list(Item = "AYCPP"), weighted_cpp_chr_dis5))

colnames(weighted_cpp_row_dis5) <- colname
################################################################################

prev_cost3_dis5_wide_space1 <- bind_rows(prev_cost3_dis5_wide_space,
                                         piamod_TOTAL,
                                         weighted_cpp_row_dis5,
                                         subset(total_cost4, Item == "total_prev_cost_dis5_USD"))|>
  mutate(Item = recode(Item,
                       "total_prev_cost_dis5_USD" = "Total prevalence costs (USD millions)",
                       "AYCPP" = "Average yearly cost per patient (USD)",
                       "PREV.CASES" = "Total prevalent cases"))
df <- prev_cost3_dis5_wide_space1
names(df) <- str_remove_all(names(df), "`")   # drop the back-ticks

df2 <- df %>% 
  mutate(
    Treatment = if_else(
      Item %in% c(
        "1L TKI", "2L TKI", "3L TKI",
        "Progression",
        "AlloSCT, 1st year", "AlloSCT, 2nd year+",
        "TFR"
      ),
      Item,
      NA_character_
    )
  ) %>% 
  tidyr::fill(Treatment)          

long <- df2 %>% 
  pivot_longer(-c(Treatment, Item), names_to = "Year", values_to = "Value")

wide <- long %>% 
  unite("ColName", Treatment, Item, sep = " ", remove = FALSE) %>%
  select(-Treatment, -Item) %>% 
  pivot_wider(names_from = ColName, values_from = Value) %>% 
  arrange(Year) %>% 
  column_to_rownames("Year")

# Delete column 1 5 9 13 17 21 25
wide1 <- wide[, -c(1, 5, 9, 13, 17, 21, 25)]
# Add the first colum called Year and give 2015 to 2030
wide2 <- cbind(Year = as.character(seq(2015, 2030)), wide1) |>
  mutate(across(matches("\\(Yearly costs per patient (USD)?\\)$"),
                ~ parse_number(.x, locale = locale(decimal_mark = ".", grouping_mark = " "))))

ft5 <- flextable(wide2) |> 
  autofit() |>
  set_header_labels(ft, values = c("Year", rep(c("Prevalent cases",
                                                 "YCPP (USD)",
                                                 "Prevalence costs (USD m)"), 7),
                                   "Total prevalent cases",
                                   "Average YCPP (USD)",
                                   "Total prevalence costs (USD m)")) |>
  add_header_row(values = c("", "1L TKI", "", "", 
                            "2L TKI", "", "", 
                            "3L TKI", "", "",
                            "Progression", "", "",
                            "AlloSCT, 1st year", "", "", 
                            "AlloSCT, 2nd year+", "", "", 
                            "TFR", "", "",
                            "Total", "", ""))  |>
  align(align = "right", j = 1:24, part = "body")
  # add_footer_lines("YCPP, yearly cost per patient; USD m, USD millions; 1L, first-line; 2L, second-line; 3L+, third-line and later; TKI, tyrosine kinase inhibitor; AlloSCT, allogeneic stem cell transplantation.")

ft5
#save_as_docx(ft5, path="../Output/Cost_05b_prev_costs_tab_dis5.docx")
