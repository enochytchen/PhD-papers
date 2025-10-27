## Filename: Cost_04a2_prev_dist_tab.R 
## Updated: 

###################
## Read packages ##
###################
library(dplyr)
library(flextable)
library(tidyr)
library(officer)

###############
## Read data ##
###############
# Read the prevalent cases by state
prev_dist <- read.csv("../Data/Cost_04a_prev_dist.csv", header = TRUE, stringsAsFactors = FALSE) |>
             dplyr::select(state, YEAR, PREV.CASES) |> 
             pivot_wider(names_from = state, values_from = PREV.CASES)

# Read PIAMOD 
piamod <- read.csv(file="../Data/PIAMOD_03_piamod_constantRS_dynINC.csv", header = TRUE)
piamod_ALL <- piamod[piamod$SEX == 3 & piamod$AGE_GROUP == "0-99" & piamod$YEAR >= 2000,] |>
              dplyr::select(c("YEAR", "PREV.CASES")) |>
              filter(YEAR >= 2015)

# Merge with piamod_ALL
df_merged <- piamod_ALL |> 
             left_join(prev_dist, by = "YEAR") 

df_merged <- df_merged |> 
             mutate(TOTAL = rowSums(pick(Prog, Sct_1y, Sct_2y, Tfr, Tki1, Tki2, Tki3), na.rm = TRUE),
                    DIFF = TOTAL - PREV.CASES)

print(df_merged$DIFF) # This looks good, the difference is 0


# Read incidence cases
inc_cases <- read.csv("../Data/Cost_04a_prev_dist_inc.csv", header = TRUE, stringsAsFactors = FALSE)
inc_data <- inc_cases |>
            pivot_longer(cols = c(INC.CASES.CP, INC.CASES.Prog),
                         names_to = "Phase",
                         values_to = "INC.CASES") |>
            mutate(
              Phase = case_when(
                Phase == "INC.CASES.CP" ~ "CP",
                Phase == "INC.CASES.Prog" ~ "AP/BP"
              ))|>
            rename(DiagYear = YEAR) 



# Read inc + prev
inc_prev_CP <- read.csv("../Data/Cost_04a_prev_dist_CPinc_year.csv", header = TRUE, stringsAsFactors = FALSE) |>
               filter(variable %in% c("Tki1", "Tki2", "Tki3", "Tfr", "Sct", "Prog")) |>
               group_by(YEAR, variable) |> 
               summarise(across(starts_with("X"), ~sum(., na.rm = TRUE)), .groups = "drop") |>
               mutate(Phase = "CP")

inc_prev_Prog <- read.csv("../Data/Cost_04a_prev_dist_Proginc_year.csv", header = TRUE, stringsAsFactors = FALSE) |>
                 filter(variable %in% c("Tki1", "Tki2", "Tki3", "Tfr", "Sct", "Prog")) |>
                 group_by(YEAR, variable) |> 
                 summarise(across(starts_with("X"), ~sum(., na.rm = TRUE)), .groups = "drop") |>
                 mutate(Phase = "AP/BP")

inc_prev <- bind_rows(inc_prev_CP, inc_prev_Prog) |>
            rename(DiagYear = YEAR)


# Prepare prevalence data
prev_data <- inc_prev |>
             pivot_longer(cols = starts_with("X"), names_to = "YEAR", values_to = "Value") |>
             mutate(YEAR = as.numeric(gsub("X", "", YEAR))) |>
             filter(YEAR %in% c(2015, 2020, 2025, 2030)) |>
             pivot_wider(names_from = variable, values_from = Value)

# Merge prevalence and incidence
final_data <- inc_data |>
              left_join(prev_data |> filter(YEAR == 2015) |> dplyr::select(-YEAR), by = c("DiagYear", "Phase")) |>
              left_join(prev_data |> filter(YEAR == 2020) |> dplyr::select(-YEAR), by = c("DiagYear", "Phase"), suffix = c("", "_2020")) |>
              left_join(prev_data |> filter(YEAR == 2025) |> dplyr::select(-YEAR), by = c("DiagYear", "Phase"), suffix = c("", "_2025")) |>
              left_join(prev_data |> filter(YEAR == 2030) |> dplyr::select(-YEAR), by = c("DiagYear", "Phase"), suffix = c("", "_2030")) |>
              mutate(
                DiagYear = as.character(DiagYear),
                across(where(is.numeric), ~ ifelse(. == 0, round(0, 0), round(., 1))))
    

final_data_selected <- final_data |>
                       dplyr::select(DiagYear, Phase, INC.CASES,
                             # 2015
                             `1L TKI_2015` = Tki1, `2L TKI_2015` = Tki2, `3L+ TKI_2015` = Tki3, Progression_2015 = Prog, AlloSCT_2015 = Sct, TFR_2015 = Tfr,
                             # 2020
                             `1L TKI_2020` = Tki1_2020, `2L TKI_2020` = Tki2_2020, `3L+ TKI_2020` = Tki3_2020, Progression_2020 = Prog_2020, AlloSCT_2020 = Sct_2020, TFR_2020 = Tfr_2020,
                             # 2025
                             `1L TKI_2025` = Tki1_2025, `2L TKI_2025` = Tki2_2025, `3L+ TKI_2025` = Tki3_2025, Progression_2025 = Prog_2025, AlloSCT_2025 = Sct_2025, TFR_2025 = Tfr_2025,
                             # 2030
                             `1L TKI_2030` = Tki1_2030, `2L TKI_2030` = Tki2_2030, `3L+ TKI_2030` = Tki3_2030, Progression_2030 = Prog_2030, AlloSCT_2030 = Sct_2030, TFR_2030 = Tfr_2030) |>
                        group_by(DiagYear) |>
                        arrange(DiagYear, desc(Phase)) |>
                        ungroup() |>
                        mutate(DiagYear = ifelse(Phase == "AP/BP", "", DiagYear))

# Add a row called with DiagYear = "Total" rowSum `1L TKI_2015` and other cols
final_data_selected2 <- final_data_selected |>
                        bind_rows(
                          final_data_selected |>
                            filter(DiagYear != "") |>
                            summarise(across(-c(DiagYear, Phase), sum, na.rm = TRUE)) |>
                            mutate(DiagYear = "Total", Phase = "")
                        )

# Tackle the issue of proportion
df_merged_row <- df_merged |>
  filter(YEAR %in% c(2015, 2020, 2025, 2030)) |>
  summarise(
    DiagYear = "Prevalent cases by state",
    Phase = "",
    INC.CASES = NA,
    
    `1L TKI_2015` = Tki1[YEAR == 2015], `2L TKI_2015` = Tki2[YEAR == 2015], `3L+ TKI_2015` = Tki3[YEAR == 2015],
    Progression_2015 = Prog[YEAR == 2015], AlloSCT_2015 = Sct_1y[YEAR == 2015] + Sct_2y[YEAR == 2015], TFR_2015 = Tfr[YEAR == 2015],
    
    `1L TKI_2020` = Tki1[YEAR == 2020], `2L TKI_2020` = Tki2[YEAR == 2020], `3L+ TKI_2020` = Tki3[YEAR == 2020],
    Progression_2020 = Prog[YEAR == 2020], AlloSCT_2020 = Sct_1y[YEAR == 2020] + Sct_2y[YEAR == 2020], TFR_2020 = Tfr[YEAR == 2020],
    
    `1L TKI_2025` = Tki1[YEAR == 2025], `2L TKI_2025` = Tki2[YEAR == 2025], `3L+ TKI_2025` = Tki3[YEAR == 2025],
    Progression_2025 = Prog[YEAR == 2025], AlloSCT_2025 = Sct_1y[YEAR == 2025] + Sct_2y[YEAR == 2025], TFR_2025 = Tfr[YEAR == 2025],
    
    `1L TKI_2030` = Tki1[YEAR == 2030], `2L TKI_2030` = Tki2[YEAR == 2030], `3L+ TKI_2030` = Tki3[YEAR == 2030],
    Progression_2030 = Prog[YEAR == 2030], AlloSCT_2030 = Sct_1y[YEAR == 2030] + Sct_2y[YEAR == 2030], TFR_2030 = Tfr[YEAR == 2030]
  )

df_merged_row2 <- df_merged |>
  filter(YEAR %in% c(2015, 2020, 2025, 2030)) |>
  summarise(
    DiagYear = "Total prevalent cases",
    Phase = "",
    INC.CASES = NA,
    TFR_2015 = floor(Tki1[YEAR == 2015] + Tki2[YEAR == 2015] + Tki3[YEAR == 2015] + Tfr[YEAR == 2015] + Sct_1y[YEAR == 2015] + Sct_2y[YEAR == 2015] + Prog[YEAR == 2015]),
    TFR_2020 = floor(Tki1[YEAR == 2020] + Tki2[YEAR == 2020] + Tki3[YEAR == 2020] + Tfr[YEAR == 2020] + Sct_1y[YEAR == 2020] + Sct_2y[YEAR == 2020] + Prog[YEAR == 2020]),
    TFR_2025 = floor(Tki1[YEAR == 2025] + Tki2[YEAR == 2025] + Tki3[YEAR == 2025] + Tfr[YEAR == 2025] + Sct_1y[YEAR == 2025] + Sct_2y[YEAR == 2025] + Prog[YEAR == 2025]),
    TFR_2030 = floor(Tki1[YEAR == 2030] + Tki2[YEAR == 2030] + Tki3[YEAR == 2030] + Tfr[YEAR == 2030] + Sct_1y[YEAR == 2030] + Sct_2y[YEAR == 2030] + Prog[YEAR == 2030])
  )


# Append to final_data_selected
final_data_selected3 <- final_data_selected2 |>
                        bind_rows(df_merged_row) |>
                        bind_rows(df_merged_row2)

ratio_row <- final_data_selected3 |>
  filter(DiagYear %in% c("Total", "Prevalent cases by state", "Total prevalent cases")) |>
  dplyr::select(-Phase) |>
  pivot_longer(-DiagYear, names_to = "variable", values_to = "value") |>
  pivot_wider(names_from = DiagYear, values_from = value) |>
  mutate(value = `Prevalent cases by state` / `Total`) |>
  dplyr::select(variable, value) |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(DiagYear = "Total-Ratio", Phase = "")

# Apply the ratio to relevant columns
final_data_selected4 <- final_data_selected3 |>
  filter(!DiagYear %in% c("Total", "Prevalent cases by state", "Total prevalent cases")) |>
  mutate(across(
    starts_with(c("1L TKI", "2L TKI", "3L+ TKI", "Progression", "AlloSCT", "TFR")),
    ~ . * unlist(ratio_row[1, cur_column()]),
    .names = "{.col}"
  )) |>
  bind_rows(df_merged_row) |>
  bind_rows(df_merged_row2) |>
  mutate(across(-c(DiagYear, Phase), ~ round(as.numeric(.), 1)))

sum(subset(final_data_selected4, DiagYear != "Prevalent cases by state" & DiagYear != "Total prevalent cases")$`1L TKI_2015`)
print(subset(df_merged, YEAR ==2015)$Tki1) # This one is TRUE

# Create flextable
ft <- flextable(final_data_selected4) |>
      autofit() |>
      set_header_labels(
        DiagYear = "Year at diagnosis",
        Phase = "Phase at diagnosis",
        INC.CASES = "Incident cases",
        `1L TKI_2015` = "1L TKI",
        `2L TKI_2015` = "2L TKI",
        `3L+ TKI_2015` = "3L+ TKI",
        `Progression_2015` = "Progression",
        `AlloSCT_2015` = "AlloSCT",
        `TFR_2015` = "TFR",
        `1L TKI_2020` = "1L TKI",
        `2L TKI_2020` = "2L TKI",
        `3L+ TKI_2020` = "3L+ TKI",
        `Progression_2020` = "Progression",
        `AlloSCT_2020` = "AlloSCT",
        `TFR_2020` = "TFR",
        `1L TKI_2025` = "1L TKI",
        `2L TKI_2025` = "2L TKI",
        `3L+ TKI_2025` = "3L+ TKI",
        `Progression_2025` = "Progression",
        `AlloSCT_2025` = "AlloSCT",
        `TFR_2025` = "TFR",
        `1L TKI_2030` = "1L TKI",
        `2L TKI_2030` = "2L TKI",
        `3L+ TKI_2030` = "3L+ TKI",
        `Progression_2030` = "Progression",
        `AlloSCT_2030` = "AlloSCT",
        `TFR_2030` = "TFR") |>
        add_header_row(
          values = c("", "2015", "2020", "2025", "2030"),
          colwidths = c(3, 6, 6, 6, 6)) |>  
        align(align = c("left"), i = 1:2, j = NULL, part = "header") |>
        border(i = nrow(final_data_selected4) - 2, border.top = officer::fp_border())|>
        add_footer_lines("CP, chronic phase; AP, accelerated phase; BP, blastic phase; 1L, first-line; 2L, second-line; 3L+, third-line and later; TKI, tyrosine kinase inhibitor; AlloSCT, allogeneic stem cell transplantation.")

ft

save_as_docx(ft, path="../Output/Cost_04a2_prev_dist_tab.docx")
