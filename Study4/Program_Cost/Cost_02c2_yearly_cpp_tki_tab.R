## File name: Cost_02c2_yearly_cpp_tki_tab
## Project: CML cost  
## Purpose: To obtain average yearly cost per patient on substance lv (TKI) in a tabulated format
## Output: Cost_02c2_yearly_cpp_tki_tab.docx

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
library(tibble)
library(flextable)

# Create an empty data frame 
# First I want a data frame with item and 2015 to 2030 as colnames

df <- data.frame(matrix(ncol = 19, nrow = 0))
colname <- c("cat", "line", "item", "`2015`", "`2016`", "`2017`", "`2018`", "`2019`", "`2020`", 
             "`2021`", "`2022`", "`2023`", "`2024`", "`2025`", "`2026`", 
             "`2027`", "`2028`", "`2029`", "`2030`")
colnames(df) <- colname

###############
## Read data ##
###############
data <- readRDS(file = "../Data/Cost_02a_yearly_cpp_tki.rds")

############
## 1L TKI ##
############
data2 <- data |>
         filter(year >= 2015) |>
         pivot_wider(names_from = year, values_from = ncpp) |>
         as.data.frame() |>
         mutate(across(-c("line", "item"), ~ round(.x, 2))) |>
         select(line, item, everything()) |>
         select(-cpp)

print(data2[data2$item == "IMA",])

processed_data <- data2 %>%
  # Pivot year columns (e.g., 2015, 2016) to a long format.
  # This handles cases where R might prepend 'X' to numeric column names (e.g., X2015).
  tidyr::pivot_longer(
    cols = matches("^(X)?[0-9]{4}$"), # Matches columns like "2015", "X2015", etc.
    names_to = "year_str",
    values_to = "cpp_for_year_val", # This is the value from the specific year column
    values_drop_na = TRUE # Keep only rows where a year column has a non-NA value
  ) %>%
  # Convert the year string (e.g., "X2015") to a numeric year (e.g., 2015)
  dplyr::mutate(year = readr::parse_number(year_str)) %>%
  # Group by the identifying columns and the newly extracted year
  dplyr::group_by(line, item, year) %>%
  # Summarize to get the cpp, price_prop, and prop for each year.
  # 'cpp_for_year_val' is the specific cpp for that year.
  # 'price_prop' and 'prop' are taken from the original row corresponding to that year's data.
  dplyr::summarise(
    cpp_value = first(cpp_for_year_val),
    price_prop_value = first(price_prop),
    prop_value = first(prop),
    .groups = 'drop' # Ungroup after summarising
  ) %>%
  # Pivot longer to create a 'category' column for cpp, price_prop, and prop
  tidyr::pivot_longer(
    cols = c(cpp_value, price_prop_value, prop_value),
    names_to = "category",
    values_to = "value"
  ) %>%
  # Clean up category names (e.g., "cpp_value" to "cpp")
  dplyr::mutate(category = sub("_value", "", category)) %>%
  # Pivot wider to create the final structure: line, item, 2015_cpp, 2015_price_prop, 2015_prop, ...
  tidyr::pivot_wider(
    names_from = c(year, category),
    names_sep = "_", # Separator for combined names (e.g., "2015_cpp")
    values_from = "value"
  ) %>%
  # Arrange the data for consistent output
  dplyr::arrange(line, item)

# Step 2: Create the Flextable

# Dynamically determine the column order and header labels based on the columns in processed_data
# This ensures that only years present in the data are included.
present_cols_final <- names(processed_data)
year_category_cols_final <- present_cols_final[!present_cols_final %in% c("line", "item")]

# Parse year and category from column names (e.g., "2015_cpp")
col_parts <- stringr::str_match(year_category_cols_final, "^([0-9]{4})_(cpp|price_prop|prop)$")

# Get unique sorted years from the parsed column names
if (nrow(col_parts) > 0 && any(!is.na(col_parts[,2]))) {
  unique_years_sorted <- sort(unique(as.numeric(col_parts[,2[!is.na(col_parts[,2])]])))
} else {
  unique_years_sorted <- numeric(0) # No year columns found / matched
}

# Prepare flextable column keys and header definitions
ft_col_keys <- c("line", "item")
ft_header_row1_labels <- c("Line", "TKI") # Your specified labels for the first two columns
ft_header_row2_labels <- c("", "")       # Second header row is empty for these two

# Populate headers for year-based columns
for (yr in unique_years_sorted) {
  yr_str <- as.character(yr)
  # Define the column names for this year's cpp, price_prop, and prop
  cols_for_year <- paste0(yr_str, "_", c("cpp", "price_prop", "prop"))
  
  # Add these column keys to the list
  ft_col_keys <- c(ft_col_keys, cols_for_year)
  
  # For the first header row, the year spans three sub-columns
  ft_header_row1_labels <- c(ft_header_row1_labels, yr_str, "", "")
  # For the second header row, these are the sub-column names
  ft_header_row2_labels <- c(ft_header_row2_labels, "cpp", "price_prop", "prop")
}

# Create the flextable object using the processed data and defined column keys
# This ensures the columns appear in the desired order.
ft <- flextable::flextable(processed_data, col_keys = ft_col_keys)

# Define the header structure as a data.frame for set_header_df
header_df <- data.frame(
  col_keys = ft_col_keys,
  row1 = ft_header_row1_labels,
  row2 = ft_header_row2_labels,
  stringsAsFactors = FALSE
)

# Apply the custom header
ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

# Merge cells in the header for the desired hierarchical look
ft <- flextable::merge_h(ft, part = "header", i = 1) # Merge year cells in the first header row
ft <- flextable::merge_v(ft, part = "header", j = c("line", "item")) # Merge "Line" and "TKI" titles vertically

# Optional: If 'line' values should be merged in the table body when they are repeated
# ft <- flextable::merge_v(ft, j = "line", part = "body")

# Apply basic styling
ft <- flextable::align(ft, align = "center", part = "header") # Center align header text
ft <- flextable::align(ft, j = c("line", "item"), align = "left", part = "body") # Left align 'Line' and 'TKI' columns in body
ft <- flextable::autofit(ft) # Adjust column widths

# You can add a theme if desired, e.g.:
# ft <- flextable::theme_box(ft)

# Print the flextable (this will display it in RStudio's viewer or an interactive session)
print(ft)

save_as_docx(ft, path="../Output/Cost_02c_yearly_cpp_tab.docx")

# Save the data frame
# saveRDS
# saveRDS(df_new, file="../Data/Cost_02c_yearly_cpp_tab.rds")
# save_as_docx(ft, path="../Output/Cost_02c_yearly_cpp_tab.docx")
# save excel
# save_as_xlsx(ft, path="../Output/Cost_02c_yearly_cpp_tab.xlsx")
