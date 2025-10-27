## Updated: 2024-08-13 Enoch Chen: Make Table 1 in R
## Purpose: Make descriptive statistics 

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

## Set decimals 
options("scipen"=100, "digits"=3)

## Read packages
library(readstata13)
library(flextable)
library(dplyr)
library(tidyr)

###################
##===============##
## Read the data ##
##===============##
###################
## Read data
data <- readRDS(file = "../Data/01b_studypop_multistate.rds") %>%
        mutate(sex = ifelse(sex==1, "male", "female"))
str(data)

## Generate agegroup
data <- data %>%
        mutate(agegroup = cut(dxage,
               breaks = c(18, 49, 59, 69, 79, 99),
               labels = c("ag1", "ag2", "ag3", "ag4", "ag5"),
               right = TRUE,  # Include the right endpoint (e.g., 49 in group1)
               include.lowest = TRUE))  # Include the lowest value in the first group

# Check the agegroup
table(data$agegroup)

##########################
##======================##
## Run items separately ##
##======================##
##########################
################################################
# Row 1: Distribution of patients by age group #
################################################
# Calculate the counts and percentages
agegroup_table <- table(data$agegroup)
agegroup_percent <- prop.table(agegroup_table) * 100

# Combine the counts and percentages into a data frame
agegroup_summary <- data.frame(
  AgeGroup = names(agegroup_table),
  Count = as.vector(agegroup_table),
  Percentage = round(as.vector(agegroup_percent), 1)
)

# Print the summary
print(agegroup_summary)

########################################
# Row 2: Median age at dx, by agegroup #
########################################
data %>%
  group_by(agegroup) %>%
  summarize(median_dxage = median(dxage, na.rm = TRUE))

# Overall median age at dx
median(data$dxage, na.rm = TRUE)

##############
# Row 3: Sex #
##############
# Create a contingency table of agegroup by sex
table_sex <- table(data$agegroup, data$sex)

# Calculate the row-wise percentages
percent_sex <- round(prop.table(table_sex, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_sex_summary <- as.data.frame.matrix(table_sex)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_sex))) {
  table_sex_summary[, i] <- paste0(table_sex[, i], " (", percent_sex[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_sex)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_sex_summary <- rbind(table_sex_summary, Total = total_row)

# Print the summary
print(table_sex_summary)

################
# Row 4: Death #
################
data <- data %>%
        mutate(death = ifelse(is.na(date_death), 0, 1))

# Create a contingency table of agegroup by death
table_death <- table(data$agegroup, data$death)

# Calculate the row-wise percentages
percent_death <- round(prop.table(table_death, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_death_summary <- as.data.frame.matrix(table_death)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_death))) {
  table_death_summary[, i] <- paste0(table_death[, i], " (", percent_death[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_death)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_death_summary <- rbind(table_death_summary, Total = total_row)

# Print the summary
print(table_death_summary)

######################
# Row 5: Progression #
######################
data <- data %>%
        mutate(progress = ifelse(is.na(date_prog), 0, 1))

# Create a contingency table of agegroup by progress
table_prog <- table(data$agegroup, data$progress)

# Calculate the row-wise percentages
percent_prog <- round(prop.table(table_prog, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_prog_summary <- as.data.frame.matrix(table_prog)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_prog))) {
  table_prog_summary[, i] <- paste0(table_prog[, i], " (", percent_prog[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_prog)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_prog_summary <- rbind(table_prog_summary, Total = total_row)

# Print the summary
print(table_prog_summary)

##################
# Row 6: AlloSCT #
##################
data <- data %>%
  mutate(sct = ifelse(is.na(date_sct), 0, 1))

# Create a contingency table of agegroup by sct
table_sct <- table(data$agegroup, data$sct)

# Calculate the row-wise percentages
percent_sct <- round(prop.table(table_sct, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_sct_summary <- as.data.frame.matrix(table_sct)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_sct))) {
  table_sct_summary[, i] <- paste0(table_sct[, i], " (", percent_sct[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_sct)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_sct_summary <- rbind(table_sct_summary, Total = total_row)

# Print the summary
print(table_sct_summary)

#########################
# Row 7: Imatinib as 1L #
#########################
data <- data %>%
        mutate(ima = ifelse(tki1 == "IMA", 1, 0))

# Create a contingency table of agegroup by ima
table_ima <- table(data$agegroup, data$ima)

# Calculate the row-wise percentages
percent_ima <- round(prop.table(table_ima, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_ima_summary <- as.data.frame.matrix(table_ima)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_ima))) {
  table_ima_summary[, i] <- paste0(table_ima[, i], " (", percent_ima[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_ima)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_ima_summary <- rbind(table_ima_summary, Total = total_row)

# Print the summary
print(table_ima_summary)

##############
# Row 8: TFR #
##############
data <- data %>%
  mutate(tfr = ifelse(is.na(date_tfr_start), 0, 1))

# Create a contingency table of agegroup by tfr
table_tfr <- table(data$agegroup, data$tfr)

# Calculate the row-wise percentages
percent_tfr <- round(prop.table(table_tfr, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_tfr_summary <- as.data.frame.matrix(table_tfr)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_tfr))) {
  table_tfr_summary[, i] <- paste0(table_tfr[, i], " (", percent_tfr[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_tfr)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_tfr_summary <- rbind(table_tfr_summary, Total = total_row)

# Print the summary
print(table_tfr_summary)

############################
# Row 9: TKI re-initiation #
############################
# Create a contingency table of agegroup by tfr
table_tfr_end <- table(data$agegroup, data$tfr_end)

# Calculate the row-wise percentages
percent_tfr_end <- round(prop.table(table_tfr_end, margin = 1) * 100, 1)

# Convert the table to a data frame for easier manipulation
table_tfr_end_summary <- as.data.frame.matrix(table_tfr_end)

# Loop to combine counts and percentages into one column
for (i in seq_len(ncol(table_tfr_end))) {
  table_tfr_end_summary[, i] <- paste0(table_tfr_end[, i], " (", percent_tfr_end[, i], "%)")
}

# Calculate the total counts and percentages for each sex across all age groups
total_counts <- colSums(table_tfr_end)
total_percent <- round(prop.table(total_counts) * 100, 1)
total_row <- paste0(total_counts, " (", total_percent, "%)")

# Add the total row to the summary table
table_tfr_end_summary <- rbind(table_tfr_end_summary, Total = total_row)

# Print the summary
print(table_tfr_end_summary)

#################################

