## File name: PIAMOD_02a1_NS_pp.R
## Updated: 20250207 Enoch Chen: use relsurv package to estimate net survival
##          20241112 Enoch Chen: tabulated relative survival the same as in the Stata code strs
## Project: CML Cost
## Purpose: Use Pohar-Perme estimator to estimate net survival
## Input:   PIAMOD_01_analysisdata_cml.dta
## Output:  PIAMOD_02a1_NS_pp.txt

library(relsurv)
library(haven)
library(tidyverse)
library(tidyr)

####################
## Make ratetable ##
####################
# Data downloaded from https://www.mortality.org/Country/Country?cntr=SWE
# Year range: 1751-2023

# Use relsurv::transrate to make the ratetable
ratetable <- transrate.hmd(male = "../Data/lifetable_swe_1y_males.txt",
                           female = "../Data/lifetable_swe_1y_males.txt")

######################################################
## Read CML survival data and estimate net survival ##
######################################################
# Read Swedish Cancer Register CML data
cml <- read_dta("../Data/PIAMOD_01_analysisdata_cml.dta")|>
       mutate(surv_dd = floor(stime * 365.241),
              age_dd = floor(agediag * 365.241),
              sex_fac = as.factor(ifelse(sex==1,"male","female")))

rs_fit_pp <- rs.surv(Surv(surv_dd, dead == 1) ~ sex + agegrp + yeargrp,
                     data      = cml,
                     ratetable = ratetable,
                     method    = "pohar-perme",
                     rmap      = list(age = age_dd, 
                                      sex = sex_fac,
                                      year = diagdate))
str(rs_fit_pp)

# Summary with times (e.g., each year up to 20 years)
# CAUTION: PIAMOD requires "Follow up duration in six month intervals from date of diagnosis, expressed as integer
# value (1:200). For ex. 1=6 months, 2=12 months, 3=18 months
rs_summary <- summary(rs_fit_pp, times = seq(0.5, 20, 0.5) * 365.241)

# Convert the summary object into a data frame
rs_df <- data.frame(
  time     = rs_summary$time,
  surv     = rs_summary$surv,
  std_err  = rs_summary$std.err,
  lower_ci = rs_summary$lower,
  upper_ci = rs_summary$upper,
  strata   = rs_summary$strata
)

# Split the strata to get sex, agegrp, and yeargrp as separate columns
rs_df <- rs_df %>%
         separate(strata, into = c("sex", "agegrp", "yeargrp"), sep = ",\\s*") %>%
         mutate(
            sex = as.numeric(gsub("sex=", "", sex)),
            agegrp = as.numeric(gsub("agegrp=", "", agegrp)),
            yeargrp = as.numeric(gsub("yeargrp=", "", yeargrp)),
            fu_6mo = time / (365.241/2),
          )

# Organise the data frame
rs_tabulated <- rs_df |>
                mutate(filterfield = case_when(
                       sex == 1 ~ "SECML_M",
                       sex == 2 ~ "SECML_F",
                       TRUE ~ "")) %>%
                mutate(
                  # Period of Diagnosis - First Year
                  firstyear = case_when(
                    yeargrp == 0  ~ 1973, yeargrp == 1  ~ 1976, yeargrp == 2  ~ 1979, yeargrp == 3  ~ 1982,
                    yeargrp == 4  ~ 1985, yeargrp == 5  ~ 1988, yeargrp == 6  ~ 1991, yeargrp == 7  ~ 1994,
                    yeargrp == 8  ~ 1997, yeargrp == 9  ~ 2000, yeargrp == 10 ~ 2003, yeargrp == 11 ~ 2006,
                    yeargrp == 12 ~ 2009, yeargrp == 13 ~ 2012, yeargrp == 14 ~ 2015,
                    TRUE ~ NA_real_
                  ),
                  # Period of Diagnosis - Last Year
                  lastyear = case_when(
                    yeargrp == 0  ~ 1975, yeargrp == 1  ~ 1978, yeargrp == 2  ~ 1981, yeargrp == 3  ~ 1984,
                    yeargrp == 4  ~ 1987, yeargrp == 5  ~ 1990, yeargrp == 6  ~ 1993, yeargrp == 7  ~ 1996,
                    yeargrp == 8  ~ 1999, yeargrp == 9  ~ 2002, yeargrp == 10 ~ 2005, yeargrp == 11 ~ 2008,
                    yeargrp == 12 ~ 2011, yeargrp == 13 ~ 2014, yeargrp == 14 ~ 2019,
                    TRUE ~ NA_real_
                  ),
                  # Age at Diagnosis - Youngest
                  youngestage = case_when(
                    agegrp == 0 ~ 20,
                    agegrp == 1 ~ 45,
                    agegrp == 2 ~ 55,
                    agegrp == 3 ~ 65,
                    agegrp == 4 ~ 75,
                    TRUE ~ NA_real_
                  ),
                  # Age at Diagnosis - Oldest
                  oldestage = case_when(
                    agegrp == 0 ~ 44,
                    agegrp == 1 ~ 54,
                    agegrp == 2 ~ 64,
                    agegrp == 3 ~ 74,
                    agegrp == 4 ~ 99,
                    TRUE ~ NA_real_
                  ),
                  cns_pp = surv
                  ) %>%
                  # Make cns_pp 4 decimals
                  mutate(cns_pp = round(cns_pp, 4)) %>%
                  select(filterfield, sex, firstyear, lastyear, youngestage, oldestage, fu_6mo, cns_pp) %>%
                  arrange(sex, firstyear, youngestage, fu_6mo)

# Write the tabulated relative survival to a text file, without row names
write.table(rs_tabulated, file = "../Data/PIAMOD_02a1_NS_pp.txt",  col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t")
