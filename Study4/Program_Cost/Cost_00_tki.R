## File name: Cost_00_tki.R 
## Updated: 2024-11-19 Enoch Chen: take cost_lan instead of cost_total (healthcare sector perspective)
##          2024-06-26 Enoch Chen: Merged duplicates TKI costs on the same dates
##          2024-06-17 Enoch Chen: Retrieve TKI information
## Project: CML cost  
## Notes:

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
library(haven)
library(dplyr)
library(tidyr)

#######################################
## Read Swedish Cancer Register Data ##
#######################################
setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")
drug <- read_sas("r_lmed_kml_11731_2018.sas7bdat", encoding="latin1")
studies <- read_dta("/Users/yitche/SweCan_CML/Data/Clean_data/studies.dta")

####################
## Rename columns ##
####################
df   <- drug %>%
        rename(
          expdate = EDATUM,       # "expendition date"
          prescrdate = FDATUM,    # "prescription date"
          amount = ANTAL,         # "number of packages"
          product = produkt,      # "drug product"
          productname = lnmn,     # "drug name"
          dddunit = dddenhet,     # "defined daily doses" 
          pkgsize = forps,        # "package size" 
          dose = styrkalf,        # "dose"
          dose_n = styrknum,      # "dose number"
          doseunit = styrkaenhet, # "dose unit"
          drugtype = lform,       # "drug type"
          drugname = subnamn      # "drug name"
        )

################################
## Generate drug based on atc ##
################################
df <- df %>%
      mutate(
        drug = case_when(
          atc == "L01XE01" ~ "IMA",
          atc == "L01XE06" ~ "DAS",
          atc == "L01XE08" ~ "NIL",
          atc == "L01XE14" ~ "BOS",
          atc == "L01XE24" ~ "PON",
          atc == "L03AB" ~ "IFN"
        )
      )

##############################
## Rename additional colums ##
##############################
df <- df %>%
      rename(
        idnr = LopNr,
        date_tki = expdate,
        tki = drug,
        cost_total = TKOST,
        cost_lan = LANKOST,
        cost_self = PATKOST,
        cost_addition = MERKOST,
        description = DOSER
      )

# Convert date_tki to date format
df$date_tki <- as.Date(df$date_tki)

################################
## Keep non-missing data only ##
################################
# Keep rows where tki is not empty and 
df <- df %>%
      filter(tki != "") %>%
      filter(!is.na(date_tki))%>% # date_tki is not missing
      select(idnr, date_tki, tki, cost_total, cost_lan, cost_self, description, dose)%>% # Keep only relevant columns
      arrange(idnr, date_tki) # Sort by idnr and date_tki

# Drop rows with negative cost values
df2 <- df %>%
       filter(cost_total > 0, cost_lan >= 0, cost_self >= 0)

# Drop idnr 16678 75399 had drug prescription during TFR period.
# Torsten Dahlen suggested to move that single record.
df2 <- df2 %>%
       filter(!(idnr == 16678 & date_tki == as.Date("2013-05-23")) &
              !(idnr == 75399 & date_tki == as.Date("2017-07-15")) &
              !(idnr == 87659 & tki == "NIL") & # This NIL record showed out of nowhere
              !(idnr == 77769 & date_tki == as.Date("2014-08-19")))  # This NIL record showed out of nowhere

# idnr 65819's IMA date was unreasonably close to NIL
# change it to diagdate 2011-04-11
df2 <- df2 %>%
       mutate(date_tki = if_else(idnr == 65819 & tki == "IMA", as.Date("2011-04-11"), date_tki))

print(subset(df2, idnr == 65819 & tki == "IMA"))

#################################
## See the distribution of TKI ##
#################################
table(df2$tki, useNA = "ifany")

######################################
## Remove duplicates of TKI records ##
######################################
# Keep those with one record only
df2_subpop1 <- df2 %>%
               group_by(idnr, date_tki, tki) %>%
               filter(n()==1) 

# Those with two records, sum the cost for the same day
df2_subpop2 <- df2 %>%
               group_by(idnr, date_tki, tki) %>%
               filter(n() > 1) %>%
               summarize(cost_lan = sum(cost_lan)) %>%
               distinct() %>%
               ungroup()

# Assemble the study population together after removing duplicates
df3 <- df2_subpop1 %>%
       rows_append(df2_subpop2) %>%
       arrange(idnr, date_tki) %>%
       select(idnr, date_tki, tki, cost_lan)

temp <- subset(df3, idnr == 73629)

##################
## Save dataset ##
##################
setwd(wd)
saveRDS(df3, file = "../Data/Cost_00_tki.rds")
