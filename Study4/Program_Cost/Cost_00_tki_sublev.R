## File name: Cost_00_tki_sublev.R 
## Updated: 2025-02-23 Enoch Chen: Organise the tki data in the substance level
## Project: CML cost  
## Notes:FIXME 20250223
## Calculate the price change
## Merge by Product name, package size and dose
## Show 2018 from Prescribed Drug Register versus 2019-2025 from TLV
## Data would look like
## Product name | Package size | Dose | price_2018 | price_2019

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
library(readxl)
library(lubridate)
library(tools)

####################################################################
## Read Prescribed Drug Register of the Swedish CML register Data ##
####################################################################
setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")
drug <- read_sas("r_lmed_kml_11731_2018.sas7bdat", encoding="latin1")
colnames(drug)

####################
## Rename columns ##
####################
df   <- drug |>
        rename(
          expdate = EDATUM,       # "expendition date"
          year = AR,              # "year of expendition date"
          prescrdate = FDATUM,    # "prescription date"
          amount = ANTAL,         # "number of packages"
          product = produkt,      # "drug product"
          productname = lnmn,     # "drug name"
          atc = atc,              # "atc code"
          cost_total = TKOST,  # "total cost"
          cost_lan = LANKOST,     # "cost to county"
          cost_patient = PATKOST, # "cost to patient"
          pkgsize = forps,        # "package size" 
          dose = styrkalf         # "dose + unit"
          )|>   
          dplyr::select(LopNr, expdate, year, amount, product, productname, 
                 atc, cost_total, cost_lan, cost_patient, pkgsize, dose) |>
          # Remove cost 0
          filter(cost_lan > 0)|>
          # Calculate the cost (healthcare perspective) per package
          mutate(cost_pkg = cost_lan/amount)


#################################
##  Stratify by atc code first ##
#################################
df_ima <- df[df$atc=="L01XE01",]|>
          mutate(pkgsize_n = as.numeric(sub("\\D+", "", pkgsize))) |>
          dplyr::select(product, dose, pkgsize_n, cost_pkg, year)

df_ima_2018 <-subset(df_ima, year==2018)
table(df_ima_2018$product)

# histogram for cost_pkg
hist(df_ima_2018$cost_pkg)

# histogram for dose
table(df_ima_2018$dose)

# histogram for cost_pkg by dose
hist(df_ima_2018$cost_pkg[df_ima_2018$dose=="100 mg"])
hist(df_ima_2018$cost_pkg[df_ima_2018$dose=="400 mg"])

# Tabulate pkgsize_n by dose
table(df_ima_2018$product[df_ima_2018$dose=="100 mg"])

ima_cmlreg <- df_ima_2018|>
              group_by(product, dose, pkgsize_n, year) |>
              summarise(price = round(mean(cost_pkg),0), .groups = "drop") |>
              rename(pkgsize = pkgsize_n)|>
              mutate(tki = "imatinib", tlv = 0)|>
              dplyr::select(tki, tlv, product, dose, pkgsize, year, price)

###############
## Dasatinib ##
###############
df_das <- df[df$atc=="L01XE06",] |>
          mutate(pkgsize_n = as.numeric(substr(pkgsize, 1, 2)))|>
          dplyr::select(product, dose, pkgsize_n, cost_pkg, year)

df_das_2018 <-subset(df_das, year==2018)
table(df_das_2018$product)

# histogram for cost_pkg
hist(df_das_2018$cost_pkg)

# tabulate for dose
table(df_das_2018$dose)

das_cmlreg <- df_das_2018|>
              group_by(product, dose, pkgsize_n, year) |>
              summarise(price = round(mean(cost_pkg),0), .groups = "drop") |>
              rename(pkgsize = pkgsize_n)|>
              mutate(tki = "dasatinib", tlv = 0)|>
              dplyr::select(tki, tlv, product, dose, pkgsize, year, price)

print(das_cmlreg)

# Dose 20 mg all have 60 tablets per pkg, with mean price per pkg 20467 SEK
# Dose 50 mg all have 60 tablets per pkg, with mean price per pkg 41139 SEK
# Dose 70 mg all have 60 tablets per pkg, with mean price per pkg 41148 SEK
# Dose 80 mg all have 30 tablets per pkg, with mean price per pkg 41262 SEK
# Dose 100 mg all have 30 tablets per pkg, with mean price per pkg 41207 SEK
# Dose 140 mg all have 30 tablets per pkg, with mean price per pkg 41271 SEK

###############
## Nilotinib ##
###############
df_nil <- df[df$atc=="L01XE08",]
df_nil_2018 <- subset(df_nil, year==2018) |>
               mutate(pkgsize_n = as.numeric(substr(pkgsize, 1, 3)))|>
               dplyr::select(product, dose, pkgsize_n, cost_pkg, year)


table(df_nil_2018$product)
hist(df_nil_2018$cost_pkg)
table(df_nil_2018$dose)


nil_cmlreg <- df_nil_2018|>
              group_by(product, dose, pkgsize_n, year) |>
              summarise(price = round(mean(cost_pkg),0), .groups = "drop") |>
              rename(pkgsize = pkgsize_n)|>
              mutate(tki = "nilotinib", tlv = 0)|>
              dplyr::select(tki, tlv, product, dose, pkgsize, year, price)

print(nil_cmlreg)
print(tlv[tlv$tki=="nilotinib",])

# Dose 150 mg all have 112 tablets per pkg, with mean price per pkg 23950 SEK
# Dose 200 mg all have 112 tablets per pkg, with mean price per pkg 38442 SEK

###############
## Bosutinib ##
###############
df_bos <- df[df$atc=="L01XE14",] |>
          mutate(pkgsize_n = as.numeric(substr(pkgsize, 1, 2)),
                 product = ifelse(product=="Bosulif®", "Bosulif", product))|>
         dplyr::select(product, dose, pkgsize_n, cost_pkg, year)


df_bos_2018 <-subset(df_bos, year==2018)
table(df_bos_2018$product)
hist(df_bos_2018$cost_pkg)
table(df_bos_2018$dose)

bos_cmlreg <- df_bos_2018|>
              group_by(product, dose, pkgsize_n, year) |>
              summarise(price = round(mean(cost_pkg),0), .groups = "drop") |>
              rename(pkgsize = pkgsize_n)|>
              mutate(tki = "bosutinib", tlv = 0)|>
              dplyr::select(tki, tlv, product, dose, pkgsize, year, price)

print(bos_cmlreg)

# Dose 100 mg all have 28 tablets per pkg, with mean price per pkg 8541 SEK
# Dose 400 mg all have 28 tablets per pkg, with mean price per pkg 34292 SEK
# Dose 500 mg all have 28 tablets per pkg, with mean price per pkg 34285 SEK

###############
## Ponatinib ##
###############
df_pon <- df[df$atc=="L01XE24",] |>
          mutate(pkgsize_n = as.numeric(substr(pkgsize, 1, 2))) |>
          dplyr::select(product, dose, pkgsize_n, cost_pkg, year)

df_pon_2018 <- subset(df_pon, year==2018)
table(df_pon_2018$product)
hist(df_pon_2018$cost_pkg)
table(df_pon_2018$dose)

pon_cmlreg <- df_pon_2018|>
              group_by(product, dose, pkgsize_n, year) |>
              summarise(price = round(mean(cost_pkg),0), .groups = "drop") |>
              rename(pkgsize = pkgsize_n)|>
              mutate(tki = "ponatinib", tlv = 0)|>
              dplyr::select(tki, tlv, product, dose, pkgsize, year, price)

print(pon_cmlreg)

# Dose 15 mg all have 30 tablets per pkg, with mean price per pkg 32481 SEK
# Dose 30 mg all have 30 tablets per pkg, with mean price per pkg 64554 SEK
# Dose 45 mg all have 30 tablets per pkg, with mean price per pkg 64607 SEK

#############
## Inferon ##
#############
## No patient was prescribed with Inferon in 2018
df_inf <- df[df$atc=="L03AB",]

########################
## Combine all cmlreg ##
########################
cmlreg <- rbind(ima_cmlreg, das_cmlreg, nil_cmlreg, bos_cmlreg, pon_cmlreg)

#######################
## Read TLV database ##
#######################
# Source: https://www.tlv.se/beslut/sok-priser-och-beslut-i-databasen.html
setwd("/Users/yitche/Library/CloudStorage/OneDrive-KarolinskaInstitutet/ec_phd/Study4_Cost/Data/TLV")

# Function to process TLV cost datasets
process_tlv <- function(file) {
  read_excel(file) |>
    dplyr::select(Produktnamn, Styrka, Antal, `Gäller fr.o.m.`, `Nytt AIP`, `Tidigare AIP`) |>
    rename(
      product = Produktnamn,
      dose = Styrka,
      pkgsize = Antal,
      date = `Gäller fr.o.m.`,
      price = `Nytt AIP`,
      price_old = `Tidigare AIP`
    ) |>
    filter(!is.na(price)) |>
    mutate(
      date = as.Date(date),
      year = as.integer(year(date)),
      price = as.numeric(gsub(",", ".", price),0),
      price_old = as.numeric(gsub(",", ".", price_old),0),
      price_increase = (price-price_old)/price_old 
     )|>
    filter(date >= as.Date("2018-01-01"),
           price_increase <= 1) |> # We thought that price increase should not exceed 100%
    group_by(product, dose, pkgsize, year) |>
    summarise(price = round(mean(price),0), .groups = "drop") |>
    mutate(tki = tools::file_path_sans_ext(basename(file)), # File name without extension
           tlv = 1)|>  # Add indicator for TLV data 
    dplyr::select(tki, tlv, product, dose, pkgsize, year, price) 
  }

# Process each TKI dataset
ima_tlv <- process_tlv("imatinib.xlsx")
  table(ima_tlv$product, ima_tlv$dose)
  table(ima_cmlreg$product, ima_cmlreg$dose)
  
  # Remove drugs in TLV database that are not in the CML register
  ima_tlv <- ima_tlv[!ima_tlv$product %in% c("Imatinib Cipla",
                                            "Imatinib Viatris",
                                            "Imatinib Zentiva"),]

das_tlv <- process_tlv("dasatinib.xlsx")
  table(das_tlv$product, das_tlv$dose)
  table(das_cmlreg$product, das_cmlreg$dose)

nil_tlv <- process_tlv("nilotinib.xlsx")
  table(nil_tlv$product, nil_tlv$dose)
  table(nil_cmlreg$product, nil_cmlreg$dose)
  
bos_tlv <- process_tlv("bosutinib.xlsx")
  table(bos_tlv$product, bos_tlv$dose)
  table(bos_cmlreg$product, bos_cmlreg$dose)
  
pon_tlv <- read_excel("ponatinib.xlsx")|>
           rename(nplpackid = 5) |>
           mutate(year = substr(nplpackid, 1, 4)) |>
           filter(year >= 2019) |>
           dplyr::select(Produktnamn, year, Styrka, Antal, AIP)|>
           rename(product = Produktnamn, 
                  dose = Styrka, 
                  pkgsize = Antal)|>
           mutate(tki = "ponatinib", tlv = 1,
                  price = as.numeric(gsub(",", ".", AIP),0)) |>
          dplyr::select(tki, tlv, product, dose, pkgsize, year, price)
  table(pon_tlv$product, pon_tlv$dose)
  table(pon_cmlreg$product, pon_cmlreg$dose)

asc_tlv <- read_excel("asciminib.xlsx")|>
           rename(nplpackid = 5) |>
           mutate(year = substr(nplpackid, 1, 4)) |>
           filter(year >= 2019) |>
           dplyr::select(Produktnamn, year, Styrka, Antal, AIP)|>
           rename(product = Produktnamn, 
                  dose = Styrka, 
                  pkgsize = Antal)|>
           mutate(tki = "asciminib", tlv = 1, 
                  price = as.numeric(gsub(",", ".", AIP),0)) |>
           dplyr::select(tki, tlv, product, dose, pkgsize, year, price)

## Merge the TLV data
tlv <- rbind(ima_tlv, das_tlv, nil_tlv, bos_tlv, pon_tlv, asc_tlv)
colnames(tlv)

########################
## Merge all the data ##
########################
costdata <- rbind(cmlreg, tlv) |>
            arrange(tki, dose, pkgsize, product, tlv, year) |>
            ## Get rid of unit mg and turn into numeric
            mutate(dose_mg = as.numeric(gsub(" mg", "", dose)),
                   pkgsize = as.numeric(pkgsize),
                   price = as.numeric(price),
                   price_st = price / pkgsize) |>
            dplyr::select(tki, tlv, dose_mg, product, year, price, pkgsize, price_st)

# Only one price for each dose per year                        
costdata2 <- costdata |>
             group_by(tki, dose_mg, year) |>
             summarise(price_st = round(mean(price_st),0), .groups = "drop") |>
             mutate(tlv = ifelse(year == 2018, 0, 1),
                    price_st_2018 = ifelse(year==2018, price_st, 0)) |>
             dplyr::select(tki, tlv, dose_mg, year, price_st, price_st_2018) |>
             group_by(tki, dose_mg) |>
             mutate(price_st_2018 = first(price_st_2018)) |>
             filter(price_st_2018 > 0)

## Group by TKI and dose generate a new column for price change compared to price in 2018
costdata3 <- costdata2 |> 
             mutate(price_change = (price_st - price_st_2018)/price_st_2018)
             
## Group by TKI and year to gen the mean price change
costdata4 <- costdata3 |> 
             group_by(tki, year) |>
             summarise(price_change = round(mean(price_change),2), .groups = "drop")

setwd(wd)
saveRDS(costdata4, file="../Data/Cost_00_tki_sublev.rds")
