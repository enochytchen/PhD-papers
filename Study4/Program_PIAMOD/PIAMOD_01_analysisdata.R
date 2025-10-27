## File name: PIAMOD_01_analysisdata.R
## Updated: 2025-01-02 Enoch Chen: Fail to add incidence data from Socialstyrelsen
##                                 due to the data structure.
##          2024-10-10 Enoch Chen: re-organize codes from 
##                     /SweCan/Site_specific/Colon/Fabrizio2024/data_preparation.R
## Project: CML Cost
## Purpose: Prepare analysis data for (1) CML incidence, 
##                                    (2) population mortality
##                                    (3) population size


#############################
####### Read packages #######
#############################
library(haven)
library(dplyr)

######################################
####### Incidence data for CML #######
######################################
# Data originally from SweCan/Data
data_reg <- read_dta("../Data/PIAMOD_01_analysisdata_cml.dta")

# Check the max year
# SweCan only had data from 1973 up to 2019
min(data_reg$yeardiag)
max(data_reg$yeardiag)

site <- "cml"
type_mort <- "allcauses"
type_pop <- "pop"
area <- "Sweden"
n <- dim(data_reg)[1]
min_year <- min(data_reg$yeardiag)
max_year <- max(data_reg$yeardiag)
n_years <- max_year-min_year+1
min_age <- min(data_reg$agediag)
max_age <- max(data_reg$agediag)
n_ages <- max_age-min_age+1

# Define full range of ages (including ages 0 to 17)
# CML dx only included ages 18+. We added ages 0 to 17 for the purpose of PIAMOD's modelling
full_min_age <- 0
full_max_age <- max_age

# Update n_inc to include ages 0-17
n_inc <- n_years * (full_max_age - full_min_age + 1) * 2

incidence_data <- data.frame(site=character(n_inc),
                             sex=integer(n_inc), 
                             year=integer(n_inc), 
                             age=integer(n_inc),
                             ninc=numeric(n_inc),
                             area=character(n_inc),
                             stringsAsFactors=FALSE)
i=1
for(year in c(min_year:max_year)){
  for(age in c(full_min_age:full_max_age)){  
    for(sexn in c(1,2)){
      temp <- data_reg %>%
        filter(yeardiag==year & agediag==age & sex==sexn)
      incidence_data[i,] <- list(site, sexn, year, age, dim(temp)[1], area)
      i <- i+1
    }
  }
}
rm(temp)

incidence_data <- incidence_data[order(incidence_data$year, incidence_data$sex), ]
write.table(incidence_data,"../Data/PIAMOD_01_analysisdata_incidenceCML.txt",
            sep = "\t", col.names = FALSE,row.names = FALSE,
            quote = FALSE)

##################################################################
####### Incidence data for CML from Social Socialstyrelsen #######
##################################################################
# Read the data from Socialstyrelsen; Riket
# Downloaded from https://sdb.socialstyrelsen.se/if_can/val.aspx
# 2051 Kronisk myeloisk leukemi; 226 Myeloisk leukemi; 15+ år; Antal nya cancerfall; 1973 to 2023
# Antal nya cancerfall, Ålder: 15-85+, Riket, Diagnos:2051 Kronisk myeloisk leukemi, Tumörtyp:226 Myeloisk leukemi

# Enoch's comments: the Swedish Cancer Register data at P:/Swe_Can had dx until 2019.
# I aimed to complement the data with longer time span from Socialstyrelsen
# However, PIAMOD requires incidence data by single year and age
# Socialstyrelsen data only have 5-year age groups, which is not suitable for PIAMOD.

###########################################################
####### Prepare population mortality file of Sweden #######
###########################################################
# Data downloaded from https://www.mortality.org/Country/Country?cntr=SWE
# Year range: 1751-2023
# Name: Death Rates 1 x 1

# Read the raw data
popmort <- read.csv("../Data/genmort_raw.txt", sep="")
popmort <- popmort %>%
           filter(Year>=1973 & !Age%in%c(as.character(100:110),"110+")) # Keep those who under 100

n_mort <- dim(popmort)[1]*2 # different rows for males and females
popmort_data <- data.frame(type=character(n_mort),
                           sex=integer(n_mort), 
                           year=integer(n_mort), 
                           age=integer(n_mort),
                           ndeaths=numeric(n_mort),
                           area=character(n_mort),
                           stringsAsFactors=FALSE)
i=1
for(year in unique(popmort$Year)){
  for(age in unique(popmort$Age)){  
    temp <- popmort %>%
      filter(Year==year & Age==age)
    popmort_data[i,] <- list(type_mort, 1, as.numeric(year),
                             as.numeric(age), temp$Male, area)
    i <- i+1
    popmort_data[i,] <- list(type_mort, 2, as.numeric(year),
                             as.numeric(age), temp$Female, area)
    i <- i+1
  }
}

rm(temp)

popmort_data <- popmort_data[order(popmort_data$year, popmort_data$sex), ]

## Save the general population all-cause mortality 
write.table(popmort_data,"../Data/PIAMOD_01_analysisdata_acmort.txt",
            sep = "\t", col.names = FALSE,row.names = FALSE,
            quote = FALSE)


######################################################
####### Prepare population size file of Sweden #######
######################################################
# Data downloaded from https://www.mortality.org/Country/Country?cntr=SWE
# Year range: 1751-2024
# Name: Population size 1 x 1

pop <- read.csv("../Data/pop_raw.txt", sep="")
pop <- pop %>%
       filter(Year>=1973 & !Age%in%c(as.character(100:110),"110+")) # Keep those who under 100
n_pop <- dim(pop)[1]*2 # different rows for males and females
popsize <- data.frame(type=character(n_pop),
                       sex=integer(n_pop), 
                       year=integer(n_pop), 
                       age=integer(n_pop),
                       npop=numeric(n_pop),
                       area=character(n_pop),
                       stringsAsFactors=FALSE)
i=1
for(year in unique(pop$Year)){
  for(age in unique(pop$Age)){  
    temp <- pop %>%
      filter(Year==year & Age==age)
    popsize[i,] <- list(type_pop, 1, as.numeric(year),
                         as.numeric(age), temp$Male, area)
    i <- i+1
    popsize[i,] <- list(type_pop, 2, as.numeric(year),
                         as.numeric(age), temp$Female, area)
    i <- i+1
  }
}

popsize <- popsize[order(popsize$year, popsize$sex), ]

## Save data
write.table(popsize,"../Data/PIAMOD_01_analysisdata_popsize.txt",
            sep = "\t", col.names = FALSE,row.names = FALSE,
            quote = FALSE)

