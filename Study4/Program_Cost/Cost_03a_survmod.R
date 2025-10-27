## Updated: 2024-12-04 Enoch Chen: 1) adopt the survival models from Study 3 CML QALY
##                                 I decided to be coherent with Study 3, 
##                                 so still modelling with linear age
##                                 And we decide the distributions of patients by the median age by age group
## 
## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

## Set decimals 
options("scipen"=100, "digits"=4)

## Read packages
library(readstata13)
library(rstpm2)
library(dplyr)
library(flexsurv)
library(biostat3)
library(ggplot2)

###################
##===============##
## Check popmort ##
##===============## 
###################
popmort <- read.dta13("../Data/Cost_00_popmort2022_proj.dta", nonint.factors = TRUE)
colnames(popmort)
colnames(popmort) <- c("year", "age", "sex", "rate", "prob")

## Sex variable
popmort <- popmort %>%
           mutate(sex = ifelse(sex=="male", 1, 0))
  
## Plot 60-year old male's popmort across years
plot_data <- popmort %>%
  filter(age == 60, sex == 1)
ggplot(plot_data, aes(x = year, y = rate)) + 
  geom_line() 


## Plot 2050 male popmort across ages
plot_data <- popmort %>%
  filter(year == 2050, sex == 1)
ggplot(plot_data, aes(x = age, y = rate)) + 
  geom_line() 

############################
##========================##
## Plot to check the data ##
##========================##
############################
## Read data
cml <- readRDS(file = "/Users/yitche/Library/CloudStorage/OneDrive-KarolinskaInstitutet/ec_phd/Study3_QALY/Data/01c_studypop_msset.rds")

## Manage data
cml1 <- cml %>%
        mutate(dxage = as.numeric(dxage),
               # Age group 18-44, 45-54, 55-64, 65-74, 75-99
               dxage_grp = cut(dxage, breaks = c(17, 44, 54, 64, 74, 100), 
                               labels = c("1", "2", "3", "4", "5")),
               start = as.Date(start, origin = "1960-01-01"), 
               stop  = as.Date(stop, origin = "1960-01-01"),
               sex   = ifelse(sex==1, 1, 0))

cml2 <- cml1 %>%
        mutate(t0 = 0,
               t  = (as.numeric(stop)-as.numeric(start))/365.24)

## A subset of patients diagnosed < 2013
## No TKI discontinuation available for patients diagnosed after 2013
cml2_dx2012 <- cml2[cml2$dxyear<=2012,]

## Calculate the median age for each age group
cml2 %>%
  group_by(dxage_grp) %>%
  summarise(median_age = median(dxage))

## A plot data for male aged 45
plotdata = expand.grid(dxage = 45, sex = 1, t=seq(0, 50, length.out = 600))

################################################################################
## Model fitting 
## trans = 1; 1L-TKI -> 2L-TKI
with(cml2, table(trans==1, status==1))
maxt <- max(subset(cml2, trans==1)$t)
maxt

m1a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans == 1), df = 2)
m1a_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans == 1), df = 3)
m1a_df4 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans == 1), df = 4)
## Test AIC
models <- list(m1a_df2, m1a_df3, m1a_df4)
aic_values <- numeric(length(models))
for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m1a_df4

## Estimate rate
m1rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==1))
m1rate

## Plot
plot(m1a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)
lines(m1a_df3, newdata=plotdata, type="haz", col="black")
lines(m1a_df4, newdata=plotdata, type="haz", col="red")
abline(h=m1rate$rate, col="blue", lty=2)

## Decision:
m1 <- m1a_df4

################################################################################
## trans = 2; 1L-TKI -> Prog
with(cml2, table(trans==2, status==1))
maxt <- max(subset(cml2, trans==2)$t)
maxt

m2a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==2), df = 1)
m2a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==2), df = 2)
m2a_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==2), df = 3)

## Test AIC
models <- list(m2a_df1, m2a_df2, m2a_df3)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m2a_df2

## Estimate rate
m2rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==2))
m2rate

plot(m2a_df3, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)
lines(m2a_df2, newdata=plotdata, type="haz", col="red")
abline(h=m2rate$rate, col="blue", lty=1)
abline(h=m2rate$lower, col="blue", lty=2)
abline(h=m2rate$upper, col="blue", lty=2)

## Decision:
m2 <- m2a_df2
################################################################################
## trans = 3; 1L-TKI -> SCT
## Sparse events
with(cml2, table(trans==3, status==1))
maxt <- max(subset(cml2, trans==3)$t)
maxt

m3a_df1 <- stpm2(Surv(t0, t, status == 1) ~ 1, 
                 data = cml2, subset = trans == 3, df = 1)
m3a_df2 <- stpm2(Surv(t0, t, status == 1) ~ 1, 
                 data = cml2, subset = trans == 3, df = 2)

models <- list(m3a_df1, m3a_df2)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m3a_df2

## Estimate rate
m3rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==3))
m3rate

plot(m3a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ylim=c(0, 0.1))
abline(h=m3rate$rate, col="blue", lty=1)
abline(h=m3rate$lower, col="blue", lty=2)
abline(h=m3rate$upper, col="blue", lty=2)

## Decision:
m3rate
################################################################################
## trans = 4; 1L-TKI -> TFR1
## For this transition, I used the subset data to estimate the rate
with(cml2_dx2012, table(trans==4, status==1))
maxt <- max(subset(cml2_dx2012, trans==4)$t)
maxt

m4a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans==4), df = 1)
m4a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans==4), df = 2)
m4a_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans==4), df = 3)
m4a_df4 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans==4), df = 4)

## Test AIC
models <- list(m4a_df1, m4a_df2, m4a_df3, m4a_df4)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m4a_df3

# Estimate rate
m4rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==4))
m4rate

## Plot
plot(m4a_df3, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)
lines(m4a_df4, newdata=plotdata, type="haz", col="black")
abline(h=m4rate$rate, col="blue", lty=1)
abline(h=m4rate$lower, col="blue", lty=2)
abline(h=m4rate$upper, col="blue", lty=2)

## Decision:
m4 <- m4a_df3
################################################################################
## trans = 5; 1L-TKI -> Death
with(cml2, table(trans==5, status==1))
maxt <- max(subset(cml2, trans==5)$t)
maxt

## Check the distribution of time of those who had an event at transition 5 
table(subset(cml2, trans==5 & status==1)$t)

# Bootstrapping m5 would make super wide CIs ?
# This would lead to low estimate of LE
# Solution: It is weird that + bhazard(rate) would inccur wide CIs

m5a_rel_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, bhazard(rate),
                     data = subset(cml2, trans == 5), df = 1)
summary(m5a_rel_df1)

m5a_ac_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex,
                    data = subset(cml2, trans == 5), df = 1)
summary(m5a_ac_df1)

m5a_rel_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, bhazard(rate),
                     data = subset(cml2, trans == 5), df = 2)
summary(m5a_rel_df2)

m5a_ac_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex,
                    data = subset(cml2, trans == 5), df = 2)
summary(m5a_ac_df2)

m5a_rel_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, bhazard(rate),
                     data = subset(cml2, trans == 5), df = 3)
summary(m5a_rel_df3)

## Test AIC
models <- list(m5a_rel_df1, m5a_rel_df2, m5a_rel_df3)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
   aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

plot(m5a_rel_df1, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ylim=c(0,0.5), ci=TRUE)
lines(m5a_ac_df1,  newdata=plotdata, type="haz", col="red")

## Decision:
## Convergence issue for the relative survival model
## Expected hazard only
################################################################################
## trans = 6; 1L TKI_after TFR to 2L TKI_after TFR 
with(cml2_dx2012, table(trans==6, status==1))
maxt <- max(subset(cml2_dx2012, trans==6)$t)
maxt

# Estimate rate
m6rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2_dx2012, trans==6))
m6rate

## Decision:
# m6 is with zero event
################################################################################
## trans = 7; 1L TKI_after TFR to Death
with(cml2_dx2012, table(trans==7, status==1))
maxt <- max(subset(cml2, trans==7)$t)
maxt

# Estimate rate
m7rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==7))
m7rate

## Expected hazard only
## The death rate is too low
################################################################################
## trans = 8; 2L-TKI -> 3L-TKI
with(cml2, table(trans==8, status==1))
maxt <- max(subset(cml2, trans==8)$t)
maxt

m8a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==8), df = 1)
m8a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==8), df = 2)
m8a_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==8), df = 3)
m8a_df4 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==8), df = 4)

## Test AIC
models <- list(m8a_df1, m8a_df2, m8a_df3, m8a_df4)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m8a_df2

# Estimate rate
m8rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==8))
m8rate

## Plot
plot(m8a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)
abline(h=m8rate$rate, col="blue", lty=1)
abline(h=m8rate$lower, col="blue", lty=2)
abline(h=m8rate$upper, col="blue", lty=2)

## Decision:
m8 <- m8a_df2
################################################################################
## trans = 9; 2L-TKI -> Prog
with(cml2, table(trans==9, status==1))
maxt <- max(subset(cml2, trans==9)$t)
maxt

m9a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==9), df = 1)
m9a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==9), df = 2)
m9a_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2, trans==9), df = 3)

## Test AIC
models <- list(m9a_df1, m9a_df2, m9a_df3)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m9a_df2

## Estimate rate
m9rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                   data = subset(cml2, trans==9))
m9rate

plot(m9a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)
abline(h=m9rate$rate, col="blue", lty=1)
abline(h=m9rate$lower, col="blue", lty=2)
abline(h=m9rate$upper, col="blue", lty=2)

## Decision:
m9 <- m9a_df2
################################################################################
## trans = 10; 2L-TKI -> SCT
## Sparse
with(cml2, table(trans==10, status==1))

m10a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 10, df = 1)
m10a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 10, df = 2)

models <- list(m10a_df1, m10a_df2)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m10a_df2

m10rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = cml2, subset = trans == 10)

plot(m10a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ylim=c(0, 0.5))
abline(h=m10rate$rate, col="blue", lty=1)
abline(h=m10rate$lower, col="blue", lty=2)
abline(h=m10rate$upper, col="blue", lty=2)

## Decision:
m10 <- m10a_df2
################################################################################
## trans = 11; 2L-TKI -> TFR2
with(cml2_dx2012, table(trans==11, status==1))
maxt <- max(subset(cml2_dx2012, trans==11)$t)
maxt

m11a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = subset(cml2_dx2012, trans == 11), df = 1)
m11a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2_dx2012, subset = trans == 11, df = 2)
m11a_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2_dx2012, subset = trans == 11, df = 3)
m11a_df4 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2_dx2012, subset = trans == 11, df = 4)

## Test AIC
models <- list(m11a_df1, m11a_df2, m11a_df3, m11a_df4)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m11a_df1

# Estimate rate
m11rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = subset(cml2, trans==11))
m11rate

plot(m11a_df1, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)
abline(h=m11rate$rate, col="blue", lty=1)
abline(h=m11rate$lower, col="blue", lty=2)
abline(h=m11rate$upper, col="blue", lty=2)

## Decision: 
m11 <- m11a_df1
################################################################################
## trans = 12; 2L-TKI -> Death
with(cml2, table(trans==12, status==1))
maxt <- max(subset(cml2, trans==12)$t)
maxt
 
## Check the distribution of time of those who had an event at transition 10
table(subset(cml2, trans==12 & status==1)$t, subset(cml2, trans==12 & status==1)$idnr)

## Relative survival model
m12a_rel_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 12), df = 1)
m12a_rel_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 12), df = 2)
m12a_rel_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 12), df = 3)
m12a_rel_df4 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 12), df = 4)

models <- list(m12a_rel_df1, m12a_rel_df2, m12a_rel_df3, m12a_rel_df4)
aic_values <- numeric(length(models))

## Test AIC
for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}
 
print(aic_values)
 
plot(m12a_rel_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ci=TRUE)

## Decision:
## Expected hazard only
################################################################################
## trans = 13; 2L TKI_after TFR to 3L TKI_after TFR 
with(cml2, table(trans==13, status==1))
maxt <- max(subset(cml2, trans==13)$t)

maxt

# Estimate rate
m13rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = subset(cml2, trans==13))
m13rate

## Decision:
m13rate
################################################################################
## trans = 14; 2L TKI_after TFR to Death
with(cml2, table(trans==14, status==1))
maxt <- max(subset(cml2, trans==14)$t)
maxt

# Estimate rate
m14rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = subset(cml2, trans==14))
m14rate

## Decision:
## Expected hazard only
## The death rate is too low
################################################################################
## trans = 15; 3L-TKI -> Prog
## Sparse
with(cml2, table(trans==15, status==1))
m15rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = cml2, subset = trans == 15)
m15rate


## Decision:
m15rate
################################################################################
## trans = 16; 3L-TKI -> SCT
## Sparse
with(cml2, table(trans==16, status==1))

m16a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 16, df = 1)
m16a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 16, df = 2)

models <- list(m16a_df1, m16a_df2)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m16a_df2

m16rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = cml2, subset = trans == 16)
m16rate

plot(m16a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ylim=c(0, 0.5))
abline(h=m16rate$rate, col="blue", lty=1)
abline(h=m16rate$lower, col="blue", lty=2)
abline(h=m16rate$upper, col="blue", lty=2)

## Decision:
m16 <- m16a_df2
################################################################################
## trans = 17; 3L-TKI -> TFR3
with(cml2_dx2012, table(trans==17, status==1))
maxt <- max(subset(cml2_dx2012, trans==17)$t)
maxt

m17_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans == 17), df = 1)
m17_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans == 17), df = 2)
m17_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = subset(cml2_dx2012, trans == 17), df = 3)
## Test AIC
models <- list(m17_df1, m17_df2, m17_df3)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m17_df1

# Estimate rate
m17rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = subset(cml2, trans==17))
m17rate

plot(m17_df1, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50))
abline(h=m17rate$rate, col="blue", lty=1)
abline(h=m17rate$lower, col="blue", lty=2)
abline(h=m17rate$upper, col="blue", lty=2)

# Decision:
m17rate
################################################################################
## trans = 18; 3L-TKI -> Death
with(cml2, table(trans==18, status==1))
maxt <- max(subset(cml2, trans==18)$t)
maxt

m18a_rel_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 18), df = 1)
m18a_rel_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 18), df = 2)
m18a_rel_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex + bhazard(rate), 
                      data = subset(cml2, trans == 18), df = 3)

## Test AIC
models <- list(m18a_rel_df1, m18a_rel_df2, m18a_rel_df3)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m18a_rel_df1

## Plot
plot(m18a_rel_df1, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50))

## Decision:
## Only expected hazard
################################################################################
## trans = 19;  3L TKI_after TFR to Death
with(cml2, table(trans==19, status==1))
maxt <- max(subset(cml2, trans==19)$t)
maxt

## Expected hazard only
## The death rate is too low
################################################################################
## trans = 20; Prog -> SCT
## Sparse
with(cml2, table(trans==20, status==1))

m20a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 20, df = 1)
m20a_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 20, df = 2)

models <- list(m20a_df1, m20a_df2)
aic_values <- numeric(length(models))

for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m20a_df2

# Estimate rate
m20rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = subset(cml2, trans==20))
m20rate

plot(m20a_df2, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ylim=c(0, 0.5))
abline(h=m20rate$rate, col="blue", lty=1)
abline(h=m20rate$lower, col="blue", lty=2)
abline(h=m20rate$upper, col="blue", lty=2)

## Decision:
m20 <- m20a_df2
################################################################################
## trans = 21; Prog -> Death
## Sparse
with(cml2, table(trans==21, status==1))
maxt <- max(subset(cml2, trans==21)$t)
maxt

m21_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex , 
                 data = cml2, subset = trans == 21, df = 1)

m21_df2 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = cml2, subset = trans == 21, df = 2)

m21_df3 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                 data = cml2, subset = trans == 21, df = 3)

## Test AIC
models <- list(m21_df1, m21_df2, m21_df3)

aic_values <- numeric(length(models))
for (i in 1:length(models)) {
  aic_values[i] <- AIC(models[[i]])
}

print(aic_values)

## Model with lowest AIC
m21_df1

# Estimate rate
m21rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = subset(cml2, trans==21))
m21rate

plot(m21_df1, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50), ylim=c(0, 3))
abline(h=m21rate$rate, col="blue", lty=1)
abline(h=m21rate$lower, col="blue", lty=2)
abline(h=m21rate$upper, col="blue", lty=2)

## Decision:
## Selected model
m21 <- m21_df1
################################################################################
## trans = 22; SCT -> Prog
## Sparse events; convergence issue
with(cml2, table(trans==22, status==1))
m22rate <- survRate(Surv(t0, t, status == 1) ~ 1, 
                    data = cml2, subset = trans == 22)
m22rate$rate

## Decision:
m22rate
################################################################################
## trans = 23; SCT -> Death
with(cml2, table(trans==23, status==1))

## No event from SCT to Death in this dataset
## Only consider expected death
################################################################################
## trans = 24; TFR1 -> 1L TKI_after TFR
with(cml2, table(trans==24, status==1))
maxt <- max(subset(cml2, trans==24)$t)
maxt

m24rate     <- survRate(Surv(t0, t, status == 1) ~ 1, 
                        data = cml2, subset = trans == 24)
m24rate$rate

m24a_df1 <- stpm2(Surv(t0, t, status == 1) ~ dxage + sex, 
                  data = cml2, subset = trans == 24, df = 1)

plot(m24a_df1, newdata=plotdata, type="haz",
     xlab="Time since entering the state (years)", ylab="Hazard",
     xlim=c(0, 50))
abline(h=m24rate$rate, col="blue", lty=1)
abline(h=m24rate$lower, col="blue", lty=2)
abline(h=m24rate$upper, col="blue", lty=2)

## Decision:
## Selected model based on expert knowledge 
m24rate
################################################################################
## trans = 25; TFR1 -> 2L TKI_after TFR
with(cml2, table(trans==25, status==1))
maxt <- max(subset(cml2, trans==25)$t)
maxt

m25rate     <- survRate(Surv(t0, t, status == 1) ~ 1, 
                        data = cml2, subset = trans == 25)
## Decision:
m25rate
################################################################################
## trans = 26; TFR1 -> Death
## Die from expected only## It looks like m16star would incur very high
## The shape indicates that there would be a spike after entering TFR
## This is not true based on clinical opinion (Those who enter TFR are doing better than others)
with(cml2, table(trans==26, status==1))
maxt <- max(subset(cml2, trans==26)$t)
maxt

# m16a_rel_df1 <- stpm2(Surv(t0, t, status == 1) ~ 1 + bhazard(rate), 
#                        data = subset(cml2, trans == 16), df = 1)
# m16a_rel_df2 <- stpm2(Surv(t0, t, status == 1) ~ 1 + bhazard(rate), 
#                        data = subset(cml2, trans == 16), df = 2)
# m16a_rel_df3 <- stpm2(Surv(t0, t, status == 1) ~ 1 + bhazard(rate), 
#                        data = subset(cml2, trans == 16), df = 3)
# 
# ## Test AIC
# models <- list(m16a_rel_df1, m16a_rel_df2, m16a_rel_df3)
# aic_values <- numeric(length(models))
# 
# for (i in 1:length(models)) {
#   aic_values[i] <- AIC(models[[i]])
# }
# 
# print(aic_values)
# 
# plot(m16a_rel_df2, newdata=plotdata, type="haz",
#      xlab="Time since entering the state (years)", ylab="Hazard",
#      xlim=c(0, 50), ci=TRUE)
# lines(m16a_rel_df1, newdata=plotdata, type="haz", col="red")
# lines(m16a_rel_df3, newdata=plotdata, type="haz", col="black")

## Only expected hazard 
################################################################################
## trans = 27; TFR2 -> 2L TKI_after TFR
with(cml2, table(trans==27, status==1))
maxt <- max(subset(cml2, trans==27)$t)
maxt

m27rate     <- survRate(Surv(t0, t, status == 1) ~ 1, 
                        data = cml2, subset = trans == 27)
m27rate
################################################################################
## trans = 28; TFR2 -> 3L TKI_after TFR
with(cml2, table(trans==28, status==1))
maxt <- max(subset(cml2, trans==28)$t)
maxt

m28rate     <- survRate(Surv(t0, t, status == 1) ~ 1, 
                        data = cml2, subset = trans == 28)
m28rate
################################################################################
## trans = 29; TFR2 -> Death
with(cml2, table(trans==29, status==1))
maxt <- max(subset(cml2, trans==29)$t)
maxt

## Only 2 events
## Die from expected only
################################################################################
## trans = 30; TFR3 -> 3L TKI_after TFR
with(cml2, table(trans==30, status==1))
maxt <- max(subset(cml2, trans==30)$t)
maxt

m30rate     <- survRate(Surv(t0, t, status == 1) ~ 1, 
                        data = cml2, subset = trans == 30)
m30rate
################################################################################
## trans = 31; TFR3 -> Death
with(cml2, table(trans==31, status==1))
maxt <- max(subset(cml2, trans==31)$t)
maxt

## Only one event
## Die from expected only
################################################################################

