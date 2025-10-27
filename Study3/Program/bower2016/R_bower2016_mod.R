#' Updated: 2023-04-21 Enoch Chen

#' Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

#' For Mark Clements only
if (Sys.getenv("USER")=="marcle") {
  if (Sys.getenv("HOSTNAME") == "vector.meb.ki.se")
    setwd("/cifs/P/SweCan_CML/Enoch_PhD/Study2_QALY/Program")
  else setwd("/media/P/SweCan_CML/Enoch_PhD/Study2_QALY/Program")
}

#' setwd for Data
wd_data = "../Data"

#'
library(dplyr)
hannah = read.dta13("../Data/analysisdata_cml.dta") |>     subset(yeardiag<=2013)
hannah = hannah |>
  mutate(dead = (stime<=15 & dead), # maximum 15 years
         stime = pmin(stime,15),
         female = (sex==2)+0,
         `_year` = floor(pmin(yeardiag+stime, 2013)),
         `_age` = floor(pmin(agediag + stime, 99)),
         age70=agediag-70,
         year1990=yeardiag-1990)
bhazard = read.dta13("../../../../SweCan/Popmort/popmort2014.dta")
hannah2 = inner_join(hannah, bhazard)

library(rstpm2)
fit = stpm2(Surv(stime,dead)~nsx(year1990,df=4,centre=0)+nsx(age70,df=4,centre=0)+female+
              bhazard(rate),
            data=hannah2,
            smooth.formula=~year1990:age70+female:year1990+female:age70+nsx(log(stime),df=5),
            tvc=list(year1990=2,age70=2,female=2))
summary(fit)

## drop yeardiag terms and restrict to 2007-2012
fitx = stpm2(Surv(stime,dead)~nsx(age70,df=4)+female+bhazard(rate),
             data=subset(hannah2,yeardiag>=2007 & yeardiag<=2012),
             smooth.formula=~female:age70+nsx(log(stime),df=5),
             tvc=list(age70=2,female=2))
summary(fitx)
dim(subset(hannah2,yeardiag>=2007 & yeardiag<=2012))

set.seed(1)
simulate(fit, newdata=data.frame(year1990=2010-1990, age70=40-70, female=0))