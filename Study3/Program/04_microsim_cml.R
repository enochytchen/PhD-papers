## Updated: 2024-08-14 Enoch Chen: update codes
##          2024-05-31 Enoch Chen: update syntaxes
##                                 For all transitions not leading to death, take an average rate
##                                 For all transitions leading to death, FPMs
##                                 Clearly assess whether FPRSMs fit or not
##          2024-04-19 Enoch Chen: fix clockret code
##                     It was:     scheduleAt(m6.randU(R::runif(0.0,1.0), now()), toTki3);
##                     Corrected:  scheduleAt(m6.randU(R::runif(0.0,1.0)) + now(), toTki3);
##          2024-02-21 Enoch Chen: update utility function
##          2024-01-29 Enoch Chen: use popmort2022 and projected rates instead of popmort2018
##          2023-12-13 Enoch Chen: remove m14's excess hazard model
##          2023-11-28 Enoch Chen: remove m5's excess hazard model
##          2023-11-20 Enoch Chen: Redefine the studypop dx 2007-2017 and hence multistate structure changed
##          2023-11-14 Enoch Chen: fix the error incurred by toRatio
##          2023-11-06 Enoch Chen: allow patients to flow from TFR2 to 3L-TKI
##          2023-09-30 Enoch Chen: refitt models after reducing the total states (8->6)
##          2023-09-12 Enoch Chen: refitt models after discussion
##          2023-08-30 Enoch Chen: Fix bootstrapping
## Purpose: Incorporate flexible relative survival models into the microsimulation models
##          Reproduce the model originally built in R (microsim_rel_app3.R)
## Caution: Must have installed the lastest Rtools and R4.2.2+


## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
    wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(wd)
}

## For Mark Clements only
if (Sys.getenv("USER")=="marcle") {
    if (Sys.getenv("HOSTNAME") == "vector.meb.ki.se")
        setwd("/cifs/P/SweCan_CML/Enoch_PhD/Study3_QALY/Program")
    else setwd("/media/P/SweCan_CML/Enoch_PhD/Study3_QALY/Program")
}

## setwd for Data
wd_data = "../Data"

#######################
##===================##
##   Read packages   ## 
##===================##
#######################
library(readstata13)
library(mstate)
library(rstpm2)
library(dplyr)
library(msm)
library(tidyverse)
library(microsimulation)
library(Rcpp) 

#################################
##=============================##
## Run all the survival models ##
##=============================##
#################################
source("03_survmod.R") 

##################################################
##==============================================##
##  Read Swedish popmort 2022 with projection   ##
##==============================================##
##################################################
popmort <- read.dta13("../Data/00_popmort2022_proj.dta", nonint.factors = TRUE)
names(popmort) = c("year", "age", "sex", "rate", "prob")

popmort2 = transform(popmort, cohort=year-age, sex=ifelse(sex==1, 1, 0))
popmort2 = arrange(popmort2, cohort, sex, age)
table(popmort2$sex)

###################################
##===============================##
## microsimulation model for CML ##
##===============================##
###################################
sourceCpp(code="
  // BH is required for older versions of microsimulation
  // RcppArmadillo is required for newer versions of microsimulation
  // For now, we depend on both:)
  // [[Rcpp::depends(BH)]]
  // [[Rcpp::depends(RcppArmadillo)]]
  // [[Rcpp::depends(microsimulation)]]
  #include <RcppArmadillo.h>
  #include <microsimulation.h>
  // #include <gsm.h>
  using namespace arma;
  using namespace ssim;
  enum state_t {Tki1, Tki1afterTfr, Tki2, Tki2afterTfr, Tki3, Tki3afterTfr, Tfr1, Tfr2, Tfr3, Prog, Sct, AcD, ExcD, ExpD};
  enum event_t {toTki1, toTki1afterTfr, toTki2, toTki2afterTfr, toTki3, toTki3afterTfr, toTfr1, toTfr2, toTfr3, toProg, toSct, toAcD, toExcD, toExpD, toEOS, toRatio};
  typedef ssim::SummaryReport<short,short> Report;
  /**
      Utility: Random exponential using rate parameterisation
  */
  double rexpRate(double rate) { return R::rexp(1.0/rate); }
  // template<class T> double rexpRate(T rate) { return R::rexp(1.0/as<double>(rate)); }
  /**
      Utility: Run a set of simulations for a single process
  */
  void runSimulations(ssim::cProcess* process, int n) {
    for (int i = 0; i < n; i++) {
      ssim::Sim::create_process(process);
      ssim::Sim::run_simulation();
      ssim::Sim::clear();
    }
  }
  /**
      Define a class for the process
  */
  class CML : public ssim::cProcess
  {
  public:
    int id;
    state_t state;
    Rcpp::List param;
    Report *report;
    ssim::Rpexp background;
    ssim::gsm m1; 
    ssim::gsm m2;
    // m3 is a rate
    ssim::gsm m4;
    // m5 only allows die from the expected haz
    // m6 is a rate
    // m7 is a rate
    ssim::gsm m8;
    ssim::gsm m9;
    ssim::gsm m10;
    ssim::gsm m11;
    // m12 only allows die from the expected haz
    // m13 is a rate
    // m14 only allows die from the expected haz
    // m15 is a rate
    ssim::gsm m16;
    // m17 is a rate
    // m18 only allows die from the expected haz
    // m19 only allows die from the expected haz
    ssim::gsm m20;
    ssim::gsm m21;
    // m22 is a rate
    // m23 only allows die from the expected haz
    // m24 is a rate
    // m25 is a rate
    // m26 only allows die from the expected haz
    // m27 is a rate
    // m28 is a rate
    // m29 only allows die from the expected haz
    // m30 is a rate
    // m31 only allows die from the expected haz
    double dxage, hsu_base, hsu_ratio, hsu_tki1, hsu_tki2, hsu_tki3, hsu_prog, hsu_sct, hsu_tfr;
    bool ever_tfr1, ever_tfr2, ever_tfr3, ever_prog, ever_sct;
    int sex;
    CML(Rcpp::List param, Report *report) : id(-1), param(param), report(report), background() {
      // get the rates from param
      vec mu0 = as<vec>(param(\"mu0\"));
      vec ages = as<vec>(param(\"ages\"));
      background = ssim::Rpexp(mu0.memptr(), ages.memptr(), ages.size());
      // read in from param...
      m1 = ssim::gsm(as<List>(param(\"m1\")));
      m2 = ssim::gsm(as<List>(param(\"m2\")));
      m4 = ssim::gsm(as<List>(param(\"m4\")));
      m8 = ssim::gsm(as<List>(param(\"m8\")));
      m9 = ssim::gsm(as<List>(param(\"m9\")));
      m10 = ssim::gsm(as<List>(param(\"m10\")));
      m11 = ssim::gsm(as<List>(param(\"m11\")));
      m16 = ssim::gsm(as<List>(param(\"m16\")));
      m20 = ssim::gsm(as<List>(param(\"m20\")));
      m21 = ssim::gsm(as<List>(param(\"m21\")));
      dxage = param(\"dxage\");
      sex = param(\"sex\");
      hsu_tki1 = param(\"hsu_tki1\");
      hsu_tki2 = param(\"hsu_tki2\");
      hsu_tki3 = param(\"hsu_tki3\");
      hsu_prog = param(\"hsu_prog\");
      hsu_sct = param(\"hsu_sct\");
      hsu_tfr = param(\"hsu_tfr\");
      ever_tfr1 = ever_tfr2 = ever_tfr3 = ever_prog = ever_sct = false;
    }
    void tki1();
    void set_ratio(); // to be specified
    void init(); // to be specified
    void handleMessage(const ssim::cMessage* msg); // to be specified
    void cancelEvents(); // utility function
  };
  void CML::set_ratio() {
    double age = dxage+now();
    if (sex==1) {
      hsu_ratio = age>=95 ? 0.751 :
                  age>=90 ? 0.803 :
                  age>=85 ? 0.831 :
                  age>=80 ? 0.865 :
                  age>=75 ? 0.892 :
                  age>=70 ? 0.909 :
                  age>=65 ? 0.915 :
                  age>=60 ? 0.910 :
                  age>=55 ? 0.910 :
                  age>=50 ? 0.914 :
                  age>=45 ? 0.924 :
                  0.930;
      hsu_ratio /= 0.914; // Because the studypop of Foulon 2021 was 52.0
    } else {
      hsu_ratio = age>=95 ? 0.696 :
                  age>=90 ? 0.761 :
                  age>=85 ? 0.814 :
                  age>=80 ? 0.852 :
                  age>=75 ? 0.883 :
                  age>=70 ? 0.899 :
                  age>=65 ? 0.909 :
                  age>=60 ? 0.899 :
                  age>=55 ? 0.894 :
                  age>=50 ? 0.895 :
                  age>=45 ? 0.904 :
                  0.906;
       hsu_ratio /= 0.895; // Because the studypop of Foulon 2021 was 52.0
    }
  }
  void CML::tki1() {
    state = Tki1;
    hsu_base = hsu_tki1;
    scheduleAt(m1.randU(R::runif(0.0,1.0),now()), toTki2);
    scheduleAt(m2.randU(R::runif(0.0,1.0),now()), toProg);
    if (dxage+now() < 73)  // In the studypop, SCT only operated on pts < 73 y
    scheduleAt(ssim::now() + rexpRate(param(\"m3rate\")), toSct);
    scheduleAt(m4.randU(R::runif(0.0,1.0),now()), toTfr1);
    scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
  }
  /**
      Initialise a simulation run for an individual
  */
  void CML::init() {
    id++;
    tki1();
    ever_tfr1 = ever_tfr2 = ever_tfr3 = ever_prog = ever_sct = false;
  }
  /**
      Handle receiving self-messages
  */
  void CML::handleMessage(const ssim::cMessage* msg) {
    if (param(\"debug\")) Rprintf(\"id: %i, state: %i, kind: %i, previous: %f, now: %f\\n\", id, state, msg->kind, this->previousEventTime, ssim::now());
    set_ratio();
    report->setUtility(hsu_base * hsu_ratio); 
    report->add(state, msg->kind, this->previousEventTime, ssim::now(), id);
    // Check for change in age group
    if (msg->kind == toRatio  && now() < 60.0) {
      scheduleAt(floor((dxage+now())/5.0)*5.0 - dxage + 5.0, toRatio);
    } else {      
      cancel_events();
      scheduleAt(60, toEOS); // Max time horizon: 60 years 
      scheduleAt(floor((dxage+now())/5.0)*5.0 - dxage + 5.0, toRatio);
      switch(msg->kind) {
      case toTki1:
        tki1();
        break;
      case toTki1afterTfr:
        state = Tki1afterTfr;
        hsu_base = hsu_tki1;
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
      case toTki2:
        state = Tki2;
        hsu_base = hsu_tki2;
        scheduleAt(m8.randU(R::runif(0.0,1.0)) + now(), toTki3);
        scheduleAt(m9.randU(R::runif(0.0,1.0)) + now(), toProg);
        if (dxage+now() < 73)  // In the studypop, SCT only operated on pts < 73 y
        scheduleAt(m10.randU(R::runif(0.0,1.0)) + now(), toSct);
        if (!ever_tfr1) 
        scheduleAt(m11.randU(R::runif(0.0,1.0)) + now(), toTfr2);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        break;
      case toTki2afterTfr:
        state = Tki2afterTfr;
        hsu_base = hsu_tki2;
        scheduleAt(ssim::now() + rexpRate(param(\"m13rate\")), toTki3afterTfr);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
      case toTki3:
        state = Tki3;
        hsu_base = hsu_tki3;
        scheduleAt(ssim::now() + rexpRate(param(\"m15rate\")), toProg);
        scheduleAt(m16.randU(R::runif(0.0,1.0)) + now(), toSct);
        if (!ever_tfr1) 
        if (!ever_tfr2) 
        scheduleAt(ssim::now() + rexpRate(param(\"m17rate\")), toTfr3);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        break;
      case toTki3afterTfr:
        state = Tki3afterTfr;
        hsu_base = hsu_tki3;
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
      case toProg:
        state = Prog;
        hsu_base = hsu_prog;
        if (!ever_sct)
        if (dxage+now() < 73)
        scheduleAt(m20.randU(R::runif(0.0,1.0)) + now(), toSct);
        scheduleAt(m21.randU(R::runif(0.0,1.0)) + now(), toExcD);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        ever_prog = true; 
        break;
      case toSct:
        state = Sct;
        hsu_base = hsu_sct;
        if (!ever_prog) 
        scheduleAt(ssim::now() + rexpRate(param(\"m22rate\")), toProg);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        ever_sct = true; 
        break;
      case toTfr1:
        state = Tfr1;
        hsu_base = hsu_tfr;
        scheduleAt(ssim::now() + rexpRate(param(\"m24rate\")), toTki1afterTfr);
        scheduleAt(ssim::now() + rexpRate(param(\"m25rate\")), toTki2afterTfr);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        ever_tfr1 = true;
        break;
      case toTfr2:
        state = Tfr2;
        hsu_base = hsu_tfr;
        scheduleAt(ssim::now() + rexpRate(param(\"m27rate\")), toTki2afterTfr);
        scheduleAt(ssim::now() + rexpRate(param(\"m28rate\")), toTki3afterTfr);        
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        ever_tfr2 = true;
        break;
      case toTfr3:
        state = Tfr3;
        hsu_base = hsu_tfr;
        scheduleAt(ssim::now() + rexpRate(param(\"m30rate\")), toTki3afterTfr);
        scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
        ever_tfr3 = true;
        break;  
      case toAcD:
        state = AcD;
        hsu_base = 0.0;
        break;
      case toExcD:
        state = ExcD;
        hsu_base = 0.0;
        break;
      case toExpD:
        state = ExpD;
        hsu_base = 0.0;
        break;
      case toRatio:
      case toEOS:
        ssim::Sim::stop_simulation();
        break;
      default:
        REprintf(\"Invalid kind of event: %i.\\n\", msg->kind);
        break;
      }
    }
    if (id % 1000 == 0) Rcpp::checkUserInterrupt(); 
  }
  /**
      Exported function: Set up the report and process, run the simulations and return a report
  */
  //[[Rcpp::export]]
  List callSim(int n, List param, bool indivp) {
    Report report(n,indivp);
    report.setPartition(0.0,100.0,param(\"partitionBy\"));
    CML person(param,&report);
    runSimulations(&person, n);
    Rcpp::List lst = report.asList();
    lst.push_back(param,\"param\");
    return lst;
  }")

simulations = function(n, param, simulator=callSim, indivp=TRUE) {
  ## stopifnot(length(param$mu0) == 104)
  object = simulator(n, param, indivp)
  if(!all(c("ut","costs","pt","events","prev") %in% names(object))) 
      stop("simulator failed")
  stateT = c("Tki1", "Tki1afterTfr", "Tki2", "Tki2afterTfr", "Tki3", "Tki3afterTfr", "Tfr1", "Tfr2", "Tfr3", "Prog", "Sct", "AcD", "ExcD", "ExpD")
  eventT = c("toTki1", "toTki1afterTfr", "toTki2", "toTki2afterTfr", "toTki3", "toTki3afterTfr", "toTfr1", "toTfr2", "toTfr3", "toProg", "toSct", "toAcD","toExcD","toExpD", "toEOS", "toRatio")
  for (name in c("ut","costs","pt","events","prev"))
    object[[name]] = transform(object[[name]], state=stateT[Key+1], time=age, Key=NULL, age=NULL)
  object$events = transform(object$events, event=eventT[event+1])
  class(object) = c("CML","SummaryReport")
  object
}

#####################################
##=================================##
## Define get expected hazard rate ##
##=================================##
#####################################
# .cohort = year-age
getRates_cohort = function(.sex, .cohort) {
  subset(popmort2, sex==.sex & cohort==.cohort & age<100)
}

#############################
##=========================##
## Run the microsimulation ##
##=========================##
#############################
ndata <- data.frame(dxage = 45, sex = 0)

## Define parameter
param <- list(mu0 = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$rate,
              ages = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$age,
              partitionBy = 0.1,
              debug = FALSE,
              dxage = ndata$dxage,
              sex = ndata$sex,
              hsu_tki1 = 0.76,
              hsu_tki2 = 0.68,
              hsu_tki3 = 0.68,
              hsu_prog = 0.41,
              hsu_sct = 0.80,
              hsu_tfr = 0.84,
              m1 = gsm_design(m1, newdata = ndata),
              m2 = gsm_design(m2, newdata = ndata),
              m3rate  = m3rate$rate,
              m4 = gsm_design(m4, newdata = ndata),
              ## Transition 5 only expected hazard
              ## Transition 6 no event
              m7rate  = m7rate$rate,
              m8 = gsm_design(m8, newdata = ndata),
              m9 = gsm_design(m9, newdata = ndata),
              m10 = gsm_design(m10, newdata = ndata),
              m11 = gsm_design(m11, newdata = ndata),
              ## Transition 12 only expected hazard
              m13rate  = m13rate$rate,
              ## Transition 14 only expected hazard
              m15rate  = m15rate$rate,
              m16 = gsm_design(m16, newdata = ndata),
              m17rate  = m17rate$rate,
              ## Transition 18 only expected hazard
              ## Transition 19 only expected hazard
              m20 = gsm_design(m20, newdata = ndata),
              m21 = gsm_design(m21, newdata = ndata),
              m22rate  = m22rate$rate,
              ## Transition 23 only expected hazard
              m24rate  = m24rate$rate,
              m25rate  = m25rate$rate,
              ## Transition 26 only expected hazard
              m27rate  = m27rate$rate,
              m28rate  = m28rate$rate,
              ## Transition 29 only expected hazard
              m30rate  = m30rate$rate
              ## Transition 31 only expected hazard
)

sim <- simulations(1e4, param, indivp = FALSE)

table(sim$pt$state)

## Life expectancy
LE <- sum(subset(sim$pt, ! state %in% c("AcD", "ExcD", "ExpD"))$pt) / sim$n
LE

## Quality-adjusted life expectancy
QALE <- sum(sim$ut$utility) / sim$n                         
QALE

## Check SCT
LoS_Sct  <- (sum(subset(sim$pt, state %in% c("Sct"))$pt)) / sim$n
LoS_Sct
################################################################
