## File name: Cost_03b_microsim_cml_apbp.R
## Updated: 
## Purpose: Run the adapted microsimulation model for CML AP BP to determine the long-term survival
##          AP/BP with two competing events: excess death, and expected death

## Set the wd as where this R file is
if (require("rstudioapi") && isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

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
source("Cost_03a_survmod.R") 

##################################################
##==============================================##
##  Read Swedish popmort 2022 with projection   ##
##==============================================##
##################################################
setwd(wd)
popmort <- read.dta13("../Data/Cost_00_popmort2022_proj.dta", nonint.factors = TRUE)
names(popmort) = c("year", "age", "sex", "rate", "prob")

popmort2 = transform(popmort, cohort=year-age, sex=ifelse(sex=="male", 1, 0))
popmort2 = arrange(popmort2, cohort, sex, age)
table(popmort2$sex)

#########################################
##=====================================##
## microsimulation model for CML AP/BP ##
##=====================================##
#########################################
sourceCpp(code="
  // BH is required for older versions of microsimulation
  // RcppArmadillo is required for newer versions of microsimulation
  // [[Rcpp::depends(BH)]]
  // [[Rcpp::depends(RcppArmadillo)]]
  // [[Rcpp::depends(microsimulation)]]
  #include <RcppArmadillo.h>
  #include <microsimulation.h>
  using namespace arma;
  using namespace ssim;
  enum state_t {Prog, ExcD, ExpD};
  enum event_t {toProg, toExcD, toExpD, toEOS};
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
  class IDM : public ssim::cProcess
  {
  public:
    int id;
    state_t state;
    Rcpp::List param;
    Report *report;
    ssim::Rpexp background;
    ssim::gsm m21; // Prog -> ExcD
    double dxage;
    int sex;
    IDM(Rcpp::List param, Report *report) : id(-1), param(param), report(report), background() {
       // get the rates from param
      vec mu0 = as<vec>(param(\"mu0\"));
      vec ages = as<vec>(param(\"ages\"));
      background = ssim::Rpexp(mu0.memptr(), ages.memptr(), ages.size());
      // read in from param...
      m21 = ssim::gsm(as<List>(param(\"m21\")));
      dxage = param(\"dxage\");
      sex = param(\"sex\");
      }
    void prog();
    void init(); // to be specified
    void handleMessage(const ssim::cMessage* msg); 
    void cancelEvents(); 
  };
  void IDM::prog() {
    state = Prog;
    report -> setUtility(1);
    if (dxage+now() < 73)
    scheduleAt(m21.randU(R::runif(0.0,1.0)) + now(), toExcD);
    scheduleAt(background.rand(std::exp(-R::rexp(1.0)), dxage+now())-dxage, toExpD);
  }
  /**
      Initialise a simulation run for an individual
  */
  void IDM::init() {
    id++;
    prog();
  }
  /**
      Handle receiving self-messages
  */
  void IDM::handleMessage(const ssim::cMessage* msg) {
    if (param(\"debug\")) Rprintf(\"id: %i, state: %i, kind: %i, previous: %f, now: %f\\n\",
                       id, state, msg->kind, this->previousEventTime, ssim::now());
    report->add(state, msg->kind, this->previousEventTime, ssim::now(), id);
    cancel_events();
    scheduleAt(60, toEOS); // End of study--Time horizon 50 years
    switch(msg->kind) {
    case toProg:
      prog();
      break;
    case toExcD:
      state = ExcD;
      report -> setUtility(0.0);
      break;
    case toExpD:
      state = ExpD;
      report -> setUtility(0.0);
      break;
    case toEOS:
      ssim::Sim::stop_simulation();
      break;
    default:
      REprintf(\"Invalid kind of event: %i.\\n\", msg->kind);
      break;
    }
    if (id % 100000 == 0) Rcpp::checkUserInterrupt(); 
  }
  /**
      Exported function: Set up the report and process, run the simulations and return a report
  */
  //[[Rcpp::export]]
  List callSim_reduced(int n, List param, bool indivp) {
    Report report(n,indivp);
    report.setPartition(0.0,100.0,param(\"partitionBy\"));
    IDM person(param,&report);
    runSimulations(&person, n);
    Rcpp::List lst = report.asList();
    lst.push_back(param,\"param\");
    return lst;
  }")

simulations_apbp = function(n, param, simulator=callSim_reduced, indivp=TRUE) {
  object = simulator(n, param, indivp)
  if(!all(c("ut","costs","pt","events","prev") %in% names(object)))
    stop("simulator failed")
  stateT = c("Prog", "ExcD", "ExpD")
  eventT = c("toProg", "toExcD", "toExpD", "toEOS")
  for (name in c("ut","costs","pt","events","prev"))
    object[[name]] = transform(object[[name]], state=stateT[Key+1], time=age, Key=NULL, age=NULL)
  object$events = transform(object$events, event=eventT[event+1])
  class(object) = c("IDM","SummaryReport")
  object
}

#############################
##=========================##
## Run the microsimulation ##
##=========================##
#############################
# Function to set up parameters and run the simulation
ndata <-  data.frame(dxage = 61, sex = 1)

param <- list(mu0 = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$rate,
              ages = getRates_cohort(ndata$sex, 2012 - ndata$dxage)$age,
              partitionBy = 0.1,
              debug = FALSE,
              dxage = ndata$dxage,
              sex = ndata$sex,
              m21 = gsm_design(m21, newdata = ndata))

sim <- simulations_apbp(1e4, param = param, indivp = FALSE)

# Plot stacked probability over time
sim$pt |>
  ggplot(aes(x = time, y = pt, fill = state)) +
  geom_area() +
  labs(x = "Time (years)", y = "Probability", fill = "State") +
  theme_minimal() +
  theme(legend.position = "bottom")

################################################################


