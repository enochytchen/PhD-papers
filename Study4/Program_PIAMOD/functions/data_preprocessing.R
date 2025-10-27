data_preprocessing <- function(inc_data,pop,popmort,sex="Male"){
  inc_data <- inc_data %>%
    left_join(pop, by = c("year"="Year","age"="Age"))
  inc_data <- inc_data %>%
    left_join(popmort, by = c("year"="Year","age"="Age"))
  inc_data <- inc_data %>%
    dplyr::select(year,age,ninc,paste0(sex,".x"),paste0(sex,".y"))
  colnames(inc_data)<-c("year","age","ninc","pop.mean","pop.dth")
  
  # Cohort variable
  inc_data$cohort <- inc_data$year-inc_data$age
  # Crude rates variable
  inc_data$crude.rates.dth <- inc_data$pop.dth/inc_data$pop.mean
  inc_data$crude.rates.inc <- inc_data$ninc/inc_data$pop.mean
  # Population at risk variables
  inc_data$pop.atrisk <- inc_data$pop.mean
  # Survivors calculation
  inc_data <- inc_data %>%
    mutate(pop.atrisk=ifelse(age==0,
                             (pop.mean)+(4/5)*pop.dth,
                             (pop.mean)+(1/2)*pop.dth))
  # Piamod variables
  inc_data$age.p <- inc_data$age + 1
  first_birth <- min(inc_data$year) - max(inc_data$age)
  inc_data$year.p <- inc_data$year - first_birth + 1
  inc_data$cohort.p <- inc_data$year.p - inc_data$age.p +1
  # Scaling factors
  nage <- max(inc_data$age.p) - 1  # (Maximum age+1) - 1 = 99 ()
  nper <- max(inc_data$year)- min(inc_data$year) + 1  # Maximum number of years of observation ()
  aage <- nage/2
  acoh <- (nage+nper)/2
  aper <- aage + acoh
  return(list(inc_data=inc_data,first_birth=first_birth,
              aage=aage,aper=aper,acoh=acoh))
}