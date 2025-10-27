predict_casesANDinc <- function(age_vec = c(0:99), period, firstyear, pop_info,
                                grade_age, grade_period, grade_cohort,
                                apc_params, aage, aper, acoh){
  source("functions/calc_inc.R")
  age_vec_p <- age_vec + 1
  period_p <- period-firstyear + 1
  cohort_vec_p <- period_p - age_vec_p +1
  
  pred_prob <- c()
  grad_matrix <- matrix(0, nrow = length(apc_params),
                        ncol = length(age_vec_p))
  for(j in c(1:length(age_vec_p))){
    pred_vec <- 1
    # Piamod scaling
    fab_age <- (age_vec_p[j]-aage)/aage
    fab_year <- (period_p-aper)/aper
    fab_cohort <- (cohort_vec_p[j]-acoh)/acoh
    ################
    if((grade_age>0)&(grade_period>0)&(grade_cohort>0)){
      for(k in c(1:grade_age)){
        pred_vec <- c(pred_vec,fab_age^k)
      }
      if(grade_period>1){
      for(k in c(2:grade_period)){
          pred_vec <- c(pred_vec,fab_year^k)
       }
      }
      for(k in c(1:grade_cohort)){
        pred_vec <- c(pred_vec,fab_cohort^k)
      }
    }else{
      if(grade_age>0){
        for(k in c(1:grade_age)){
          pred_vec <- c(pred_vec,fab_age^k)
        }
      }
      if(grade_period>0){
        for(k in c(1:grade_period)){
          pred_vec <- c(pred_vec,fab_year^k)
        }
      }
      if(grade_cohort>0){
        for(k in c(1:grade_cohort)){
          pred_vec <- c(pred_vec,fab_cohort^k)
        }
      }
    }
    fab <- calc_inc(apc_params,pred_vec)
    pred_prob <- c(pred_prob,fab)
    grad_matrix[,j] <-  fab*(1-fab)*pred_vec
  }
  pred_npop <- pop_info %>%
    filter((year==period) & (age%in%(age_vec)))
  pred_cases <- pred_prob*pred_npop$pop.atrisk 
  grad_matrix <- grad_matrix*pred_npop$pop.atrisk
  crude_rate <- sum(pred_cases)/sum(pred_npop$pop.mean)
  return(list(pred_cases = sum(pred_cases), pred_prob = pred_prob, 
              crude_rate = crude_rate*100000, # Crude rates per 100000 people
              grad_pred_cases = rowSums(grad_matrix))) 
}