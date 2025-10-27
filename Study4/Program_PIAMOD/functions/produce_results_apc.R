produce_results_apc <- function(min_age, max_age, firstyear, min_year,max_year,
                                data, grade_age, grade_period, grade_cohort,
                                params, aage, aper, acoh){
  source("functions/predict_casesANDinc.R")
  age_vec <- c(min_age:max_age)
  year_vec <- c(min_year:max_year)
  results <- list(pred_cases = matrix(0,nrow=length(year_vec),ncol=2),
                  grad = matrix(0,nrow=length(params),ncol=length(year_vec)))
  j<-1
  for(year in year_vec){
    results$pred_cases[j,1]<-year
    predict_cases <- predict_casesANDinc(age_vec = age_vec, 
                                         period = year, 
                                         firstyear = firstyear, 
                                         pop_info = data,
                                         grade_age = grade_age, 
                                         grade_period = grade_period, 
                                         grade_cohort = grade_cohort,
                                         apc_params = params, 
                                         aage=aage, 
                                         aper=aper, 
                                         acoh=acoh)
    results$pred_cases[j,2] <- predict_cases$pred_cases
    results$grad[,j] <- predict_cases$grad_pred_cases
    j<-j+1
  }
  return(results)
}