fit_apc_model <- function(design_matr, pp1, data_inc, test_years, piam_out, 
                          deg_age, deg_per, deg_coh, first_birth,
                          aage, aper, acoh, full_data){
  source("functions/predict_casesANDinc.R")
  time_start <- Sys.time()
  response_pois <- data_inc$ninc/data_inc$pop.atrisk
  weights_glm <- data_inc$pop.atrisk
  #
  design_X_glm <- as.data.frame(design_matr[,2:pp1])
  glm_res <- glm(response_pois~., data = design_X_glm,
                 family = poisson(link="logit"),
                 weights = weights_glm)
  #
  # Calculate MSE and MAE in the test set.
  mse_final <- 0
  mae_final <- 0
  for(year in test_years){
    fab1 <- piam_out %>%
      filter(YEAR==year)
    obs <- as.numeric(fab1$OBS.CASES)
    fab2 <- predict_casesANDinc(age_vec = c(0:99), 
                                period = year, 
                                firstyear = first_birth, 
                                pop_info = full_data,
                                grade_age = deg_age, 
                                grade_period = deg_per, 
                                grade_cohort = deg_coh,
                                apc_params = glm_res$coefficients,  
                                aage = aage, 
                                aper = aper, 
                                acoh = acoh)
    mse_final <- mse_final + (obs-fab2$pred_cases)^2
    mae_final <- mae_final + abs(obs-fab2$pred_cases)
  }
  mse_final <- unname(mse_final)/length(test_years)
  mae_final <- unname(mae_final)/length(test_years)
  #
  time_end <- Sys.time()
  elapsed_time <- time_end - time_start
  #
  return(list(model = glm_res, mse = mse_final, 
              mae = mae_final, elapsed = elapsed_time))
}