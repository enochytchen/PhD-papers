fit_and_select_pen_apc_model_v2 <- function(design_matr, pp1, data_inc, test_years, piam_out, 
                                     deg_age, deg_per, deg_coh, alp=0, first_birth,
                                     aage, aper, acoh, full_data, stand = TRUE, lam.min=1e-4){
  source("functions/predict_casesANDinc.R")
  time_start <- Sys.time()
  response_pois <- data_inc$ninc/data_inc$pop.atrisk
  weights_glm <- data_inc$pop.atrisk
  glmnet_res <- glmnet(x = design_matr[,2:pp1], y = response_pois,
                       family = poisson(link="logit"),
                       weights = weights_glm,
                       alpha = alp,
                       thresh = 1e-12,
                       lambda.min.ratio = lam.min,
                       standardize = stand)
  lambda_seq <- glmnet_res[["lambda"]]
  mae_seq <- c()
  mse_seq <- c()
  for(lam in lambda_seq){
    coef_cur <- as.numeric(coef(glmnet_res, s = lam))
    active_idx <- which(coef_cur!=0)
    relaxed_fit <- glm(response_pois~.-1, data = as.data.frame(design_matr[,active_idx]),
                       family = poisson(link="logit"),
                       weights = weights_glm)
    coef_cur[which(coef_cur!=0)]<-relaxed_fit$coefficients
    # Calculate MSE and MAE in the test set.
    mse_cur <- 0
    mae_cur <- 0
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
                                  apc_params = coef_cur,  
                                  aage = aage, 
                                  aper = aper, 
                                  acoh = acoh)
      mse_cur <- mse_cur + (obs-fab2$pred_cases)^2
      mae_cur <- mae_cur + abs(obs-fab2$pred_cases)
    }
    mse_seq <- c(mse_seq, (unname(mse_cur))/length(test_years))
    mae_seq <- c(mae_seq, (unname(mae_cur))/length(test_years))
  }
  #
  mse_final <- min(mse_seq)
  # plot(mse_seq)
  mae_final <- mae_seq[which.min(mse_seq)]
  #
  lambda_opt <- lambda_seq[which.min(mse_seq)]
  coef_opt <- as.numeric(coef(glmnet_res, s = lambda_opt))
  time_end <- Sys.time()
  elapsed_time <- time_end - time_start
  #
  return(list(model = glmnet_res, lambda_opt = lambda_opt,
         coef_opt = coef_opt, lambda_seq = lambda_seq,
         mse = mse_final, mae = mae_final, elapsed = elapsed_time))
}