# Stability selection function
stability_selection_apc <- function(B, lambdas, data_work, pp1, 
                                    first_birth, test_years, 
                                    deg_age, deg_per, deg_coh){
  source("functions/predict_casesANDinc.R")
  n <- dim(data_work)[1]
  stability_matrix <- matrix(0, ncol = pp1, nrow = B)
  mse_samples <- c()
  mae_samples <- c()
  for(j in c(1:B)){
    set.seed(j)
    idx <- sample(x=c(1:n), size=round(n/2), replace = TRUE)
    data_sample <- data_work[idx,]
    data_train <- data_work[idx,] %>%
      filter(year<test_years[1])
    nage_tmp <- max(data_train$age.p) - 1  # (Maximum age+1) - 1 = 99 ()
    nper_tmp <- max(data_train$year)- min(data_train$year) + 1  # Maximum number of years of observation ()
    aage_sample <- nage_tmp/2
    acoh_sample <- (nage_tmp+nper_tmp)/2
    aper_sample <- aage_sample + acoh_sample
    #
    design_X_train <- build_design_matrix_apc(data_train, aage_sample, aper_sample,
                                              acoh_sample, deg_age, deg_per, deg_coh)
    #
    response_pois_train <- data_train$ninc/data_train$pop.atrisk
    weights_glm_train <- data_train$pop.atrisk
    glmnet_res_train <- glmnet(x = design_X_train[,2:pp1], y = response_pois_train,
                               family = poisson(link="logit"),
                               weights = weights_glm_train,
                               alpha = 1,
                               thresh = 1e-12)
    mae_seq <- c()
    mse_seq <- c()
    for(lam in lambdas){
      coef_cur <- as.numeric(coef(glmnet_res_train, s = lam))
      active_idx <- which(coef_cur!=0)
      relaxed_fit <- glm(response_pois_train~.-1, data = as.data.frame(design_X_train[,active_idx]),
                         family = poisson(link="logit"),
                         weights = weights_glm_train)
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
                                    pop_info = data_work_full,
                                    grade_age = deg_age, 
                                    grade_period = deg_per, 
                                    grade_cohort = deg_coh,
                                    apc_params = coef_cur,  
                                    aage = aage_sample, 
                                    aper = aper_sample, 
                                    acoh = acoh_sample)
        mse_cur <- mse_cur + (obs-fab2$pred_cases)^2
        mae_cur <- mae_cur + abs(obs-fab2$pred_cases)
      }
      mse_seq <- c(mse_seq, (unname(mse_cur))/length(test_years))
      mae_seq <- c(mae_seq, (unname(mae_cur))/length(test_years))
    }
    #
    mse_samples <- c(mse_samples,min(mse_seq))
    mae_samples <- c(mae_samples,mae_seq[which.min(mse_seq)])
    #
    lambda_opt <- lambdas[which.min(mse_seq)]
    coef_opt <- as.numeric(coef(glmnet_res_train, s = lambda_opt))
    stability_matrix[j,] <- c(lambda_opt,coef_opt[-1])
  }
  return(list(stability_matrix=stability_matrix, 
              mse_vec=mse_samples,
              mae_vec = mae_samples))
}