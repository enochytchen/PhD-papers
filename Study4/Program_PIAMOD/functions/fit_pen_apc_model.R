fit_pen_apc_model <- function(design_matr, pp1, data_inc, lambda_opt, 
                              alpha_opt=0, stand = TRUE){
  response_pois <- data_inc$ninc/data_inc$pop.atrisk
  weights_glm <- data_inc$pop.atrisk
  glmnet_res <- glmnet(x = design_matr[,2:pp1], y = response_pois,
                       family = poisson(link="logit"),
                       weights = weights_glm,
                       alpha = alpha_opt,
                       standardize = stand)
  coef_opt <- as.numeric(coef(glmnet_res, s = lambda_opt))
  return(list(model = glmnet_res, coef_opt = coef_opt, lambda_opt = lambda_opt,
              alpha_opt = alpha_opt))
}