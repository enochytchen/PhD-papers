predict_flexcure <- function(fitted.model,ndata,t, stpm = FALSE){
  gen.pred <- predict(fitted.model, newdata = ndata,
                      type="surv", time = t[-1], var.type = "ci")
  
  gen.pred.mean <- as.data.frame(matrix(0, nrow = dim(gen.pred[[1]])[1],
                                        ncol = dim(gen.pred[[1]])[2]))
  for(i in c(1:dim(gen.pred[[1]])[1])){
    gen.pred.mean <- gen.pred.mean + gen.pred[[i]]
  }
  gen.pred.mean<-gen.pred.mean/dim(gen.pred[[1]])[1]
  
  return(rbind(1,gen.pred.mean))
}