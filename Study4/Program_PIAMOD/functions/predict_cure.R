predict_cure <- function(fitted.model,ndata,t){
  wei.pred <- predict(fitted.model, newdata = ndata,
                      type="surv", time = t,se.fit = TRUE)
  
  wei.pred.mean <- as.data.frame(matrix(0, nrow = dim(wei.pred[[1]])[1],
                                        ncol = dim(wei.pred[[1]])[2]))
  for(i in c(1:dim(wei.pred[[1]])[1])){
    wei.pred.mean <- wei.pred.mean + wei.pred[[i]]
  }
  return(wei.pred.mean/dim(wei.pred[[1]])[1])
}