calc_inc <- function(par,x){
  1/(1+exp(-sum(x*par)))
}