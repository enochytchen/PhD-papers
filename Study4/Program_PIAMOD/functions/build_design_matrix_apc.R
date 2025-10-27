build_design_matrix_apc <- function(data_work, aage, aper, acoh,
                                    n1, n2, n3){
  # Piamod scaling
  age_tmp <- (data_work$age.p - aage)/aage # Variable age in the polynomial
  year_tmp <- (data_work$year.p - aper)/aper # Variable period in the polynomial
  cohort_tmp <- (data_work$cohort.p - acoh)/acoh # Variable cohort in the polynomial
  
  j <- 2
  if((n1>0)&(n2>0)&(n3>0)){
    design_X <- matrix(0,nrow=dim(data_work)[1],
                       ncol = n1+n2+n3) 
    design_X[,1] <- 1 # column of ones
    for(k in c(1:n1)){
      design_X[,j] <- age_tmp^k
      j <- j + 1
    }
    if(n2>1){
      for(k in c(2:n2)){ # Do not fit linear term for period
        design_X[,j] <- year_tmp^k
        j <- j + 1
      }
    }
    for(k in c(1:n3)){
      design_X[,j] <- cohort_tmp^k
      j <- j + 1
    }
  }else{
    design_X <- matrix(0,nrow=dim(data_work)[1],
                       ncol = n1+n2+n3+1)
    design_X[,1] <- 1
    if(n1>0){
      for(k in c(1:n1)){
        design_X[,j] <- age_tmp^k
        j <- j + 1
      }
    }
    if(n2>0){
      for(k in c(1:n2)){ # Can allow for the linear term for period
        design_X[,j] <- year_tmp^k
        j <- j + 1
      }
    }
    if(n3>0){
      for(k in c(1:n3)){
        design_X[,j] <- cohort_tmp^k
        j <- j + 1
      }
    } 
  }
  return(design_X)
}