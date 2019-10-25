#Fucntion that will be used to check if prioris are according to lm, need changes
check_prioris <- function(prioris){
  n <- nrow(prioris)
  for(i in 1:n){
    if(!is.numeric(prioris[i,1]) || !is.numeric(prioris[i,2]))
      stop("The prioris must be numeric")
    if(prioris[i,2] <= 0)
      stop("The precision must be greater than 0")
  }
}