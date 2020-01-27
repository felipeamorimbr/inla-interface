#Fuction that returns the number of Hyperparameters of a family
n_hyper <- function(family){
  return(length(names(inla.models()$likelihood[[ paste0(family) ]]$hyper)))
}

control_family_input <- function(family = family_input, ...){
  control_family <- list()
  
}