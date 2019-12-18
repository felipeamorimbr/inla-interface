#Fuction that returns the number of Hyperparameters of a family
n_hyper <- function(family){
  return(length(names(inla.models()$likelihood[[ paste0(family) ]]$hyper)))
}

#Function that returns the number of Parameters of prior distribution
n_param_prior <- function(prior){
  return(inla.models()$prior[[paste0(prior)]]$nparameters)
}

#Function that recice family and number returns the name of the hyper param
name_hyper <- function(family, number){
  return(inla.models()$likelihood[[paste0(family)]]$hyper[[number]]$name[1])
}

#Function that recice family and number returns the short.name of the hyper param
shortname_hyper <- function(family, numer){
  return(inla.models()$likelihood[[paste0(family)]]$hyper[[number]]$short.name[1])
}