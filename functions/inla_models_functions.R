#Fuction that returns the number of Hyperparameters of a family
n_hyper <- function(family){
  return(length(names(inla.models()$likelihood[[ paste0(family) ]]$hyper)))
}

#Function that returns the number of Parameters of prior distribution
n_param_prior <- function(prior){
  return(inla.models()$prior[[paste0(prior)]]$nparameters)
}

#Function that recive family and number (the hyper param) returns the name of the hyper param
name_hyper <- function(family, number){
  return(inla.models()$likelihood[[paste0(family)]]$hyper[[number]]$name[1])
}

#Function that recive family and number (the hyper param) returns the short.name of the hyper param
shortname_hyper <- function(family, number){
  return(inla.models()$likelihood[[paste0(family)]]$hyper[[number]]$short.name[1])
}

#Function that recive family and number (the hyper param) and returns the prior default of the hyper param
hyper_default <- function(family, number){
  return(inla.models()$likelihood[[paste0(family)]]$hyper[[number]]$prior[1])
}

#
hyper_default_param <- function(family, number){
  param <- inla.models()$likelihood[[paste0(family)]]$hyper[[number]]$param
  attr(param, "inla.read.only") <- NULL
  return(param)
}
