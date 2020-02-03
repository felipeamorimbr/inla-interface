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

#Function that create the input to control.family on INLA
control_family_input <- function(family_input, input, ...){
  if(is.null(input$lm_hyper_dist_1)) return(inla.set.control.family.default())
  n_hyper_input <- n_hyper(family_input)
  hyper <- list()
  for(i in 1:n_hyper_input){
    hyper[[ shortname_hyper(family_input, i) ]] <- list(prior = noquote(eval(parse(text = paste0("input$lm_hyper_dist_", n_hyper_input)))))
    for(j in 1:n_param_prior(eval(parse(text = paste0("input$lm_hyper_dist_", n_hyper_input))))){
      hyper[[ shortname_hyper(family_input, i) ]][["param"]] <- c(hyper[[ shortname_hyper(family_input, i) ]][["param"]],
                                                                  eval(parse(text = paste0("input$input_hyper_",i,"_param_", j))))
    }
  }
  return(list(hyper = hyper))
}

