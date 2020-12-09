#Fuction that returns the number of Hyperparameters of a family
n_hyper <- function(family, ...){
  return(length(names(inla.models()$likelihood[[ paste0(family) ]]$hyper)))
}

control_family_input <- function(...){
  n_hyper_input <- n_hyper(family_input)
  hyper <- list()
  browser()
  for(i in 1:n_hyper_input){
    hyper[[ shortname_hyper(family_input, i) ]] <- list(prior = paste0("input$input_hyper_dist_", n_hyper_input))
    for(j in 1:n_param_prior(paste0("input$input_hyper_dist_", n_hyper_input))){
      hyper[[ shortname_hyper(family_input, i) ]][["param"]] <- c(hyper[[ shortname_hyper(family_input, i) ]][["param"]],
                                                                  paste0("input$input_hyper_",i,"_param_", j))
    }
  }
  
  hyper$control.link = list(model = ifelse(is.null(input$glm_link_function), "default", input$glm_link_function))
  return(list(hyper))
}
