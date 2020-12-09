# Check if input is ok to make the linear regression

lm_check_regression <- function(input, covariates_selected = covariates_selected, lm_priors, data) {
  if(!(length(grep("lm_mean", names(input))) == 0) && any(is.na(lm_priors))){ #Check if fixed effects prioris are all numeric
    return(FALSE)
  }
  if(!(lapply(data$data, class)[input$lm_responseVariable] == "numeric")){ #Check if responseVariable is numeric 
    return(FALSE)
  }
  hyper_prior <- unlist(control_family_input(input))
  if (!(length(grep("lm_hyper_dist", names(input))) == 0) && any(is.na(hyper_prior))) { #Check if hyperprior are all numeric
    return(FALSE)
  }
  return(TRUE)
}