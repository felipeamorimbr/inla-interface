# Check if input is ok to make the linear regression

lm_check_regression <- function(input, covariates_selected = covariates_selected, prioris, data) {
  if ((input$lm_intercept + length(input$lm_covariates)) == 0) { # Check if at least one beta is in the regression
    return(FALSE)
  }
  if(!(length(grep("mean", names(input))) == 0) && any(is.na(prioris))){ #Check if fixed effects prioris are all numeric
    return(FALSE)
  }
  if(!(lapply(data$data, class)[input$responseVariable] == "numeric")){ #Check if responseVariable is numeric 
    return(FALSE)
  }
  hyper_prior <- unlist(control_family_input(input))
  if (!(length(grep("lm_hyper_dist", names(input))) == 0) && any(is.na(hyper_prior))) { #Check if hyperprior are all numeric
    return(FALSE)
  }
  return(TRUE)
}