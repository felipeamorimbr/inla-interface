#Check if input is ok to make the linear regression

lm_check_regression <- function(input, covariates_selected = covariates_selected){
  if(is.na(covariates_selected$names))
    return(TRUE)
  if((input$lm_intercept + length(input$lm_covariates)) == 0) #Check if at least one beta is in the regression
    return(FALSE)
  prior_fixed <- matrix(0, nrow = covariates_selected$n_covariates , ncol = 2)
  if(!all(grepl("mean", names(input)) == FALSE))
  for(i in 1:covariates_selected$n_covariates){
    browser()
    hyper_fixed[i,1] <- ifelse(i == 1, input[[ paste0("mean1") ]], input[[ paste0("mean", covariates_selected$names[i]) ]])
    hyper_fixed[i,2] <- ifelse(i == 1, input[[ paste0("prec1") ]], input[[ paste0("prec", covariates_selected$names[i]) ]])
  }
  if(!all(is.na(hyper_fixed)) && any(is.na(hyper_fixed))) #Check if fixed prios are all numeric
    return(FALSE)
  hyper_prior <- unlist(control_family_input(input))
  if(!all(is.na(hyper_prior)) && any(is.na(hyper_prior)))
    return(FALSE)
  return(TRUE)
}