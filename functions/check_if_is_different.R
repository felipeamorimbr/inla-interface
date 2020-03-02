### Checking diferences between input and the default
checking_control_family <- function(input) {
  if (is.null(input$lm_hyper_dist_1)) {
    return(TRUE)
  }
  n_hyper_input <- n_hyper(input$lm_family_input)
  for (i in 1:n_hyper_input) {
    if (eval(paste0("input$lm_hyper_dist_", i)) != hyper_default(input$lm_family_input, i)) {
      return(FALSE)
    }
  }

  for (i in 1:n_hyper_input) {
    for (j in 1:n_param_prior(eval(parse(text = paste0("input$lm_hyper_dist_", i))))) {
      if (eval(parse(text = paste0("input$input_hyper_", i, "_param_", j))) != hyper_default_param(family = input$lm_family_input, i)[j]) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}


### Checking differences between input and the default (control.fixed)
checking_control_fixed <- function(priors, intercept) {
  if(all(is.na(priors)))
     return(TRUE)
  if (intercept == TRUE) {
    if ((all(priors[, 1] == 0)) && (all(priors[-1, 2] == 0.001)) && (priors[1, 2] == 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if ((all(priors[, 1] == 0)) && (all(priors[, 2] == 0.001))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
