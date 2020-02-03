###Checking diferences between input and the default
checking_control_family <- function(input){
  n_hyper_input <- n_hyper(input$lm_family_input)
  for(i in 1:n_hyper_input){
    if(eval(parse(text = paste0((eval(parse(text = paste0("input$lm_hyper_dist_", i))))))) =! hyper_default(input$lm_family_input, i)) return(TRUE)
  }
  
  for(i in 1:n_hyper_input){
    for(j in 1:n_param_prior(eval(parse(text = paste0("input$lm_hyper_dist_", i))))){
      if(eval(parse(text = paste0("input$input_hyper_",i,"_param_", j))) != hyper_default_param(family = input$lm_family_input, j)) return(TRUE)
    }
  }
  return(FALSE)
}


  