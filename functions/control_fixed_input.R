#Function that recive the matrix of prioris and the variables names and return the input of control.fixed from INLA
control_fixed_input <- function(prioris, v.names){
  control_fixed_input_list <- list() #Creating the object to return
  if(!is.na(prioris[1,1])) #If existis, putting mean and precion of intercept on the list
    control_fixed_input_list[["mean.intercept"]] <- prioris[1,1]
  if(!is.na(prioris[1,2]))
    control_fixed_input_list[["prec.intercept"]] <- prioris[1,2]
  
  
  n <- nrow(prioris) #Number of variables + 1 (intercept)
  formula.terms <- v.names #Getting the names of variables and excluding the intercept
  mean.prioris <- list() #Creating the list for the prioris means
  prec.prioris <- list() #Creating the list for the prioris precisions
  
  j <- 1
  for(i in 2:n){ #Puting the terms inside the list
    if(!is.na(prioris[i,1])){
      mean.prioris[[j]] <- prioris[i,1]
      names(mean.prioris)[j] <- formula.terms[i]
      j <- j+1
    }
  }
  
  k <- 1
  for(l in 2:n){ #Puting the terms inside the list
    if(!is.na(prioris[l,2])){
      prec.prioris[[k]] <- prioris[l,2]
      names(prec.prioris)[k] <- formula.terms[l]
      k <- k+1
    }
  }

  if(length(mean.prioris) != 0)
    control_fixed_input_list[["mean"]] <- mean.prioris
  if(length(prec.prioris) != 0)
    control_fixed_input_list[["prec"]] <- prec.prioris
  
  return(control_fixed_input_list)
}

