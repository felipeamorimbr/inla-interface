#Function that return the input of control.fixed()
control.fixed.input <- function(prioris, v.names){
  n <- nrow(prioris) #Number of variables + 1 (intercept)
  formula.terms <- v.names[-1] #Getting the names of variables
  mean.prioris <- list() #Creating the list for the prioris means
  prec.prioris <- list() #Creating the list for the prioris precisions
  for(i in 2:n){ #Puting the terms inside the list
    mean.prioris[[i-1]] <- ifelse(is.na(prioris[i,1]), inla.set.control.fixed.default()$mean, prioris[i,1])
    prec.prioris[[i-1]] <- ifelse(is.na(prioris[i,2]), inla.set.control.fixed.default()$prec, prioris[i,2])
  }
  names(mean.prioris) <- formula.terms #Putting names on the list
  names(prec.prioris) <- formula.terms
  return(list(mean.intercept = ifelse(is.na(prioris[1,1]), inla.set.control.fixed.default()$mean.intercept, prioris[1,1]), #Organizing intercept's priori
              prec.intercept = ifelse(is.na(prioris[1,2]), inla.set.control.fixed.default()$prec.intercept, prioris[1,2]),
              mean = mean.prioris,
              prec = prec.prioris))
}

