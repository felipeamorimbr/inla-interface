#Function that return the input of control.fixed()
control_fixed_input <- function(prioris, v.names){
  n <- nrow(prioris) #Number of variables + 1 (intercept)
  formula.terms <- v.names #Getting the names of variables and excluding the intercept
  mean.prioris <- list() #Creating the list for the prioris means
  prec.prioris <- list() #Creating the list for the prioris precisions
  j <- 1
  
  for(i in 2:n){ #Puting the terms inside the list
    if(prioris[i,1] != ""){
      mean.prioris[[j]] <- prioris[i,1]
      names(mean.prioris)[j] <- formula.terms[i]
      j <- j+1
    }
  }
  if(j != n){
    mean.prioris[[j]] <- inla.set.control.fixed.default()$mean
    names(mean.prioris)[j] <- "default"
  }
  
  k <- 1
  for(l in 2:n){ #Puting the terms inside the list
    if(prioris[l,2] != ""){
      prec.prioris[[k]] <- prioris[l,2]
      names(prec.prioris)[k] <- formula.terms[l]
      k <- k+1
    }
  }
  if(k != n){
    prec.prioris[[k]] <- inla.set.control.fixed.default()$mean
    names(prec.prioris)[k] <- "default"
  }
  
  return(list(mean.intercept = ifelse(prioris[1,1] == "", inla.set.control.fixed.default()$mean.intercept, prioris[1,1]), #Organizing intercept's priori
              prec.intercept = ifelse(prioris[1,2] == "", inla.set.control.fixed.default()$prec.intercept, prioris[1,2]),
              mean = mean.prioris,
              prec = prec.prioris))
}

