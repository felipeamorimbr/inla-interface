#Checando se a função control-fixed.input funciona corretamente
library(INLA)
library(brinla)
data("usair")

control.fixed.input <- function(prioris, formula){
  n <- nrow(prioris) #Number of variables + 1 (intercept)
  formula.terms <- labels(terms(formula)) #Getting the names of variables
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

usair.formula1 <- SO2 ~ negtemp + manuf + wind + precip + days
priori <- matrix(NA, 6, 2)

usair.inla2.t1 <- inla(usair.formula1, data = usair, control.compute = list(dic = TRUE, cpo = TRUE),
                    control.fixed = list(mean.intercept = 100, prec.intercept = 10^-2, mean = list(
                      negtemp = 2, wind = -3, default = 0), prec = 1), control.family = list(hyper = list(prec = list (prior = "gaussian",
                                                                                                                       param = c(0,1)))))

priori[1,1:2] <- c(100,10^-2)
priori[2:6,2] <- 1
priori[2,1] <- 2
priori[4,1] <- -3
priori[c(3,5,6), 1] <- 0
usair.inla2.controlfixed <- control.fixed.input(prioris = priori, formula = usair.formula1)
usair.inla2.t2 <- inla(usair.formula1, data = usair, control.compute = list(dic = TRUE, cpo = TRUE),
                       control.fixed = usair.inla2.controlfixed,
                       control.family = list(hyper = list(prec = list (prior = "gaussian", param = c(0,1)))))

#Função control.fixed.input funciona corretamente
