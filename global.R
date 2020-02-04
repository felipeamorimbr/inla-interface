#Global
library(shiny)
library(shinyMenus)
library(shinyjs)
library(INLA)
library(rhandsontable)
library(shinyWidgets)
library(DT)

source("functions/file_ext.R")
source("functions/control_fixed_input.R")
source("functions/list_call.R")
source("functions/inla_models_functions.R")
source("functions/check_if_is_different.R")


accetable_formats <- c("csv", "txt")
control_compute_input <- inla.set.control.compute.default()
control_compute_input[[11]] <- inla.getOption("smtp")

control_inla_input <- inla.set.control.inla.default()

priors_distributions <- names(inla.models()$prior)
priors_distributions <- priors_distributions[-c(37,38)]

lm_family <- c("gaussian", "t")


menor <- 0
for(i in 1:64){
  if(menor <= n_hyper(inla.models()$likelihood[[i]]$pdf))
    menor <- n_hyper(inla.models()$likelihood[[i]]$pdf)
}
