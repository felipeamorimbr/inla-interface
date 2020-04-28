#Global
library(shiny)
library(shinyMenus)
library(shinyjs)
library(INLA)
library(rhandsontable)
library(shinyWidgets)
library(DT)
library(shinydashboard)
library(enc)

source("functions/file_ext.R")
source("functions/control_fixed_input.R")
source("functions/list_call.R")
source("functions/inla_models_functions.R")
source("functions/check_if_is_different.R")
source("functions/dictionary.R")
source("functions/check_lm_ok.R")
source("functions/check_glm_ok.R")


accetable_formats <- c("csv", "txt")
control_compute_input <- inla.set.control.compute.default()
control_compute_input[[11]] <- inla.getOption("smtp")

control_inla_input <- inla.set.control.inla.default()

priors_distributions <- names(inla.models()$prior)
priors_distributions <- priors_distributions[-c(37,38)]

lm_family <- c("gaussian", "t")
glm_family <- names(inla.models()$likelihood)

language_selected <- ifelse(!exists("input$language"), "en", input$language)