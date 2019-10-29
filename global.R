#Global
library(shiny)
library(shinyMenus)
library(shinyjs)
library(INLA)
library(rhandsontable)
library(shinyWidgets)

source("functions/file_ext.R")
source("functions/control_fixed_input.R")
source("functions/list_call.R")

accetable_formats <- c("csv", "txt")
control_compute_input <- inla.set.control.compute.default()
control_compute_input[[11]] <- inla.getOption("smtp")