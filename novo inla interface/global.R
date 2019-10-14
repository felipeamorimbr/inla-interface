#Global
library(shiny)
library(shinyMenus)
library(shinyjs)
library(INLA)
library(rhandsontable)
library(shinyWidgets)

source("file_ext.R")
source("control_fixed_input.R")

accetable_formats <- c("csv", "txt")