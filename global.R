#Global
#Packages Used
library(shiny)
library(shinyMenus)
library(shinyjs)
library(INLA)
library(rhandsontable)
library(shinyWidgets)
library(DT)
library(shinydashboard)


#Functions Used
source("functions/file_ext.R")
source("functions/control_fixed_input.R")
source("functions/list_call.R")
source("functions/inla_models_functions.R")
source("functions/check_if_is_different.R")
source("functions/translate.R")
source("functions/check_lm_ok.R")
source("functions/check_glm_ok.R")

#Modules 
source("Modules/fixed_effect_priors.R")
source("Modules/sel_hyper.R")
# source("Modules/sel_formula.R")
source("Modules/box_modules.R")
source("Modules/new_chooser.R")


#Data loaded
load("data/avaliable_languages_RData.RData")
load("data/main_UI_words.RData")
load("data/options_modal_words.RData")
load("data/file_modal_words.RData")
load("data/glm_modal_words.RData")
load("data/lm_modal_words.RData")

#Bottons and boxes
model_buttons <- list()
model_boxes <- list()

accetable_formats <- c("csv", "txt")
control_compute_input <- inla.set.control.compute.default()
control_compute_input[[11]] <- inla.getOption("smtp")

control_inla_input <- inla.set.control.inla.default()

priors_distributions <- names(inla.models()$prior)
priors_distributions <- priors_distributions[priors_distributions != c("expression:",  "table:")]

lm_family <- c("gaussian", "t")
glm_family <- names(inla.models()$likelihood)

language_selected <- ifelse(!exists("input$language"), "en", input$language)

avaliable_languages <- avaliable_languages_RData$language

names(avaliable_languages) <- unlist(avaliable_languages_RData$name)