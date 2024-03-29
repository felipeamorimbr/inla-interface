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
library(dplyr)
library(haven)
library(tools)
library(stringr)
library(rgdal)
library(spdep)
library(shinyFiles)

#Functions Used
# source("functions/file_ext.R")
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
source("Modules/random_effects_module.R")
source("Modules/result_module.R")
source("Modules/random_effects_with_fixed_number_module.R")


#Data loaded
load("data/avaliable_languages_RData.RData")
load("data/main_UI_words.RData")
load("data/options_modal_words.RData")
load("data/file_modal_words.RData")
load("data/glm_modal_words.RData")
load("data/lm_modal_words.RData")
load("data/Dictionary/words_one.RData")

#Bottons and boxes
model_buttons <- list()
model_boxes <- list()

accetable_formats <- c("csv", "txt",
                       "dta", #Stata File
                       "sas7bcat","sas7bdat", #SAS files
                       "zsav", #spss files
                       "xpt", #SAS transport files
                       "shp"
                       ) 

accetable_formats_options <- c("csv", "txt")

control_compute_input <- inla.set.control.compute.default()
control_compute_input[[11]] <- inla.getOption("smtp")

control_inla_input <- inla.set.control.inla.default()

priors_distributions <- names(inla.models()$prior)
remove_priors <- c(which(priors_distributions == "table:"), which(priors_distributions == "expression:"))
priors_distributions <- priors_distributions[-remove_priors]

latent_effects <- names(inla.models()$latent)
ts_latent_effects <- c("rw1", "rw2", "ar1", "ar", "seasonal")

lm_family <- c("gaussian", "t")
RE_lm_family <- c("gaussian", "t")
glm_family <- names(inla.models()$likelihood)
spatial_RE_family <- names(inla.models()$likelihood)
surv_family <- str_subset(string = names(inla.models()$likelihood), pattern = c("surv"))
ts_family <- names(inla.models()$likelihood)

language_selected <- ifelse(!exists("input$language"), "en", input$language)

spatial_models <- c("rw2d", "matern2d", "besagproper", "besag0", "bym")

avaliable_languages <- avaliable_languages_RData$language

names(avaliable_languages) <- unlist(avaliable_languages_RData$name)
volumes <- getVolumes()
