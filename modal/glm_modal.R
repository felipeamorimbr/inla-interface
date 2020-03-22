#GLM Modal

#Modal's UI ----
glm_modal <- modalDialog(
  useShinyjs(),
  useSweetAlert(),
  title = translate("General Linear Models", language = "en", dictionary),
  fade = FALSE,
  size = "l",
  footer = tagList(
    actionButton("glm_ok", "Ok"),
    modalButton(translate("Cancel", "en", dictionary))
  ),
  tabsetPanel(
    id = "linear_panel",
    selected = translate("Select Variables", "en", dictionary),
    tabPanel(
      title = translate("Select Variables", "en", dictionary),
      column(
        6,
        fluidRow(
          uiOutput("glm_uiResponse"),
          uiOutput("glm_uiCovariates")
        ),
        fluidRow(
          checkboxInput(
            inputId = "glm_intercept",
            label = translate("Intercept", "en", dictionary),
            value = TRUE
          )
        ),
        fluidRow(
          verbatimTextOutput(outputId = "glm_error_no_covariate")
        )
      ),
      column(
        6, fluidRow(
          column(
            width = 12,
            align = "center",
            actionButton(
              inputId = "glm_show_fixed_prior",
              label = translate("Edit priors", "en", dictionary)
            )
          )
        ),
        fluidRow(
          column(6, shinyjs::hidden(uiOutput("glm_uiPrioriMean"))),
          column(6, shinyjs::hidden(uiOutput("glm_uiPrioriPrec")))
        )
      )
    ),
    tabPanel(
      title = translate("Hyperpriors", "en", dictionary),
      fluidRow(
        column(6, selectInput(
          inputId = "glm_family_input",
          label = translate("Family", "en", dictionary),
          choices = glm_family, 
          selected = "gaussian"
        ),
        uiOutput(outputId = "glm_link_function_ui")
        ),
        column(6, uiOutput("glm_ui_hyper_prior"),
               shinyjs::hidden(textOutput(outputId = "glm_no_hyperprior")))
      )
    )
  ),
  tags$head(tags$style(".modal-footer{border-top: 0 none}"))
)

observeEvent(input$glm_show_fixed_prior, {
  shinyjs::toggle(id = "glm_uiPrioriMean")
  shinyjs::toggle(id = "glm_uiPrioriPrec")
})

observeEvent(input$glm_action_btn, {
  showModal(glm_modal)
})

observeEvent(c(input$glm_covariates, input$glm_responseVariable, input$glm_intercept), {
  if ((length(input$glm_covariates) + input$glm_intercept) == 0) {
    shinyjs::show(id = "glm_error_no_covariate")
    output$glm_error_no_covariate <- renderText(translate("Error: no covariates selected", "en", dictionary))
    shinyjs::disable(id = "glm_ok")
  } else {
    shinyjs::hide(id = "glm_error_no_covariate")
    shinyjs::enable(id = "glm_ok")
  }
})

#Select Variable's UI ----
#UI to select response variable
output$glm_uiResponse <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  radioGroupButtons(
    inputId = "glm_responseVariable",
    label = translate("Select the response variable", "en", dictionary),
    choices = data_input()$covariates,
    justified = TRUE,
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon")
    )
  )
})

#UI to select covariates
output$glm_uiCovariates <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  checkboxGroupButtons(
    inputId = "glm_covariates",
    label = translate("Select the covariates", "en", dictionary),
    choices = data_input()$covariates[data_input()$covariates != input$glm_responseVariable],
    selected = data_input()$covariates[data_input()$covariates != input$glm_responseVariable],
    justified = TRUE,
    checkIcon = list(
      yes = icon("ok",
                 lib = "glyphicon"
      )
    )
  )
})

#Element with selected variables
glm_covariates_selected <- eventReactive(c(input$glm_responseVariable, input$glm_covariates, input$glm_intercept), {
  if (input$glm_intercept == TRUE) {
    if (is.null(input$glm_covariates)) {
      list(
        names = "(Intercept)",
        n_covariates = 1
      )
    } else {
      list(
        names = names(model.matrix(formula(data_input()$data[, c(input$glm_responseVariable, input$glm_covariates)]),
                                   data = data_input()$data
        )[1, ]),
        n_covariates = length(names(model.matrix(formula(data_input()$data[, c(input$glm_responseVariable, input$glm_covariates)]),
                                                 data = data_input()$data
        )[1, ]))
      )
    }
  } else {
    list(
      names = names(model.matrix(formula(data_input()$data[, c(input$glm_responseVariable, input$glm_covariates)]),
                                 data = data_input()$data
      )[1, ])[-1],
      n_covariates = length(names(model.matrix(formula(data_input()$data[, c(input$glm_responseVariable, input$glm_covariates)]),
                                               data = data_input()$data
      )[1, ])[-1])
    )
  }
})


#Fixed Effects' UI ----
#Column to select mean priors to fixed effects 
output$glm_uiPrioriMean <- renderUI({ 
  if (is.null(data_input()$n.variables) || (length(input$glm_covariates) + input$glm_intercept) == 0) {
    return()
  }
  lapply(1:glm_covariates_selected()$n_covariates, function(number) {
    fluidRow(
      column(6, numericInput(
        inputId = ifelse(number == 1, paste0("glm_mean1"), paste0("glm_mean", glm_covariates_selected()$names[number])),
        label = paste0("mean", glm_covariates_selected()$names[number]),
        value = 0
      ))
    )
  })
})

#Column to select prec priors to fixed effects 
output$glm_uiPrioriPrec <- renderUI({ # Generate the input boxes for the precision according to the number of columns of input file
  if (is.null(data_input()$n.variables) || (length(input$glm_covariates) + input$glm_intercept) == 0) {
    return()
  }
  lapply(1:glm_covariates_selected()$n_covariates, function(number) {
    fluidRow(
      column(6, numericInput(
        inputId = ifelse(number == 1, paste0("glm_prec1"), paste0("glm_prec", glm_covariates_selected()$names[number])),
        label = paste0("prec", glm_covariates_selected()$names[number]),
        value = ifelse((number == 1) && (input$glm_intercept == TRUE), 0, 0.001)
      ))
    )
  })
})

#Hyperpriors' UI and select Family ----

#Show text if the family have no hyperparameter
observeEvent(input$glm_family_input, {
  shinyjs::toggle(id = "glm_no_hyperprior", condition = n_hyper(input$glm_family_input) == 0)
  
  output$glm_link_function_ui <- renderUI({
    selectInput(inputId = "glm_link_function",
                label = translate("Select the link function", language = "en", dictionary),
                choices = link_avaliable(input$glm_family_input),
                selected = link_avaliable(input$glm_family_input)[1],
                multiple = FALSE)
    
  })
})

output$glm_no_hyperprior <- renderText({
  "No Hyperparameter avaliabe for this family"
})

# Create the UI with options to user select hyper priors distributions
output$glm_ui_hyper_prior <- renderUI({
  if(n_hyper(input$glm_family_input) == 0){
    return()
  }else{
  lapply(1:n_hyper(input$glm_family_input), function(number) {
    fluidRow(column(
      6, selectInput(
        inputId = paste0("glm_hyper_dist_", number),
        label = paste0(translate("Select the distribution of ", "en", dictionary), name_hyper(input$glm_family_input, number)),
        choices = priors_distributions,
        selected = hyper_default(input$glm_family_input, number),
        multiple = FALSE
      ),
      uiOutput(outputId = paste0("glm_numeric_input_hyper_", number))
    ))
  })
  }
})


# Create the UI with options to user input the values of the first hyperparamether
output$glm_numeric_input_hyper_1 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_1")]]), hyper_default(input$glm_family_input, 1), input[[ paste0("glm_hyper_dist_", 1)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_1")]]), hyper_default(input$glm_family_input, 1), input[[ paste0("glm_hyper_dist_", 1)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_1_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 1)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the second hyperparamether
output$glm_numeric_input_hyper_2 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_2")]]), hyper_default(input$glm_family_input, 2), input[[ paste0("glm_hyper_dist_", 2)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_2")]]), hyper_default(input$glm_family_input, 2), input[[ paste0("glm_hyper_dist_", 2)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_2_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 2)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the third hyperparamether
output$glm_numeric_input_hyper_3 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_3")]]), hyper_default(input$glm_family_input, 3), input[[ paste0("glm_hyper_dist_", 3)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_3")]]), hyper_default(input$glm_family_input, 3), input[[ paste0("glm_hyper_dist_", 3)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_3_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 3)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the fourth hyperparamether
output$glm_numeric_input_hyper_4 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_4")]]), hyper_default(input$glm_family_input, 4), input[[ paste0("glm_hyper_dist_", 4)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_4")]]), hyper_default(input$glm_family_input, 4), input[[ paste0("glm_hyper_dist_", 4)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_4_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 4)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the fifth hyperparamether
output$glm_numeric_input_hyper_5 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_5")]]), hyper_default(input$glm_family_input, 5), input[[ paste0("glm_hyper_dist_", 5)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_5")]]), hyper_default(input$glm_family_input, 5), input[[ paste0("glm_hyper_dist_", 5)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_5_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 5)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the sixth hyperparamether
output$glm_numeric_input_hyper_6 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_6")]]), hyper_default(input$glm_family_input, 6), input[[ paste0("glm_hyper_dist_", 6)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_6")]]), hyper_default(input$glm_family_input, 6), input[[ paste0("glm_hyper_dist_", 6)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_6_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 6)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the seventh hyperparamether
output$glm_numeric_input_hyper_7 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_7")]]), hyper_default(input$glm_family_input, 7), input[[ paste0("glm_hyper_dist_", 7)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_7")]]), hyper_default(input$glm_family_input, 7), input[[ paste0("glm_hyper_dist_", 7)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_7_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 7)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the eighth hyperparamether
output$glm_numeric_input_hyper_8 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_8")]]), hyper_default(input$glm_family_input, 8), input[[ paste0("glm_hyper_dist_", 8)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_8")]]), hyper_default(input$glm_family_input, 8), input[[ paste0("glm_hyper_dist_", 8)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_8_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 8)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the ninth hyperparamether
output$glm_numeric_input_hyper_9 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_9")]]), hyper_default(input$glm_family_input, 9), input[[ paste0("glm_hyper_dist_", 9)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_9")]]), hyper_default(input$glm_family_input, 9), input[[ paste0("glm_hyper_dist_", 9)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_9_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 9)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the tenth hyperparamether
output$glm_numeric_input_hyper_10 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_10")]]), hyper_default(input$glm_family_input, 10), input[[ paste0("glm_hyper_dist_", 10)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_10")]]), hyper_default(input$glm_family_input, 10), input[[ paste0("glm_hyper_dist_", 10)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_10_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 10)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the eleventh hyperparamether
output$glm_numeric_input_hyper_11 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_11")]]), hyper_default(input$glm_family_input, 11), input[[ paste0("glm_hyper_dist_", 11)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_11")]]), hyper_default(input$glm_family_input, 11), input[[ paste0("glm_hyper_dist_", 11)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_11_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 11)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the twelfth hyperparamether
output$glm_numeric_input_hyper_12 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_12")]]), hyper_default(input$glm_family_input, 12), input[[ paste0("glm_hyper_dist_", 12)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_12")]]), hyper_default(input$glm_family_input, 12), input[[ paste0("glm_hyper_dist_", 12)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_12_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 12)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the thirteenth hyperparamether
output$glm_numeric_input_hyper_13 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_13")]]), hyper_default(input$glm_family_input, 13), input[[ paste0("glm_hyper_dist_", 13)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_13")]]), hyper_default(input$glm_family_input, 13), input[[ paste0("glm_hyper_dist_", 13)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_13_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 13)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the fourteenth hyperparamether
output$glm_numeric_input_hyper_14 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_14")]]), hyper_default(input$glm_family_input, 14), input[[ paste0("glm_hyper_dist_", 14)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_14")]]), hyper_default(input$glm_family_input, 14), input[[ paste0("glm_hyper_dist_", 14)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_14_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 14)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the fifteenth hyperparamether
output$glm_numeric_input_hyper_15 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_15")]]), hyper_default(input$glm_family_input, 15), input[[ paste0("glm_hyper_dist_", 15)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_15")]]), hyper_default(input$glm_family_input, 15), input[[ paste0("glm_hyper_dist_", 15)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_15_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 15)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the sixteenth hyperparamether
output$glm_numeric_input_hyper_16 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_16")]]), hyper_default(input$glm_family_input, 16), input[[ paste0("glm_hyper_dist_", 16)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("glm_hyper_dist_16")]]), hyper_default(input$glm_family_input, 16), input[[ paste0("glm_hyper_dist_", 16)]])), function(n_param) {
    numericInput(
      inputId = paste0("glm_input_hyper_16_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$glm_family_input, 16)[n_param]
    )
  })
})

# What happens after the user clicks in ok to make the model
glm_tabindex <- reactiveVal(0)
observeEvent(input$glm_ok, {
  useShinyjs()
  # Create the matrix used in control_fixed_input
  glm_priors <- matrix(NA_real_, nrow = glm_covariates_selected()$n_covariates, ncol = 2)
  for (i in 1:glm_covariates_selected()$n_covariates) {
    glm_priors[i, 1] <- ifelse("glm_prec1" %in% names(input), input[[  ifelse(i == 1, paste0("glm_mean1"), paste0("glm_mean", glm_covariates_selected()$names[i]))]], NA_real_)
    glm_priors[i, 2] <- ifelse("glm_prec1" %in% names(input), input[[  ifelse(i == 1, paste0("glm_prec1"), paste0("glm_prec", glm_covariates_selected()$names[i]))]], NA_real_)
  }

  if (glm_check_regression(input, glm_covariates_selected(), glm_priors, data_input()) == FALSE) {
    sendSweetAlert(
      session = session,
      title = translate("Error", language = "en", dictionary = dictionary),
      text = tags$span(
        ifelse(!(is.numeric(data_input()$data[, input$glm_responseVariable])),
               paste0(translate("-The response variable must be numeric", language = "en", dictionary)),
               ""
        ),
        tags$br(),
        ifelse(!(length(grep("glm_mean", names(input))) == 0) && any(is.na(glm_priors)),
               paste0(translate("-The priors of fixed effects must be numeric", language = "en", dictionary)),
               ""
        ),
        tags$br(),
        ifelse(!(length(grep("glm_hyper_dist", names(input))) == 0) && any(is.na(unlist(control_family_input(input)))),
               paste0(translate("-The Hyperprioris must be numeric", language = "en", dictionary)),
               ""
        )
      ),
      html = TRUE,
      type = "error",
      closeOnClickOutside = TRUE
    )
  } else {
    # Create the input of the fomula used on inla funtion
    glm_inla.formula <- eventReactive(c(input$glm_responseVariable, input$glm_covariates, input$glm_intercept), {
      intercept <- ifelse(input$glm_intercept, " +1", " -1")
      f.covariates <- ifelse(is.null(input$glm_covariates), 0, paste0(input$glm_covariates, collapse = "+"))
      f.response <- paste0(input$glm_responseVariable)
      if (f.covariates != 0) {
        as.formula(paste0(f.response, rawToChar(as.raw(126)), paste0(c(intercept, f.covariates), collapse = "+")))
      } else {
        as.formula(paste0(f.response, rawToChar(as.raw(126)), intercept))
      }
    })
    
    # Count the number of tabs
    glm_tabindex(glm_tabindex() + 1)
    glm_output_name <- paste("output_tab", glm_tabindex(), sep = "_")
    
    # Create values to the result of the model and the edited call of the model
    glm_inla <- list()
    glm_inla_call_print <- list()
    
    browser()
    # Created the model according to user input
    glm_inla[[glm_output_name]] <- try(inla(
      formula = glm_inla.formula(),
      data = hot_to_r(input$data),
      family = input$glm_family_input,
      control.fixed = control_fixed_input(
        prioris = glm_priors,
        v.names = glm_covariates_selected()$names,
        intercept = input$glm_intercept,
        covariates = input$glm_covariates
      ),
      control.compute = control_compute_input,
      control.inla = control_inla_input,
      control.family = control_family_input(input)
    ), silent = TRUE)
    
    if (class(glm_inla[[glm_output_name]]) == "try-error") {
      sendSweetAlert(
        session = session,
        title = translate("Error in inla", language = "en", dictionary),
        text = tags$span(
          translate("Inla has crashed. Try edit the fixed priors and/or the hyperpriors and rerun", language = "en", dictionary)
        ),
        html = TRUE,
        type = "error",
        closeOnClickOutside = TRUE
      )
    } else {
      # Close the modal with lm options
      removeModal()
      
      # Create the new call to the model
      glm_inla_call_print[[glm_output_name]] <- paste0(
        "inla(data = ", "dat",
        ", formula = ", '"', input$glm_responseVariable,
        " ~ ", ifelse(input$glm_intercept, ifelse(is.null(input$glm_covariates), "+1", ""), "-1 + "), paste0(input$glm_covariates, collapse = " + "), '"',
        ifelse(input$glm_family_input == "gaussian", "", noquote(paste0(", family = ", '"', input$glm_family_input, '"'))),
        ifelse(checking_control_fixed(glm_priors, input$glm_intercept), "", paste0(
          ", control.fixed = ",
          list_call(control_fixed_input(
            prioris = glm_priors,
            v.names = glm_covariates_selected()$names,
            intercept = input$glm_intercept
          ))
        )),
        ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
               paste0(", control.compute = ", list_call(control_compute_input))
        ),
        ifelse(checking_control_family(input), "", paste0(", control.family = ", list_call(control_family_input(family_input = input$glm_family_input, input)))),
       
        ifelse(input$glm_link_function == link_avaliable(input$glm_family_input)[1], "", paste0(", control.link = list(model = ", input$glm_link_function, ")")),
        ")"
      )
      
      
      appendTab(
        inputId = "mytabs", select = TRUE,
        tabPanel(
          title = paste0(translate("Model", "en", dictionary), glm_tabindex()),
          useShinydashboard(),
          useShinyjs(),
          fluidRow(
            column(
              width = 6,
              box(
                id = paste0("glm_box_call_", glm_tabindex()),
                title = translate("Call", "en", dictionary),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                textOutput(outputId = paste0("glm_call", glm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_call", glm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_call", glm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("glm_inla_", glm_tabindex()), " <- ", glm_inla_call_print[[glm_output_name]],
                    tags$br(),
                    paste0("glm_inla_", glm_tabindex(), "$call")
                  )
                )
              )
            ),
            column(
              width = 6,
              box(
                id = paste0("glm_box_time_used", glm_tabindex()),
                title = translate("Time Used", "en", dictionary),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("glm_time_used_", glm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_time", glm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_time", glm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("glm_inla_", glm_tabindex()), " <- ", glm_inla_call_print[[glm_output_name]],
                    tags$br(),
                    paste0("glm_inla_", glm_tabindex(), "$cpu.sued")
                  )
                )
              )
            )
          ), # fluidrow ends here
          fluidRow(
            column(
              width = 12,
              box(
                id = paste0("glm_box_fix_effects_", glm_tabindex()),
                title = translate("Fixed Effects", "en", dictionary),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("glm_fix_effects_", glm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", glm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_fix_effects_", glm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("glm_inla_", glm_tabindex()), " <- ", glm_inla_call_print[[glm_output_name]],
                    tags$br(),
                    paste0("glm_inla_", glm_tabindex(), "$summary.fixed")
                  )
                )
              )
            ),
            column(
              width = 12,
              useShinyjs(),
              fluidRow(
                conditionalPanel(
                  condition = "(input.ccompute_input_2 != '') || (input.ccompute_input_2 == '' &&  input.ccompute_input_2 == true)",
                  box(
                    id = paste0("glm_box_model_hyper_", glm_tabindex()),
                    title = translate("Model Hyperparameters", "en", dictionary),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("glm_model_hyper_", glm_tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", glm_tabindex()))),
                    tags$div(
                      class = "collapse", id = paste0("showcode_model_hyper_", glm_tabindex()),
                      tags$code(
                        class = "language-r",
                        paste0("dat <- ", '"', input$file$name, '"'),
                        tags$br(),
                        paste0("glm_inla_", glm_tabindex()), " <- ", glm_inla_call_print[[glm_output_name]],
                        tags$br(),
                        paste0("glm_inla_", glm_tabindex(), "$summary.hyperpar")
                      )
                    )
                  )
                ),
                box(
                  id = paste0("glm_box_neffp_", glm_tabindex()),
                  title = translate("Expected Effective Number of Parameters in the Model", "en", dictionary),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("glm_neffp_", glm_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", glm_tabindex()))),
                  tags$div(
                    class = "collapse", id = paste0("showcode_neffp_", glm_tabindex()),
                    tags$code(
                      class = "language-r",
                      paste0("dat <- ", '"', input$file$name, '"'),
                      tags$br(),
                      paste0("glm_inla_", glm_tabindex()), " <- ", glm_inla_call_print[[glm_output_name]],
                      tags$br(),
                      paste0("glm_inla_", glm_tabindex(), "$neffp")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
                  box(
                    id = paste0("glm_box_dic_waic_", glm_tabindex()),
                    title = translate("DIC and WAIC", "en", dictionary),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("glm_dic_waic_", glm_tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", glm_tabindex()))),
                    tags$div(
                      class = "collapse", id = paste0("showcode_dic_waic_", glm_tabindex()),
                      tags$code(
                        class = "language-r",
                        paste0("dat <- ", '"', input$file$name, '"'),
                        tags$br(),
                        paste0("glm_inla_", glm_tabindex()), " <- ", glm_inla_call_print[[glm_output_name]],
                        tags$br(),
                        paste0("glm_inla_", glm_tabindex(), "$dic$dic"),
                        tags$br(),
                        paste0("glm_inla", glm_tabindex(), "$dic$dic.sat"),
                        tags$br(),
                        paste0("glm_inla", glm_tabindex(), "$dic$p.eff")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      
      # "Server" of result tab
      
      # Call
      output[[ paste0("glm_call", glm_tabindex()) ]] <- renderText({
        glm_inla_call_print[[glm_output_name]]
      })
      
      # Time Used
      output[[ paste0("glm_time_used_", glm_tabindex()) ]] <- renderDataTable({
        data_time_used <- glm_inla[[glm_output_name]][["cpu.used"]] %>%
          t() %>%
          as.data.frame(row.names = c("Time")) %>%
          round(digits = 5)
        
        DT::datatable(
          data = data_time_used,
          options = list(
            dom = "t",
            pageLength = 5
          )
        )
      })
      
      # Fixed Effects
      output[[ paste0("glm_fix_effects_", glm_tabindex())]] <- renderDataTable(
        {
          glm_inla[[glm_output_name]][["summary.fixed"]] %>%
            round(digits = 5)
        },
        options = list(
          paging = FALSE,
          dom = "t"
        )
      )
      
      # Model Hyper
      output[[ paste0("glm_model_hyper_", glm_tabindex())]] <- renderDataTable(
        {
          glm_inla[[glm_output_name]][["summary.hyperpar"]] %>%
            round(digits = 5)
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
      
      # Others (neffp)
      output[[ paste0("glm_neffp_", glm_tabindex())]] <- renderDataTable(
        {
          glm_neffp_dataframe <- glm_inla[[glm_output_name]][["neffp"]] %>%
            round(digits = 5)
          colnames(glm_neffp_dataframe) <- "Expected Value"
          glm_neffp_dataframe
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
      
      # Devicance Information Criterion (DIC)
      output[[ paste0("glm_dic_waic_", glm_tabindex())]] <- renderDataTable(
        {
          data.frame(
            "DIC" = glm_inla[[glm_output_name]][["dic"]][["dic"]],
            "DIC Saturated" = glm_inla[[glm_output_name]][["dic"]][["dic.sat"]],
            "Effective number of parameters (DIC)" = glm_inla[[glm_output_name]][["dic"]][["p.eff"]],
            "WAIC" = glm_inla[[glm_output_name]][["waic"]][["waic"]],
            "Effective number of parameters (WAIC)" = glm_inla[[glm_output_name]][["waic"]][["p.eff"]],
            row.names = "Expected Value"
          ) %>%
            round(digits = 5) %>%
            t()
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
    }
  }
})