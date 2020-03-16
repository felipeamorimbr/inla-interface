## -- Modal Dialog Linear Model ----
## -- Modal Dialog ----
linear_model_modal <- modalDialog(
  useShinyjs(),
  useSweetAlert(),
  title = translate("Linear Regression", "en", dictionary),
  fade = FALSE,
  size = "l",
  footer = tagList(
    actionButton("lm_ok", "Ok"),
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
          uiOutput("uiResponse"),
          uiOutput("uiCovariates")
        ),
        fluidRow(
          checkboxInput(
            inputId = "lm_intercept",
            label = translate("Intercept", "en", dictionary),
            value = TRUE
          )
        ),
        fluidRow(
          verbatimTextOutput(outputId = "error_no_covariate")
        )
      ),
      column(
        6, fluidRow(
          column(
            width = 12,
            align = "center",
            actionButton(
              inputId = "lm_show_fixed_prior",
              label = translate("Edit priors", "en", dictionary)
            )
          )
        ),
        fluidRow(
          column(6, shinyjs::hidden(uiOutput("uiPrioriMean"))),
          column(6, shinyjs::hidden(uiOutput("uiPrioriPrec")))
        )
      )
    ),
    tabPanel(
      title = translate("Hyperpriors", "en", dictionary),
      fluidRow(
        column(6, selectInput(
          inputId = "lm_family_input",
          label = translate("Family", "en", dictionary),
          choices = lm_family,
          selected = "normal"
        )),
        column(6, uiOutput("ui_hyper_prior"))
      )
    )
  ),
  tags$head(tags$style(".modal-footer{border-top: 0 none}"))
)

observeEvent(input$lm_show_fixed_prior, {
  shinyjs::toggle(id = "uiPrioriMean")
  shinyjs::toggle(id = "uiPrioriPrec")
})

observeEvent(input$linear_action_btn, {
  showModal(linear_model_modal)
})

## -- Modal Render UI Response ----
output$uiResponse <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  radioGroupButtons(
    inputId = "responseVariable",
    label = translate("Select the response variable", "en", dictionary),
    choices = data_input()$covariates,
    justified = TRUE,
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon")
    )
  )
})

## -- Modal Render UI Covariates ----
output$uiCovariates <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  checkboxGroupButtons(
    inputId = "covariates",
    label = translate("Select the covariates", "en", dictionary),
    choices = data_input()$covariates[data_input()$covariates != input$responseVariable],
    selected = data_input()$covariates[data_input()$covariates != input$responseVariable],
    justified = TRUE,
    checkIcon = list(
      yes = icon("ok",
        lib = "glyphicon"
      )
    )
  )
})

lm_covariates_selected <- eventReactive(c(input$responseVariable, input$covariates, input$lm_intercept), {
  if (input$lm_intercept == TRUE) {
    if (is.null(input$covariates)) {
      list(
        names = "(Intercept)",
        n_covariates = 1
      )
    } else {
      list(
        names = names(model.matrix(formula(data_input()$data[, c(input$responseVariable, input$covariates)]),
          data = data_input()$data
        )[1, ]),
        n_covariates = length(names(model.matrix(formula(data_input()$data[, c(input$responseVariable, input$covariates)]),
          data = data_input()$data
        )[1, ]))
      )
    }
  } else {
    list(
      names = names(model.matrix(formula(data_input()$data[, c(input$responseVariable, input$covariates)]),
        data = data_input()$data
      )[1, ])[-1],
      n_covariates = length(names(model.matrix(formula(data_input()$data[, c(input$responseVariable, input$covariates)]),
        data = data_input()$data
      )[1, ])[-1])
    )
  }
})

output$uiPrioriMean <- renderUI({ # Generate the input boxes for the mean
  if (is.null(data_input()$n.variables)) {
    return()
  } # according to the number of columns of input file
  lapply(1:lm_covariates_selected()$n_covariates, function(number) {
    fluidRow(
      column(
        6,
        numericInput(
          inputId = ifelse(number == 1, paste0("mean1"), paste0("mean", lm_covariates_selected()$names[number])),
          label = paste0("mean", lm_covariates_selected()$names[number]),
          value = 0
        )
      )
    )
  })
})

output$uiPrioriPrec <- renderUI({ # Generate the input boxes for the precision according to the number of columns of input file
  if (is.null(data_input()$n.variables) || (length(input$covariates) + input$lm_intercept) == 0) {
    return()
  }
  lapply(1:lm_covariates_selected()$n_covariates, function(number) {
    fluidRow(
      column(6, numericInput(
        inputId = ifelse(number == 1, paste0("prec1"), paste0("prec", lm_covariates_selected()$names[number])),
        label = paste0("prec", lm_covariates_selected()$names[number]),
        value = ifelse((number == 1) && (input$lm_intercept == TRUE), 0, 0.001)
      ))
    )
  })
})


observeEvent(c(input$covariates, input$responseVariable, input$lm_intercept), {
  if ((length(input$covariates) + input$lm_intercept) == 0) {
    shinyjs::show(id = "error_no_covariate")
    output$error_no_covariate <- renderText(translate("Error: no covariates selected", "en", dictionary))
    shinyjs::disable(id = "lm_ok")
  } else {
    shinyjs::hide(id = "error_no_covariate")
    shinyjs::enable(id = "lm_ok")
  }
})
# Create the UI with options to user select hyper priors distributions
output$ui_hyper_prior <- renderUI({
  lapply(1:n_hyper(input$lm_family_input), function(number) {
    fluidRow(column(
      6, selectInput(
        inputId = paste0("lm_hyper_dist_", number),
        label = paste0(translate("Select the distribution of ", "en", dictionary), name_hyper(input$lm_family_input, number)),
        choices = priors_distributions,
        selected = hyper_default(input$lm_family_input, number),
        multiple = FALSE
      ),
      uiOutput(outputId = paste0("numeric_input_hyper_", number))
    ))
  })
})

# Create the UI with options to user input the values of the first hyperparamether
output$numeric_input_hyper_1 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_1")]]), hyper_default(input$lm_family_input, 1), input[[ paste0("lm_hyper_dist_", 1)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_1")]]), hyper_default(input$lm_family_input, 1), input[[ paste0("lm_hyper_dist_", 1)]])), function(n_param) {
    numericInput(
      inputId = paste0("input_hyper_1_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$lm_family_input, 1)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the second hyperparamether
output$numeric_input_hyper_2 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_2")]]), hyper_default(input$lm_family_input, 2), input[[ paste0("lm_hyper_dist_", 2)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_2")]]), hyper_default(input$lm_family_input, 2), input[[ paste0("lm_hyper_dist_", 2)]])), function(n_param) {
    numericInput(
      inputId = paste0("input_hyper_2_param_", n_param),
      label = paste0(translate("Parameter ", "en", dictionary), n_param),
      value = hyper_default_param(input$lm_family_input, 2)[n_param]
    )
  })
})




# What happens after the user clicks in ok to make the model
tabindex <- reactiveVal(0)
observeEvent(input$lm_ok, {
  useShinyjs()
  # Create the matrix used in control_fixed_input
  prioris <- matrix(NA_real_, nrow = lm_covariates_selected()$n_covariates, ncol = 2)
  for (i in 1:lm_covariates_selected()$n_covariates) {
    prioris[i, 1] <- ifelse("prec1" %in% names(input), input[[  ifelse(i == 1, paste0("mean1"), paste0("mean", lm_covariates_selected()$names[i]))]], NA_real_)
    prioris[i, 2] <- ifelse("prec1" %in% names(input), input[[  ifelse(i == 1, paste0("prec1"), paste0("prec", lm_covariates_selected()$names[i]))]], NA_real_)
  }
  if (lm_check_regression(input, lm_covariates_selected(), prioris, data_input()) == FALSE) {
    sendSweetAlert(
      session = session,
      title = translate("Error", language = "en", dictionary = dictionary),
      text = tags$span(
        ifelse(!(is.numeric(data_input()$data[, input$responseVariable])),
          paste0(translate("-The response variable must be numeric", language = "en", dictionary)),
          ""
        ),
        tags$br(),
        ifelse(!(length(grep("mean", names(input))) == 0) && any(is.na(prioris)),
          paste0(translate("-The priors of fixed effects must be numeric", language = "en", dictionary)),
          ""
        ),
        tags$br(),
        ifelse(!(length(grep("lm_hyper_dist", names(input))) == 0) && any(is.na(unlist(control_family_input(input)))),
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
    inla.formula <- eventReactive(c(input$responseVariable, input$covariates, input$lm_intercept), {
      intercept <- ifelse(input$lm_intercept, " +1", " -1")
      f.covariates <- ifelse(is.null(input$covariates), 0, paste0(input$covariates, collapse = "+"))
      f.response <- paste0(input$responseVariable)
      if (f.covariates != 0) {
        as.formula(paste0(f.response, rawToChar(as.raw(126)), paste0(c(intercept, f.covariates), collapse = "+")))
      } else {
        as.formula(paste0(f.response, rawToChar(as.raw(126)), intercept))
      }
    })

    # Count the number of tabs
    tabindex(tabindex() + 1)
    output_name <- paste("output_tab", tabindex(), sep = "_")


    # Create values to the result of the model and the edited call of the model
    lm_inla <- list()
    lm_inla_call_print <- list()

    # Created the model according to user input
    lm_inla[[output_name]] <- try(inla(
      formula = inla.formula(),
      data = hot_to_r(input$data),
      family = input$lm_family_input,
      control.fixed = control_fixed_input(
        prioris = prioris,
        v.names = lm_covariates_selected()$names,
        intercept = input$lm_intercept,
        covariates = input$covariates
      ),
      control.compute = control_compute_input,
      control.inla = control_inla_input,
      control.family = control_family_input(input)
    ), silent = TRUE)

    if (class(lm_inla[[output_name]]) == "try-error") {
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
      lm_inla_call_print[[output_name]] <- paste0(
        "inla(data = ", "dat",
        ", formula = ", '"', input$responseVariable,
        " ~ ", ifelse(input$lm_intercept, ifelse(is.null(input$covariates), "+1", ""), "-1 + "), paste0(input$covariates, collapse = " + "), '"',
        ifelse(input$lm_family_input == "gaussian", "", noquote(paste0(", family = ", '"', input$lm_family_input, '"'))),
        ifelse(checking_control_fixed(prioris, input$lm_intercept), "", paste0(
          ", control.fixed = ",
          list_call(control_fixed_input(
            prioris = prioris,
            v.names = lm_covariates_selected()$names,
            intercept = input$lm_intercept
          ))
        )),
        ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
          paste0(", control.compute = ", list_call(control_compute_input))
        ),
        ifelse(checking_control_family(input), "", paste0(", control.family = ", list_call(control_family_input(family_input = input$lm_family_input, input)))),
        ")"
      )

      # UI of the result tab
      appendTab(
        inputId = "mytabs", select = TRUE,
        tabPanel(
          title = paste0(translate("Model", "en", dictionary), tabindex()),
          useShinydashboard(),
          useShinyjs(),
          fluidRow(
            column(
              width = 6,
              box(
                id = paste0("lm_box_call_", tabindex()),
                title = translate("Call", "en", dictionary),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                textOutput(outputId = paste0("lm_call", tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_call", tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_call", tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_", tabindex()), " <- ", lm_inla_call_print[[output_name]],
                    tags$br(),
                    paste0("lm_inla_", tabindex(), "$call")
                  )
                )
              )
            ),
            column(
              width = 6,
              box(
                id = paste0("lm_box_time_used", tabindex()),
                title = translate("Time Used", "en", dictionary),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("lm_time_used_", tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_time", tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_time", tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_", tabindex()), " <- ", lm_inla_call_print[[output_name]],
                    tags$br(),
                    paste0("lm_inla_", tabindex(), "$cpu.sued")
                  )
                )
              )
            )
          ), # fluidrow ends here
          fluidRow(
            column(
              width = 12,
              box(
                id = paste0("lm_box_fix_effects_", tabindex()),
                title = translate("Fixed Effects", "en", dictionary),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("lm_fix_effects_", tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_fix_effects_", tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_", tabindex()), " <- ", lm_inla_call_print[[output_name]],
                    tags$br(),
                    paste0("lm_inla_", tabindex(), "$summary.fixed")
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
                    id = paste0("lm_box_model_hyper_", tabindex()),
                    title = translate("Model Hyperparameters", "en", dictionary),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("lm_model_hyper_", tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", tabindex()))),
                    tags$div(
                      class = "collapse", id = paste0("showcode_model_hyper_", tabindex()),
                      tags$code(
                        class = "language-r",
                        paste0("dat <- ", '"', input$file$name, '"'),
                        tags$br(),
                        paste0("lm_inla_", tabindex()), " <- ", lm_inla_call_print[[output_name]],
                        tags$br(),
                        paste0("lm_inla_", tabindex(), "$summary.hyperpar")
                      )
                    )
                  )
                ),
                box(
                  id = paste0("lm_box_neffp_", tabindex()),
                  title = translate("Expected Effective Number of Parameters in the Model", "en", dictionary),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("lm_neffp_", tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", tabindex()))),
                  tags$div(
                    class = "collapse", id = paste0("showcode_neffp_", tabindex()),
                    tags$code(
                      class = "language-r",
                      paste0("dat <- ", '"', input$file$name, '"'),
                      tags$br(),
                      paste0("lm_inla_", tabindex()), " <- ", lm_inla_call_print[[output_name]],
                      tags$br(),
                      paste0("lm_inla_", tabindex(), "$neffp")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
                  box(
                    id = paste0("lm_box_dic_waic_", tabindex()),
                    title = translate("DIC and WAIC", "en", dictionary),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("lm_dic_waic_", tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", "en", dictionary), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", tabindex()))),
                    tags$div(
                      class = "collapse", id = paste0("showcode_dic_waic_", tabindex()),
                      tags$code(
                        class = "language-r",
                        paste0("dat <- ", '"', input$file$name, '"'),
                        tags$br(),
                        paste0("lm_inla_", tabindex()), " <- ", lm_inla_call_print[[output_name]],
                        tags$br(),
                        paste0("lm_inla_", tabindex(), "$dic$dic"),
                        tags$br(),
                        paste0("lm_inla", tabindex(), "$dic$dic.sat"),
                        tags$br(),
                        paste0("lm_inla", tabindex(), "$dic$p.eff")
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
      output[[ paste0("lm_call", tabindex()) ]] <- renderText({
        lm_inla_call_print[[output_name]]
      })

      # Time Used
      output[[ paste0("lm_time_used_", tabindex()) ]] <- renderDataTable({
        data_time_used <- lm_inla[[output_name]][["cpu.used"]] %>%
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
      output[[ paste0("lm_fix_effects_", tabindex())]] <- renderDataTable(
        {
          lm_inla[[output_name]][["summary.fixed"]] %>%
            round(digits = 5)
        },
        options = list(
          paging = FALSE,
          dom = "t"
        )
      )

      # Model Hyper
      output[[ paste0("lm_model_hyper_", tabindex())]] <- renderDataTable(
        {
          lm_inla[[output_name]][["summary.hyperpar"]] %>%
            round(digits = 5)
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )

      # Others (neffp)
      output[[ paste0("lm_neffp_", tabindex())]] <- renderDataTable(
        {
          lm_neffp_dataframe <- lm_inla[[output_name]][["neffp"]] %>%
            round(digits = 5)
          colnames(lm_neffp_dataframe) <- "Expected Value"
          lm_neffp_dataframe
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )

      # Devicance Information Criterion (DIC)
      output[[ paste0("lm_dic_waic_", tabindex())]] <- renderDataTable(
        {
          data.frame(
            "DIC" = lm_inla[[output_name]][["dic"]][["dic"]],
            "DIC Saturated" = lm_inla[[output_name]][["dic"]][["dic.sat"]],
            "Effective number of parameters (DIC)" = lm_inla[[output_name]][["dic"]][["p.eff"]],
            "WAIC" = lm_inla[[output_name]][["waic"]][["waic"]],
            "Effective number of parameters (WAIC)" = lm_inla[[output_name]][["waic"]][["p.eff"]],
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
