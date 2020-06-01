# Modal Dialog Linear Model

# Modal Dialog' UI ----
# linear_model_modal <- modalDialog(
#   useShinyjs(),
#   useSweetAlert(),
#   title = translate("Linear Regression", language = language_selected, lm_modal_words),
#   fade = FALSE,
#   size = "l",
#   footer = uiOutput(outputId = "lm_footer"),
#   uiOutput(outputId = "lm_main_UI"),
#   tags$head(tags$style(".modal-footer{border-top: 0 none}"))
# )


output$lm_main_UI <- renderUI({
  tabsetPanel(
    id = "linear_panel",
    selected = translate("Select Variables", language = language_selected, lm_modal_words),
    tabPanel(
      title = translate("Select Variables", language = language_selected, lm_modal_words),
      column(
        6,
        fluidRow(
          uiOutput("lm_uiResponse"),
          uiOutput("lm_uiCovariates")
        ),
        fluidRow(
          checkboxInput(
            inputId = "lm_intercept",
            label = translate("Intercept", language = language_selected, lm_modal_words),
            value = TRUE
          )
        ),
        fluidRow(
          verbatimTextOutput(outputId = "lm_error_no_covariate")
        )
      ),
      column(
        6, fluidRow(
          column(
            width = 12,
            align = "center",
            actionButton(
              inputId = "lm_show_fixed_prior",
              label = translate("Edit priors", language = language_selected, lm_modal_words)
            )
          )
        ),
        fluidRow(
          column(6, shinyjs::hidden(uiOutput("lm_uiPrioriMean"))),
          column(6, shinyjs::hidden(uiOutput("lm_uiPrioriPrec")))
        )
      )
    ),
    tabPanel(
      title = translate("Hyperpriors", language = language_selected, lm_modal_words),
      fluidRow(
        column(6, selectInput(
          inputId = "lm_family_input",
          label = translate("Family", language = language_selected, lm_modal_words),
          choices = lm_family,
          selected = "normal"
        )),
        column(6, uiOutput("lm_ui_hyper_prior"))
      )
    )
  )
})

observeEvent(input$lm_show_fixed_prior, {
  shinyjs::toggle(id = "lm_uiPrioriMean")
  shinyjs::toggle(id = "lm_uiPrioriPrec")
})

output$lm_footer <- renderUI({
  fluidRow(column(
    12,
    actionButton(inputId = "lm_ok", label = translate("Ok", language = language_selected, lm_modal_words)),
    modalButton(label = translate("Cancel", language = language_selected, lm_modal_words))
  ))
})

observeEvent(input$linear_action_btn, {
  showModal(modalDialog(
    useShinyjs(),
    useSweetAlert(),
    title = translate("Linear Regression", language = language_selected, lm_modal_words),
    fade = FALSE,
    size = "l",
    footer = uiOutput(outputId = "lm_footer"),
    uiOutput(outputId = "lm_main_UI"),
    tags$head(tags$style(".modal-footer{border-top: 0 none}"))
  )
  )
})

observeEvent(c(input$lm_covariates, input$lm_responseVariable, input$lm_intercept), {
  if ((length(input$lm_covariates) + input$lm_intercept) == 0) {
    shinyjs::show(id = "lm_error_no_covariate")
    output$lm_error_no_covariate <- renderText(translate("Error: no covariates selected", language = language_selected, lm_modal_words))
    shinyjs::disable(id = "lm_ok")
  } else {
    shinyjs::hide(id = "lm_error_no_covariate")
    shinyjs::enable(id = "lm_ok")
  }
})

# Select Variable's UI ----
# UI to select response variable
output$lm_uiResponse <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  radioGroupButtons(
    inputId = "lm_responseVariable",
    label = translate("Select the response variable", language = language_selected, lm_modal_words),
    choices = data_input()$covariates,
    justified = TRUE,
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon")
    )
  )
})

# UI to select covariates
output$lm_uiCovariates <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  checkboxGroupButtons(
    inputId = "lm_covariates",
    label = translate("Select the covariates", language = language_selected, lm_modal_words),
    choices = data_input()$covariates[data_input()$covariates != input$lm_responseVariable],
    selected = data_input()$covariates[data_input()$covariates != input$lm_responseVariable],
    justified = TRUE,
    checkIcon = list(
      yes = icon("ok",
        lib = "glyphicon"
      )
    )
  )
})

# Element with selected variable
lm_covariates_selected <- eventReactive(c(input$lm_responseVariable, input$lm_covariates, input$lm_intercept), {
  if (input$lm_intercept == TRUE) {
    if (is.null(input$lm_covariates)) {
      list(
        names = "(Intercept)",
        n_covariates = 1
      )
    } else {
      list(
        names = names(model.matrix(formula(data_input()$data[, c(input$lm_responseVariable, input$lm_covariates)]),
          data = data_input()$data
        )[1, ]),
        n_covariates = length(names(model.matrix(formula(data_input()$data[, c(input$lm_responseVariable, input$lm_covariates)]),
          data = data_input()$data
        )[1, ]))
      )
    }
  } else {
    list(
      names = names(model.matrix(formula(data_input()$data[, c(input$lm_responseVariable, input$lm_covariates)]),
        data = data_input()$data
      )[1, ])[-1],
      n_covariates = length(names(model.matrix(formula(data_input()$data[, c(input$lm_responseVariable, input$lm_covariates)]),
        data = data_input()$data
      )[1, ])[-1])
    )
  }
})


# Fixed Effects' UI ----
# Column to select mean priors to fixed effects
output$lm_uiPrioriMean <- renderUI({
  if (is.null(data_input()$n.variables)) {
    return()
  }
  lapply(1:lm_covariates_selected()$n_covariates, function(number) {
    fluidRow(
      column(
        6,
        numericInput(
          inputId = ifelse(number == 1, paste0("lm_mean1"), paste0("lm_mean", lm_covariates_selected()$names[number])),
          label = paste0("mean", lm_covariates_selected()$names[number]),
          value = 0
        )
      )
    )
  })
})

# Column to select prec priors to fixed effects
output$lm_uiPrioriPrec <- renderUI({
  if (is.null(data_input()$n.variables) || (length(input$lm_covariates) + input$lm_intercept) == 0) {
    return()
  }
  lapply(1:lm_covariates_selected()$n_covariates, function(number) {
    fluidRow(
      column(6, numericInput(
        inputId = ifelse(number == 1, paste0("lm_prec1"), paste0("lm_prec", lm_covariates_selected()$names[number])),
        label = paste0("prec", lm_covariates_selected()$names[number]),
        value = ifelse((number == 1) && (input$lm_intercept == TRUE), 0, 0.001)
      ))
    )
  })
})

# Hyperpriors' UI and select Family ----

# Create the UI with options to user select hyper priors distributions
output$lm_ui_hyper_prior <- renderUI({
  lapply(1:n_hyper(input$lm_family_input), function(number) {
    fluidRow(column(
      6, selectInput(
        inputId = paste0("lm_hyper_dist_", number),
        label = paste0(translate("Select the distribution of ", language = language_selected, lm_modal_words), name_hyper(input$lm_family_input, number)),
        choices = priors_distributions,
        selected = hyper_default(input$lm_family_input, number),
        multiple = FALSE
      ),
      uiOutput(outputId = paste0("lm_numeric_input_hyper_", number))
    ))
  })
})

# Create the UI with options to user input the values of the first hyperparamether
output$lm_numeric_input_hyper_1 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[paste0("lm_hyper_dist_1")]]), hyper_default(input$lm_family_input, 1), input[[paste0("lm_hyper_dist_", 1)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[paste0("lm_hyper_dist_1")]]), hyper_default(input$lm_family_input, 1), input[[paste0("lm_hyper_dist_", 1)]])), function(n_param) {
    numericInput(
      inputId = paste0("lm_input_hyper_1_param_", n_param),
      label = paste0(translate("Parameter ", language = language_selected, lm_modal_words), n_param),
      value = hyper_default_param(input$lm_family_input, 1)[n_param]
    )
  })
})

# Create the UI with options to user input the values of the second hyperparamether
output$lm_numeric_input_hyper_2 <- renderUI({
  if (n_param_prior(ifelse(is.null(input[[paste0("lm_hyper_dist_2")]]), hyper_default(input$lm_family_input, 2), input[[paste0("lm_hyper_dist_", 2)]])) == 0) {
    return()
  }
  lapply(1:n_param_prior(ifelse(is.null(input[[paste0("lm_hyper_dist_2")]]), hyper_default(input$lm_family_input, 2), input[[paste0("lm_hyper_dist_", 2)]])), function(n_param) {
    numericInput(
      inputId = paste0("lm_input_hyper_2_param_", n_param),
      label = paste0(translate("Parameter ", language = language_selected, lm_modal_words), n_param),
      value = hyper_default_param(input$lm_family_input, 2)[n_param]
    )
  })
})




# What happens after the user clicks in ok to make the model
lm_tabindex <- reactiveVal(0)
observeEvent(input$lm_ok, {
  useShinyjs()
  # Create the matrix used in control_fixed_input
  lm_priors <- matrix(NA_real_, nrow = lm_covariates_selected()$n_covariates, ncol = 2)
  for (i in 1:lm_covariates_selected()$n_covariates) {
    lm_priors[i, 1] <- ifelse("lm_prec1" %in% names(input), input[[ifelse(i == 1, paste0("lm_mean1"), paste0("lm_mean", lm_covariates_selected()$names[i]))]], NA_real_)
    lm_priors[i, 2] <- ifelse("lm_prec1" %in% names(input), input[[ifelse(i == 1, paste0("lm_prec1"), paste0("lm_prec", lm_covariates_selected()$names[i]))]], NA_real_)
  }
  if (lm_check_regression(input, lm_covariates_selected(), lm_priors, data_input()) == FALSE) {
    sendSweetAlert(
      session = session,
      title = translate("Error", language = language_selected, lm_modal_words = lm_modal_words),
      text = tags$span(
        ifelse(!(is.numeric(data_input()$data[, input$lm_responseVariable])),
          paste0(translate("-The response variable must be numeric", language = language_selected, lm_modal_words)),
          ""
        ),
        tags$br(),
        ifelse(!(length(grep("lm_mean", names(input))) == 0) && any(is.na(lm_priors)),
          paste0(translate("-The priors of fixed effects must be numeric", language = language_selected, lm_modal_words)),
          ""
        ),
        tags$br(),
        ifelse(!(length(grep("lm_hyper_dist", names(input))) == 0) && any(is.na(unlist(control_family_input(input)))),
          paste0(translate("-The Hyperprioris must be numeric", language = language_selected, lm_modal_words)),
          ""
        )
      ),
      html = TRUE,
      type = "error",
      closeOnClickOutside = TRUE
    )
  } else {
    # Create the input of the fomula used on inla funtion
    lm_inla.formula <- eventReactive(c(input$lm_responseVariable, input$lm_covariates, input$lm_intercept), {
      intercept <- ifelse(input$lm_intercept, " +1", " -1")
      f.covariates <- ifelse(is.null(input$lm_covariates), 0, paste0(input$lm_covariates, collapse = "+"))
      f.response <- paste0(input$lm_responseVariable)
      if (f.covariates != 0) {
        as.formula(paste0(f.response, rawToChar(as.raw(126)), paste0(c(intercept, f.covariates), collapse = "+")))
      } else {
        as.formula(paste0(f.response, rawToChar(as.raw(126)), intercept))
      }
    })

    # Count the number of tabs
    lm_tabindex(lm_tabindex() + 1)
    lm_output_name <- paste("output_tab", lm_tabindex(), sep = "_")


    # Create values to the result of the model and the edited call of the model
    lm_inla <- list()
    lm_inla_call_print <- list()

    # Created the model according to user input
    lm_inla[[lm_output_name]] <- try(inla(
      formula = lm_inla.formula(),
      data = hot_to_r(input$data),
      family = input$lm_family_input,
      control.fixed = control_fixed_input(
        prioris = lm_priors,
        v.names = lm_covariates_selected()$names,
        intercept = input$lm_intercept,
        covariates = input$lm_covariates
      ),
      control.compute = control_compute_input,
      control.inla = control_inla_input,
      control.family = control_family_input(input)
    ), silent = TRUE)

    if (class(lm_inla[[lm_output_name]]) == "try-error") {
      sendSweetAlert(
        session = session,
        title = translate("Error in inla", language = language_selected, lm_modal_words),
        text = tags$span(
          translate("INLA has crashed. INLA try to run and failed.", language = language_selected, lm_modal_words)
        ),
        html = TRUE,
        type = "error",
        closeOnClickOutside = TRUE
      )
    } else {

      # Close the modal with lm options
      removeModal()

      # Create the new call to the model
      lm_inla_call_print[[lm_output_name]] <- paste0(
        "inla(data = ", "dat",
        ", formula = ", '"', input$lm_responseVariable,
        " ~ ", ifelse(input$lm_intercept, ifelse(is.null(input$lm_covariates), "+1", ""), "-1 + "), paste0(input$lm_covariates, collapse = " + "), '"',
        ifelse(input$lm_family_input == "gaussian", "", noquote(paste0(", family = ", '"', input$lm_family_input, '"'))),
        ifelse(checking_control_fixed(lm_priors, input$lm_intercept), "", paste0(
          ", control.fixed = ",
          list_call(control_fixed_input(
            prioris = lm_priors,
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
          title = paste0(translate("Model", language = language_selected, lm_modal_words), lm_tabindex()),
          useShinydashboard(),
          useShinyjs(),
          fluidRow(
            column(
              width = 6,
              box(
                id = paste0("lm_box_call_", lm_tabindex()),
                title = translate("Call", language = language_selected, lm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                textOutput(outputId = paste0("lm_call", lm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_call", lm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_call", lm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                    tags$br(),
                    paste0("lm_inla_", lm_tabindex(), "$call")
                  )
                )
              )
            ),
            column(
              width = 6,
              box(
                id = paste0("lm_box_time_used", lm_tabindex()),
                title = translate("Time Used", language = language_selected, lm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("lm_time_used_", lm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_time", lm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_time", lm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                    tags$br(),
                    paste0("lm_inla_", lm_tabindex(), "$cpu.sued")
                  )
                )
              )
            )
          ), # fluidrow ends here
          fluidRow(
            column(
              width = 12,
              box(
                id = paste0("lm_box_fix_effects_", lm_tabindex()),
                title = translate("Fixed Effects", language = language_selected, lm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("lm_fix_effects_", lm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", lm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_fix_effects_", lm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                    tags$br(),
                    paste0("lm_inla_", lm_tabindex(), "$summary.fixed")
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
                    id = paste0("lm_box_model_hyper_", lm_tabindex()),
                    title = translate("Model Hyperparameters", language = language_selected, lm_modal_words),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("lm_model_hyper_", lm_tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", lm_tabindex()))),
                    tags$div(
                      class = "collapse", id = paste0("showcode_model_hyper_", lm_tabindex()),
                      tags$code(
                        class = "language-r",
                        paste0("dat <- ", '"', input$file$name, '"'),
                        tags$br(),
                        paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                        tags$br(),
                        paste0("lm_inla_", lm_tabindex(), "$summary.hyperpar")
                      )
                    )
                  )
                ),
                box(
                  id = paste0("lm_box_neffp_", lm_tabindex()),
                  title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, lm_modal_words),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("lm_neffp_", lm_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", lm_tabindex()))),
                  tags$div(
                    class = "collapse", id = paste0("showcode_neffp_", lm_tabindex()),
                    tags$code(
                      class = "language-r",
                      paste0("dat <- ", '"', input$file$name, '"'),
                      tags$br(),
                      paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                      tags$br(),
                      paste0("lm_inla_", lm_tabindex(), "$neffp")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
                  box(
                    id = paste0("lm_box_dic_waic_", lm_tabindex()),
                    title = translate("DIC and WAIC", language = language_selected, lm_modal_words),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("lm_dic_waic_", lm_tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", lm_tabindex()))),
                    tags$div(
                      class = "collapse", id = paste0("showcode_dic_waic_", lm_tabindex()),
                      tags$code(
                        class = "language-r",
                        paste0("dat <- ", '"', input$file$name, '"'),
                        tags$br(),
                        paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                        tags$br(),
                        paste0("lm_inla_", lm_tabindex(), "$dic$dic"),
                        tags$br(),
                        paste0("lm_inla", lm_tabindex(), "$dic$dic.sat"),
                        tags$br(),
                        paste0("lm_inla", lm_tabindex(), "$dic$p.eff")
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
      output[[paste0("lm_call", lm_tabindex())]] <- renderText({
        lm_inla_call_print[[lm_output_name]]
      })

      # Time Used
      output[[paste0("lm_time_used_", lm_tabindex())]] <- renderDataTable({
        data_time_used <- lm_inla[[lm_output_name]][["cpu.used"]] %>%
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
      output[[paste0("lm_fix_effects_", lm_tabindex())]] <- renderDataTable(
        {
          lm_inla[[lm_output_name]][["summary.fixed"]] %>%
            round(digits = 5)
        },
        options = list(
          paging = FALSE,
          dom = "t"
        )
      )

      # Model Hyper
      output[[paste0("lm_model_hyper_", lm_tabindex())]] <- renderDataTable(
        {
          lm_inla[[lm_output_name]][["summary.hyperpar"]] %>%
            round(digits = 5)
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )

      # Others (neffp)
      output[[paste0("lm_neffp_", lm_tabindex())]] <- renderDataTable(
        {
          lm_neffp_dataframe <- lm_inla[[lm_output_name]][["neffp"]] %>%
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
      output[[paste0("lm_dic_waic_", lm_tabindex())]] <- renderDataTable(
        {
          data.frame(
            "DIC" = lm_inla[[lm_output_name]][["dic"]][["dic"]],
            "DIC Saturated" = lm_inla[[lm_output_name]][["dic"]][["dic.sat"]],
            "Effective number of parameters (DIC)" = lm_inla[[lm_output_name]][["dic"]][["p.eff"]],
            "WAIC" = lm_inla[[lm_output_name]][["waic"]][["waic"]],
            "Effective number of parameters (WAIC)" = lm_inla[[lm_output_name]][["waic"]][["p.eff"]],
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
