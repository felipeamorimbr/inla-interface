#GLM Data
observeEvent(data_input(), {
  glm_data <<- list()
  
  glm_data$formula <<- list(
    resp_var = reactive({data_input()$covariates[1]}),
    cov_var = reactive({NULL}),
    not_selected = reactive({data_input()$covariates}),
    intercept = reactive({TRUE}),
    family = reactive({"Gaussian"})
  )
  glm_data$fixed_priors <<- inla.set.control.fixed.default()
  glm_data$hyper <<- inla.set.control.family.default()
  
  glm_data$fixed_priors_tab <<- FALSE
  glm_data$hyper_tab <<- FALSE
})

#GLM access buttons 
model_buttons$glm <- smAction("glm_action_btn", "Linear Regression")
model_boxes$glm <- actionButton(
  inputId = "glm_box_btn",
  box_model_ui(id = "glm_box", name = "Hierarchical Linear Models", author = "Felipe Amorim", icon = "fa-chart-area", color = "#12a19b"),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)

#Modal UI
observeEvent(c(input$glm_action_btn, input$glm_box_btn), {
  validate(need(sum(input$glm_action_btn, input$glm_box_btn) > 0, ""))
  
  glm_data$formula <<- new_chooser(
    id = "glm_formula",
    selected_right = glm_data$formula$cov_var(),
    selected_left = glm_data$formula$not_selected(),
    resp_var = glm_data$formula$resp_var(),
    rightLabel = "Covariates Selected",
    leftLabel = "Covariates"
  )
  
  glm_data$fixed_priors <<- fixed_effects_priors(
    id = "glm_fixed",
    cov_var = glm_data$formula$cov_var(),
    intercept = glm_data$formula$intercept()
  )
  
  showModal(modalDialog(fluidPage(
    includeCSS(path = "modal/style_lm.css"),
    shinyjs::useShinyjs(),
    tabsetPanel(
      id = "glm_tabs", type = "tabs",
      tabPanel(
        title = "Select Variables",
        tags$br(),
        new_chooser_UI(
          id = "glm_formula",
          respLabel = "Response",
          resp_var = glm_data$formula$resp_var(),
          selected_right = glm_data$formula$cov_var(),
          selected_left = glm_data$formula$not_selected(),
          familyLabel = "Family",
          familyChoices = glm_family
        )
      ),
      tabPanel(
        title = "Fixed Effects",
        tags$br(),
        fixed_effects_priors_ui(id = "glm_fixed")
      ),
      tabPanel(
        title = "Hyperpriors",
        sel_hyper_ui(
          id = "glm_hyper"
        )
      )
    ),
    tags$head(
      tags$style(HTML(
        "
          .modal-header{
          border-bottom-color: #12a19b;
          }
          "
      ))
    )
  ),
  title = "Linear Model",
  size = "l",
  fade = FALSE,
  footer = tagList(actionButton(inputId = "glm_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
  
})

observeEvent(input$glm_tabs, {
  glm_data$fixed_priors <<- fixed_effects_priors(
    id = "glm_fixed",
    cov_var = glm_data$formula$cov_var(),
    intercept = glm_data$formula$intercept()
  )
  
  glm_data$hyper <<- sel_hyper(id = "glm_hyper",
                              Link = FALSE,
                              sel_family = glm_data$formula$family(),
                              linkLabel = NULL)
})

observeEvent(input$glm_tabs,{
  glm_data$fixed_priors_tab <<- ifelse(input$glm_tabs == "Fixed Effects", TRUE, glm_data$fixed_priors_tab)
  glm_data$hyper_tab <<- ifelse(input$glm_tabs == "Hyperpriors", TRUE, glm_data$hyper_tab)
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
      title = translate("Error", language = language_selected, glm_modal_words = glm_modal_words),
      text = tags$span(
        ifelse(!(is.numeric(data_input()$data[, input$glm_responseVariable])),
               paste0(translate("-The response variable must be numeric", language = language_selected, glm_modal_words)),
               ""
        ),
        tags$br(),
        ifelse(!(length(grep("glm_mean", names(input))) == 0) && any(is.na(glm_priors)),
               paste0(translate("-The priors of fixed effects must be numeric", language = language_selected, glm_modal_words)),
               ""
        ),
        tags$br(),
        ifelse(!(length(grep("glm_hyper_dist", names(input))) == 0) && any(is.na(unlist(control_family_input(input)))),
               paste0(translate("-The Hyperprioris must be numeric", language = language_selected, glm_modal_words)),
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
        title = translate("Error in inla", language = language_selected, glm_modal_words),
        text = tags$span(
          translate("INLA has crashed. INLA try to run and failed.", language = language_selected, glm_modal_words)
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
          title = paste0(translate("Model", language = language_selected, glm_modal_words), glm_tabindex()),
          useShinydashboard(),
          useShinyjs(),
          fluidRow(
            column(
              width = 6,
              box(
                id = paste0("glm_box_call_", glm_tabindex()),
                title = translate("Call", language = language_selected, glm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                textOutput(outputId = paste0("glm_call", glm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, glm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_call", glm_tabindex()))),
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
                title = translate("Time Used", language = language_selected, glm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("glm_time_used_", glm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, glm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_time", glm_tabindex()))),
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
                title = translate("Fixed Effects", language = language_selected, glm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                dataTableOutput(outputId = paste0("glm_fix_effects_", glm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, glm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", glm_tabindex()))),
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
                    title = translate("Model Hyperparameters", language = language_selected, glm_modal_words),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("glm_model_hyper_", glm_tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, glm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", glm_tabindex()))),
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
                  title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, glm_modal_words),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("glm_neffp_", glm_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, glm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", glm_tabindex()))),
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
                    title = translate("DIC and WAIC", language = language_selected, glm_modal_words),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput(outputId = paste0("glm_dic_waic_", glm_tabindex())),
                    tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, glm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", glm_tabindex()))),
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