# Modal for Survival Model
observeEvent(data_input(), {
  surv_data <<- list()

  surv_data$formula <<- list(
    resp_var = reactive({data_input()$covariates[1]}),
    status_var = reactive({data_input()$covariates[2]}),
    cov_var = reactive({NULL}),
    not_selected = reactive({data_input()$covariates}),
    intercept = reactive({TRUE}),
    family = reactive({"weibullsurv"})
  )
  surv_data$fixed_priors <<- inla.set.control.fixed.default()
  surv_data$hyper <<- inla.set.control.family.default()
})

observeEvent(c(input$survival_action_btn_2, input$surv_box), {
  validate(need(sum(input$survival_action_btn_2, input$surv_box) > 0, ""))
  surv_data$formula <<- new_chooser_surv(
    id = "surv_formula",
    selected_right = surv_data$formula$cov_var(),
    selected_left = surv_data$formula$not_selected(),
    resp_var = surv_data$formula$resp_var(),
    status_var = surv_data$formula$status_var(),
    rightLabel = "Covariates Selected",
    leftLabel = "Covariates"
  )
  surv_data$fixed_priors <<- fixed_effects_priors(
    id = "surv_fixed",
    cov_var = surv_data$formula$cov_var(),
    intercept = surv_data$formula$intercept()
  )
  showModal(modalDialog(fluidPage(
    includeCSS(path = "modal/style_lm.css"),
    shinyjs::useShinyjs(),
    tabsetPanel(
      id = "surv_tabs", type = "tabs",
      tabPanel(
        title = "Select Variables",
        tags$br(),
        new_chooser_UI_surv(
          id = "surv_formula",
          respLabel = "Time",
          resp_var = surv_data$formula$resp_var(),
          status_var = surv_data$formula$status_var(),
          selected_right = surv_data$formula$cov_var(),
          selected_left = surv_data$formula$not_selected(),
          familyLabel = "Family",
          familyChoices = surv_family
        )
      ),
      tabPanel(
        title = "Fixed Effects",
        tags$br(),
        fixed_effects_priors_ui(id = "surv_fixed")
      ),
      tabPanel(
        title = "Hyperparameters",
        sel_hyper_ui(
          id = "surv_family",
          linkLabel = NULL
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
  title = "Survival Model",
  size = "l",
  fade = FALSE,
  footer = tagList(actionButton(inputId = "surv_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
})

model_buttons$surv <- smAction("survival_action_btn_2", "Survival Model")
model_boxes$surv <- actionButton(
  inputId = "surv_box_btn",
  box_model_ui(id = "surv_box", name = "Survival Model", author = "Adriana Lana", icon = "fa-chart-area", color = "#12a19b"),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)
observeEvent(input$surv_tabs,{
  surv_data$fixed_priors <<- fixed_effects_priors(
    id = "surv_fixed",
    cov_var = surv_data$formula$cov_var(),
    intercept = surv_data$formula$intercept()
  )
  
  surv_data$hyper <<- sel_hyper(id = "surv_family",
                                Link = TRUE,
                                sel_family = surv_data$formula$family())
})

# surv_fixed_priors_data <- fixed_effects_priors(
#   id = "surv_fixed",
#   cov_var = surv_formula_data$cov_var(),
#   intercept = surv_formula_data$intercept()
# )
#
# observeEvent(surv_formula_data$family(), {
#   useShinyjs()
#   validate(need(surv_formula_data, FALSE))
#   surv_control_family <- sel_hyper(id = "surv_family", Link = FALSE, sel_family = surv_data()$family)
# })
#
# observeEvent(input$surv_cancel, {
# new_chooser_surv(
#   id = "surv_formula",
#   selected_left = NULL,
#   selected_right = NULL,
#   rightLabel = "Covariates",
#   leftLabel = "Covariates Selected"
# )
# fixed_effects_priors(
#   id = "surv_fixed",
#   resp_variables = NULL,
#   intercept = NULL
# )
# sel_hyper(id = "surv_family", Link = FALSE, sel_family = surv_formula_data$family())
#   removeModal()
# })

surv_tabindex <- reactiveVal(0)
observeEvent(input$surv_ok, {
#Formula surv
  time <- surv_data$formula$resp_var()
  status <- surv_data$formula$status_var()
  variaveis <- surv_data$formula$cov_var()
  sinla.surv <<- inla.surv(data_input()$data[[time]], data_input()$data[[status]])
if (length(variaveis) > 0)
    {
      if (surv_data$formula$intercept()) surv_formula <- paste("sinla.surv ~ 1+",  ((paste(variaveis, collapse = "+"))), sep = "")
      else (surv_formula <- paste("sinla.surv ~ -1+", ((paste(variaveis, collapse = "+"))), sep = ""))
    }
    else (surv_formula <- sinla.surv ~ 1)
  surv_inla <- list()
  surv_inla_call_print <- list()
  surv_tabindex(surv_tabindex() + 1)
  surv_output_name <- paste("output_tab", surv_tabindex(), sep = "_")
  
  surv_inla[[surv_output_name]] <- try(inla(
    formula = as.formula(surv_formula),
    data = hot_to_r(input$data),
    family = surv_data$formula$family(),
    control.fixed = control_fixed_input(
      prioris = surv_data$fixed_priors(), 
      v.names = surv_data$formula$cov_var(),
      intercept = surv_data$formula$intercept()
    ),
    control.compute = control_compute_input,
    control.inla = control_inla_input,
    control.family = surv_data$hyper$control_family_input() #surv_control_hyper$control_family_input()
  ), silent = TRUE)
  if (class(surv_inla[[surv_output_name]]) == "try-error") {
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

    # Close the modal with surv options
    removeModal()

    # Create the new call to the model
    surv_inla_call_print[[surv_output_name]] <- paste0(
      "inla(data = ", "dat",
      ', formula =  "sinla.surv ~ ', 
      ifelse(surv_data$formula$intercept(), ifelse(is.null(surv_data$formula$cov_var()), "+1", ""), "-1 + "), paste0(surv_data$formula$cov_var(), collapse = " + "), '"',
      paste0(", family = ", '"', surv_data$formula$family(), '"'),
      paste0(
        ", control.fixed = ",
        list_call(control_fixed_input(
          prioris = surv_data$fixed_priors(), 
          v.names = surv_data$formula$cov_var(),
          intercept = surv_data$formula$intercept()
        ))
      ),
      ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
        paste0(", control.compute = ", list_call(control_compute_input), ", control.inla = ", list_call(control_inla_input))
      ),
      paste0(", control.family = ", list_call(surv_data$hyper$control_family_input())),
      ")"
    )
    # UI of the result tab
    appendTab(
      inputId = "mytabs", select = TRUE,
      tabPanel(
        title = paste0(translate("Model", language = language_selected, lm_modal_words), surv_tabindex()),
        useShinydashboard(),
        useShinyjs(),
        fluidRow(
          column(
            width = 6,
            box(
              id = paste0("surv_box_call_", surv_tabindex()),
              title = translate("Call", language = language_selected, lm_modal_words),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              textOutput(outputId = paste0("surv_call", surv_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_call", surv_tabindex()))),
              tags$div(
                class = "collapse", id = paste0("showcode_call", surv_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("surv_inla_", surv_tabindex()), " <- ", surv_inla_call_print[[surv_output_name]],
                  tags$br(),
                  paste0("surv_inla_", surv_tabindex(), "$call")
                )
              ),
              footer = downloadBttn(
                outputId = paste0("download_script_", surv_tabindex()),
                label = "Download Script",
                style = "material-flat",
                color = "primary",
                size = "xs"
              )
            )
          ),
          column(
            width = 6,
            box(
              id = paste0("surv_box_time_used", surv_tabindex()),
              title = translate("Time Used", language = language_selected, lm_modal_words),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput(outputId = paste0("surv_time_used_", surv_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_time", surv_tabindex()))),
              tags$div(
                class = "collapse", id = paste0("showcode_time", surv_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("surv_inla_", surv_tabindex()), " <- ", surv_inla_call_print[[surv_output_name]],
                  tags$br(),
                  paste0("surv_inla_", surv_tabindex(), "$cpu.sued")
                )
              )
            )
            # ,dropdownButton(
            #   icon = icon("download"),
            #   up = TRUE,
            #   status = "myclass",
            #   inputId = paste0("download_rdata_", surv_tabindex()),
            #   actionButton("test", "test"),
            #   tags$head(tags$style(
            #     "
            #                  .btn-myclass {
            #                     position: fixed;
            #                     bottom: 20px;
            #                     right: 30px;
            #                     z-index: 99;
            #                     font-size: 18px;
            #                     border: none;
            #                     outline: none;
            #                     background-color: #12a19b;
            #                     color: white;
            #                     cursor: pointer;
            #                     padding: 15px;
            #                     border-radius: 4px;
            #                  }
            #                   .dropup .dropdown-menu, .navbar-fixed-bottom .dropdown .dropdown-menu {
            #           top: auto;
            #           bottom: 8vh;
            #           right: 80px;
            #           left: auto;
            #           position: fixed;
            #       }
            #                  "
            #   ))
            # )
          )
        ), # fluidrow ends here
        fluidRow(
          column(
            width = 12,
            box(
              id = paste0("surv_box_fix_effects_", surv_tabindex()),
              title = translate("Fixed Effects", language = language_selected, lm_modal_words),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput(outputId = paste0("surv_fix_effects_", surv_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", surv_tabindex()))),
              tags$div(
                class = "collapse", id = paste0("showcode_fix_effects_", surv_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("surv_inla_", surv_tabindex()), " <- ", surv_inla_call_print[[surv_output_name]],
                  tags$br(),
                  paste0("surv_inla_", surv_tabindex(), "$summary.fixed")
                )
              ),
              footer = downloadBttn(
                outputId = paste0("download_summary_", surv_tabindex()),
                label = "Save Summary data",
                style = "material-flat",
                size = "xs"
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
                  id = paste0("surv_box_model_hyper_", surv_tabindex()),
                  title = translate("Model Hyperparameters", language = language_selected, lm_modal_words),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("surv_model_hyper_", surv_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", surv_tabindex()))),
                  tags$div(
                    class = "collapse", id = paste0("showcode_model_hyper_", surv_tabindex()),
                    tags$code(
                      class = "language-r",
                      paste0("dat <- ", '"', input$file$name, '"'),
                      tags$br(),
                      paste0("surv_inla_", surv_tabindex()), " <- ", surv_inla_call_print[[surv_output_name]],
                      tags$br(),
                      paste0("surv_inla_", surv_tabindex(), "$summary.hyperpar")
                    )
                  )
                )
              ),
              box(
                id = paste0("surv_box_neffp_", surv_tabindex()),
                title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, lm_modal_words),
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dataTableOutput(outputId = paste0("surv_neffp_", surv_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", surv_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_neffp_", surv_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("surv_inla_", surv_tabindex()), " <- ", surv_inla_call_print[[surv_output_name]],
                    tags$br(),
                    paste0("surv_inla_", surv_tabindex(), "$neffp")
                  )
                )
              ),
              conditionalPanel(
                condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
                box(
                  id = paste0("surv_box_dic_waic_", surv_tabindex()),
                  title = translate("DIC and WAIC", language = language_selected, lm_modal_words),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("surv_dic_waic_", surv_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, lm_modal_words), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", surv_tabindex()))),
                  tags$div(
                    class = "collapse", id = paste0("showcode_dic_waic_", surv_tabindex()),
                    tags$code(
                      class = "language-r",
                      paste0("dat <- ", '"', input$file$name, '"'),
                      tags$br(),
                      paste0("surv_inla_", surv_tabindex()), " <- ", surv_inla_call_print[[surv_output_name]],
                      tags$br(),
                      paste0("surv_inla_", surv_tabindex(), "$dic$dic"),
                      tags$br(),
                      paste0("surv_inla", surv_tabindex(), "$dic$dic.sat"),
                      tags$br(),
                      paste0("surv_inla", surv_tabindex(), "$dic$p.eff")
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
    output[[paste0("surv_call", surv_tabindex())]] <- renderText({
      surv_inla_call_print[[surv_output_name]]
    })

    # Download Script
    output[[paste0("download_script_", surv_tabindex())]] <- downloadHandler(
      filename = function() {
        paste0("model_", surv_tabindex(), "_script", ".r")
      },
      content = function(file) {
        write(paste(paste0("dat <- read.csv2(", input$file$datapath, ")"),
          paste0("inla_model_", surv_tabindex(), "<-", surv_inla_call_print[[surv_output_name]]),
          sep = "\n"
        ), file)
      }
    )

    # Time Used
    output[[paste0("surv_time_used_", surv_tabindex())]] <- renderDataTable({
      data_time_used <- surv_inla[[surv_output_name]][["cpu.used"]] %>%
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
    output[[paste0("surv_fix_effects_", surv_tabindex())]] <- renderDataTable(
      {
        surv_inla[[surv_output_name]][["summary.fixed"]] %>%
          round(digits = 5)
      },
      options = list(
        paging = FALSE,
        dom = "t"
      )
    )

    # Download Summary
    output[[paste0("download_summary_", surv_tabindex())]] <- downloadHandler(
      filename = function() {
        paste0("model_", surv_tabindex(), "summary.csv")
      },
      content = function(file) {
        write.csv2(as.data.frame(surv_inla[[surv_output_name]]$summary.fixed), file = file)
      }
    )

    # Model Hyper
    output[[paste0("surv_model_hyper_", surv_tabindex())]] <- renderDataTable(
      {
        surv_inla[[surv_output_name]][["summary.hyperpar"]] %>%
          round(digits = 5)
      },
      options = list(
        dom = "t",
        paging = FALSE
      )
    )

    # Others (neffp)
    output[[paste0("surv_neffp_", surv_tabindex())]] <- renderDataTable(
      {
        surv_neffp_dataframe <- surv_inla[[surv_output_name]][["neffp"]] %>%
          round(digits = 5)
        colnames(surv_neffp_dataframe) <- "Expected Value"
        surv_neffp_dataframe
      },
      options = list(
        dom = "t",
        paging = FALSE
      )
    )

    # Devicance Information Criterion (DIC)
    output[[paste0("surv_dic_waic_", surv_tabindex())]] <- renderDataTable(
      {
        data.frame(
          "DIC" = surv_inla[[surv_output_name]][["dic"]][["dic"]],
          "DIC Saturated" = surv_inla[[surv_output_name]][["dic"]][["dic.sat"]],
          "Effective number of parameters (DIC)" = surv_inla[[surv_output_name]][["dic"]][["p.eff"]],
          "WAIC" = surv_inla[[surv_output_name]][["waic"]][["waic"]],
          "Effective number of parameters (WAIC)" = surv_inla[[surv_output_name]][["waic"]][["p.eff"]],
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
})
