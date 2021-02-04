#GLM Modal
model_buttons$glm <- smAction(id = "glm_action_bttn", label = "Hierarchical Linear Models")
model_boxes$glm <- actionButton(
  inputId = "gm_box_btn",
  box_model_ui(id = "glm_box", name = "Hierarchical Linear Model", author = "Felipe Amorim", icon = "fa-chart-area", color = "#12a19b"),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)

observeEvent(c(input$glm_action_bttn, input$glm_box), {
  validate(need(sum(input$glm_action_bttn, input$glm_box) > 0, ""))
  showModal(modalDialog(fluidPage(
    includeCSS(path = "modal/style_lm.css"),
    tabsetPanel(
      id = "glm_tabs", type = "tabs",
      tabPanel(
        title = "Select Variables",
        tags$br(),
        sel_formula_ui(id = "glm_formula")
      ),
      tabPanel(
        title = "Fixed Effects",
        tags$br(),
        fixed_effects_priors_ui(id = "glm_fixed")
      ),
      tabPanel(
        title = "Hyperpriors",
        sel_hyper_ui(
          id = "glm_family",
          linkLabel = "Link Function"
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
  title = "Hierarchical Linear Model",
  size = "l",
  fade = FALSE,
  footer = tagList(actionButton(inputId = "glm_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
})

glm_formula_data <- sel_formula(
  id = "glm_formula",
  variables = data_input()$covariates,
  resp_label = "Response Variable",
  left_label = "Variables to Choose",
  right_label = "Variables Selected",
  familyLabel = "Family",
  familyChoices = glm_family
)

glm_fixed_priors_data <- reactiveValues(priors = 0)

observeEvent(c(glm_formula_data$resp_var(), glm_formula_data$intercept(), glm_formula_data$cov_var()), {
  glm_fixed_priors_data$priors <- fixed_effects_priors(
    id = "glm_fixed",
    resp_variables = glm_formula_data$cov_var(),
    intercept = glm_formula_data$intercept()
  )
})

observeEvent(glm_formula_data$family(), {
  useShinyjs()
  glm_control_family <- sel_hyper(id = "glm_family", Link = FALSE, sel_family = glm_formula_data$family())
})

glm_tabindex <- reactiveVal(0)

observeEvent(input$glm_ok, {
  glm_formula <- paste0(glm_formula_data$resp_var(), " ~ ", paste0(glm_formula_data$cov_var(), collapse = " + "), ifelse(glm_formula_data$intercept(), " + 1", " - 1"))
  
  glm_inla <- list()
  glm_inla_call_print <- list()
  glm_tabindex(glm_tabindex() + 1)
  glm_output_name <- paste("output_tab", glm_tabindex(), sep = "_")
  glm_inla[[glm_output_name]] <- try(inla(
    formula = as.formula(glm_formula),
    data = hot_to_r(input$data),
    # family = glm_control_family$family(),
    # control.fixed = control_fixed_input(
    #   prioris = glm_fixed_priors_data$priors(),
    #   v.names = glm_formula_data$cov_var(),
    #   intercept = glm_formula_data$intercept(),
    #   covariates = input$glm_covariates
    # ),
    control.compute = control_compute_input,
    control.inla = control_inla_input,
    control.family = glm_control_family$control_family_input()
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
    
    # Close the modal with glm options
    removeModal()
    
    # Create the new call to the model
    glm_inla_call_print[[glm_output_name]] <- paste0(
      "inla(data = ", "dat",
      ", formula = ", '"', glm_formula_data$resp_var(),
      " ~ ", ifelse(glm_formula_data$intercept(), ifelse(is.null(glm_formula_data$cov_var()), "+1", ""), "-1 + "), paste0(glm_formula_data$cov_var(), collapse = " + "), '"',
      ifelse(input$glm_family_input == "gaussian", "", noquote(paste0(", family = ", '"', glm_control_family$family(), '"'))),
      ifelse(1, "", paste0(
        ", control.fixed = ",
        list_call(inla.set.control.fixed.default())
      )),
      ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
             paste0(", control.compute = ", list_call(control_compute_input))
      ),
      ifelse(checking_control_family(input), "", paste0(", control.family = ", list_call(glm_control_family$control_family_input()))),
      ")"
    )
    
    # UI of the result tab
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
              ),
              footer = downloadBttn(
                outputId = paste0("download_script_", glm_tabindex()),
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
            ),
            dropdownButton(
              icon = icon("download"),
              up = TRUE,
              status = "myclass",
              inputId = paste0("download_rdata_", glm_tabindex()),
              actionButton("test", "test"),
              tags$head(tags$style(
                "
                             .btn-myclass {
                                position: fixed;
                                bottom: 20px;
                                right: 30px;
                                z-index: 99;
                                font-size: 18px;
                                border: none;
                                outline: none;
                                background-color: #12a19b;
                                color: white;
                                cursor: pointer;
                                padding: 15px;
                                border-radius: 4px;
                             }
                              .dropup .dropdown-menu, .navbar-fixed-bottom .dropdown .dropdown-menu {
                      top: auto;
                      bottom: 8vh;
                      right: 80px;
                      left: auto;
                      position: fixed;
                  }
                             "
              ))
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
              ),
              footer = downloadBttn(
                outputId = paste0("download_summary_", glm_tabindex()),
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
    output[[paste0("glm_call", glm_tabindex())]] <- renderText({
      glm_inla_call_print[[glm_output_name]]
    })
    
    # Download Script
    output[[paste0("download_script_", glm_tabindex())]] <- downloadHandler(
      filename = function() {
        paste0("model_", glm_tabindex(), "_script", ".r")
      },
      content = function(file) {
        write(paste(paste0("dat <- read.csv2(", input$file$datapath, ")"),
                    paste0("inla_model_", glm_tabindex(), "<-", glm_inla_call_print[[glm_output_name]]),
                    sep = "\n"
        ), file)
      }
    )
    
    # Time Used
    output[[paste0("glm_time_used_", glm_tabindex())]] <- renderDataTable({
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
    output[[paste0("glm_fix_effects_", glm_tabindex())]] <- renderDataTable(
      {
        glm_inla[[glm_output_name]][["summary.fixed"]] %>%
          round(digits = 5)
      },
      options = list(
        paging = FALSE,
        dom = "t"
      )
    )
    
    # Download Summary
    output[[paste0("download_summary_", glm_tabindex())]] <- downloadHandler(
      filename = function() {
        paste0("model_", glm_tabindex(), "summary.csv")
      },
      content = function(file) {
        write.csv2(as.data.frame(glm_inla[[glm_output_name]]$summary.fixed), file = file)
      }
    )
    
    # Model Hyper
    output[[paste0("glm_model_hyper_", glm_tabindex())]] <- renderDataTable(
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
    output[[paste0("glm_neffp_", glm_tabindex())]] <- renderDataTable(
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
    output[[paste0("glm_dic_waic_", glm_tabindex())]] <- renderDataTable(
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
})