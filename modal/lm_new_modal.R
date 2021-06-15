# Modal for Linear Model
observeEvent(data_input(), {
  lm_data <<- list()

  lm_data$formula <<- list(
    resp_var = reactive({data_input()$covariates[1]}),
    cov_var = reactive({NULL}),
    not_selected = reactive({data_input()$covariates}),
    intercept = reactive({TRUE}),
    family = reactive({"Gaussian"})
  )
  lm_data$fixed_priors <<- reactive({NULL})
  lm_data$hyper <<- inla.set.control.family.default()
  
  lm_data$fixed_priors_tab <<- FALSE
  lm_data$hyper_tab <<- FALSE
})

observeEvent(c(input$linear_action_btn_2, input$lm_box), {
  validate(need(sum(input$linear_action_btn_2, input$lm_box) > 0, ""))
  
  lm_data$formula <<- new_chooser(
    id = "lm_formula",
    selected_right = lm_data$formula$cov_var(),
    selected_left = lm_data$formula$not_selected(),
    resp_var = lm_data$formula$resp_var(),
    rightLabel = translate("Covariates Selected", language = language_selected, words_one),
    leftLabel = translate("Covariates", language = language_selected, words_one)
  )

  lm_data$fixed_priors <<- fixed_effects_priors(
    id = "lm_fixed",
    formula_data = lm_data$formula
  )
  
  lm_data$hyper <<- sel_hyper(
    id = "lm_hyper",
    Link = FALSE,
    formula_data = lm_data$formula,
    linkLabel = NULL
  )
  showModal(modalDialog(fluidPage(
    includeCSS(path = "modal/style_lm.css"),
    shinyjs::useShinyjs(),
    tabsetPanel(
      id = "lm_tabs", type = "tabs",
      tabPanel(
        title = translate("Select Variables", language = language_selected, words_one),
        tags$br(),
        new_chooser_UI(
          id = "lm_formula",
          respLabel = translate("Response", language = language_selected, words_one),
          resp_var = lm_data$formula$resp_var(),
          selected_right = lm_data$formula$cov_var(),
          selected_left = lm_data$formula$not_selected(),
          familyLabel = translate("Family", language = language_selected, words_one),
          familyChoices = lm_family,
          data = data_input()$data,
          resp_numeric = TRUE
        )
      ),
      tabPanel(
        title = translate("Fixed Effects", language = language_selected, words_one),
        tags$br(),
        fixed_effects_priors_ui(id = "lm_fixed")
      ),
      tabPanel(
        title = translate("Hyperparameter Prior", language = language_selected, words_one),
        sel_hyper_ui(
          id = "lm_hyper"
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
  title = translate("Linear Model", language = language_selected, words_one),
  size = "l",
  fade = FALSE,
  footer = tagList(actionButton(inputId = "lm_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
})

model_buttons$lm <- smAction("linear_action_btn_2", translate("Linear Regression", language = language_selected, words_one))
model_boxes$lm <- actionButton(
  inputId = "lm_box_btn",
  box_model_ui(id = "lm_box", name = translate("Linear Model", language = language_selected, words_one), author = "Felipe Amorim", icon = "fa-chart-area", color = "#12a19b"),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)

# observeEvent(input$lm_tabs, {
#   lm_data$fixed_priors <<- fixed_effects_priors(
#     id = "lm_fixed",
#     cov_var = lm_data$formula$cov_var(),
#     intercept = lm_data$formula$intercept()
#   )
# 
#   lm_data$hyper <<- sel_hyper(id = "lm_hyper",
#                               Link = FALSE,
#                               sel_family = lm_data$formula$family(),
#                               linkLabel = NULL)
# })

# observeEvent(lm_data$formula$cov_var, {
#   browser()
#     lm_data$fixed_priors <<- fixed_effects_priors(
#       id = "lm_fixed",
#       cov_var = lm_data$formula$cov_var(),
#       intercept = lm_data$formula$intercept()
#     )
# })

observeEvent(input$lm_tabs,{
  lm_data$fixed_priors_tab <<- ifelse(input$lm_tabs == translate("Fixed Effects", language = language_selected, words_one), TRUE, lm_data$fixed_priors_tab )
  lm_data$hyper_tab <<- ifelse(input$lm_tabs == translate("Hyperparameter Prior", language = language_selected, words_one), TRUE, lm_data$hyper_tab)
})

lm_tabindex <- reactiveVal(1)
observeEvent(input$lm_ok, {
  browser()
  lm_formula <- paste0(lm_data$formula$resp_var(), " ~ ", paste0(lm_data$formula$cov_var(), collapse = " + "), ifelse(lm_data$formula$intercept(), " + 1", " - 1"))
  lm_inla <- list()
  lm_inla_call_print <- list()
  lm_output_name <- paste("output_tab", lm_tabindex(), sep = "_")
  if(lm_data$fixed_priors_tab == FALSE){
    lm_control_fixed <- inla.set.control.fixed.default()
  }else{
    lm_control_fixed <- control_fixed_input(
      prioris = lm_data$fixed_priors(),
      v.names = lm_data$formula$cov_var(),
      intercept = lm_data$formula$intercept()
    )
  }
  if(lm_data$hyper_tab == FALSE){
    lm_control_family <- inla.set.control.family.default()
  }else{
    lm_control_family <- lm_data$hyper$control_family_input()
  }
  
  lm_inla[[lm_output_name]] <- try(inla(
    formula = as.formula(lm_formula),
    data = hot_to_r(input$data),
    family = lm_data$formula$family(),
    control.fixed = lm_control_fixed,
    control.compute = control_compute_input,
    control.inla = control_inla_input,
    control.family = lm_control_family    
  ), silent = TRUE)
  if (class(lm_inla[[lm_output_name]]) == "try-error") {
    sendSweetAlert(
      session = session,
      title = translate("Error in inla", language = language_selected, words_one),
      text = tags$span(
        translate("INLA has crashed. INLA try to run and failed.", language = language_selected, words_one)
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
      ", formula = ", '"', lm_data$formula$resp_var(),
      " ~ ", ifelse(lm_data$formula$intercept(), ifelse(is.null(lm_data$formula$cov_var()), "+1", ""), "-1 + "), paste0(lm_data$formula$cov_var(), collapse = " + "), '"',
      paste0(", family = ", '"', lm_data$formula$family(), '"'),
      ifelse(lm_data$fixed_priors_tab == FALSE, "", paste0(
        ", control.fixed = ",
        list_call(lm_control_fixed)
      )),
      ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
        paste0(", control.compute = ", list_call(control_compute_input), ", control.inla = ", list_call(control_inla_input))
      ),
      ifelse(lm_data$hyper_tab == FALSE, "", paste0(", control.family = ", list_call(lm_control_family))),
      ")"
    )

    # UI of the result tab
    appendTab(
      inputId = "mytabs", select = TRUE,
      tabPanel(
        title = paste0(translate("Linear Model", language = language_selected, words_one), " ",lm_tabindex()),
        useShinydashboard(),
        useShinyjs(),
        fluidRow(
          column(
            width = 6,
            box(
              id = paste0("lm_box_call_", lm_tabindex()),
              title = translate("Call", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              textOutput(outputId = paste0("lm_call", lm_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_call", lm_tabindex()))),
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
              ),
              footer = downloadBttn(
                outputId = paste0("download_script_", lm_tabindex()),
                label = translate("Download Script", language = language_selected, words_one),
                style = "material-flat",
                color = "primary",
                size = "xs"
              )
            )
          ),
          column(
            width = 6,
            box(
              id = paste0("lm_box_time_used", lm_tabindex()),
              title = translate("Time Used", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput(outputId = paste0("lm_time_used_", lm_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_time", lm_tabindex()))),
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
            ),
            dropdownButton(
              icon = icon("download"),
              up = TRUE,
              status = "myclass",
              inputId = paste0("download_rdata_", lm_tabindex()),
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
              id = paste0("lm_box_fix_effects_", lm_tabindex()),
              title = translate("Fixed Effects", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput(outputId = paste0("lm_fix_effects_", lm_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", lm_tabindex()))),
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
              ),
              footer = downloadBttn(
                outputId = paste0("download_summary_", lm_tabindex()),
                label = translate("Save Summary data", language = language_selected, words_one),
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
                  id = paste0("lm_box_model_hyper_", lm_tabindex()),
                  title = translate("Model Hyperparameters", language = language_selected, words_one),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("lm_model_hyper_", lm_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", lm_tabindex()))),
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
                title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, words_one),
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dataTableOutput(outputId = paste0("lm_neffp_", lm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", lm_tabindex()))),
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
                  title = translate("DIC and WAIC", language = language_selected, words_one),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  dataTableOutput(outputId = paste0("lm_dic_waic_", lm_tabindex())),
                  tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", lm_tabindex()))),
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

    # Download Script
    output[[paste0("download_script_", lm_tabindex())]] <- downloadHandler(
      filename = function() {
        paste0("model_", lm_tabindex(), "_script", ".r")
      },
      content = function(file) {
        write(paste(paste0("dat <- read.csv2(", input$file$datapath, ")"),
          paste0("inla_model_", lm_tabindex(), "<-", lm_inla_call_print[[lm_output_name]]),
          sep = "\n"
        ), file)
      }
    )

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

    # Download Summary
    output[[paste0("download_summary_", lm_tabindex())]] <- downloadHandler(
      filename = function() {
        paste0("model_", lm_tabindex(), "summary.csv")
      },
      content = function(file) {
        write.csv2(as.data.frame(lm_inla[[lm_output_name]]$summary.fixed), file = file)
      }
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
    
    lm_tabindex(lm_tabindex() + 1)
  }
})
