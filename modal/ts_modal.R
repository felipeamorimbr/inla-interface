# Time Series Modal
model_buttons$ts <- smAction(id = "ts_action_bttn", label = "Hierarchical Time Series Models")
model_boxes$ts <- actionButton(
  inputId = "ts_box_btn",
  box_model_ui(
    id = "ts_box",
    name = "Hierarchical Time Series Models",
    author = "Italo Ferreira",
    icon = "fa-chart-area",
    color = "#12a19b"
  ),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)

observeEvent(data_input(), {
  ts_data <<- list()
  
  ts_data$formula <<- list(
    resp_var = reactive({
      data_input()$covariates[1]
    }),
    cov_var = reactive({
      NULL
    }),
    not_selected = reactive({
      data_input()$covariates
    }),
    intercept = reactive({
      TRUE
    }),
    family = reactive({
      "Gaussian"
    })
  )
  
  ts_data$fixed_priors <<- inla.set.control.fixed.default()
  ts_data$hyper <<- inla.set.control.family.default()
  ts_data$random_formula <<- NULL
  ts_data$fixed_priors_tab <<- FALSE
  ts_data$hyper_tab <<- FALSE
  ts_data$ts_tab <<- FALSE
})


observeEvent(c(input$ts_action_bttn, input$ts_box_btn), {
  validate(need(sum(
    input$ts_action_bttn, input$ts_box_btn
  ) > 0, ""))
  
  ts_data$formula <<- new_chooser(
    id = "ts_formula",
    selected_right = ts_data$formula$cov_var(),
    selected_left = ts_data$formula$not_selected(),
    resp_var = ts_data$formula$resp_var(),
    rightLabel = translate("Covariates Selected", language = language_selected, words_one),
    leftLabel = translate("Covariates", language = language_selected, words_one)
  )
  
  ts_data$fixed_priors <<-
    fixed_effects_priors(id = "ts_fixed", formula_data = ts_data$formula)
  
  ts_data$hyper <<- sel_hyper(
    id = "ts_hyper",
    Link = FALSE,
    formula_data = ts_data$formula,
    linkLabel = NULL
  )
  
  ts_data$random_formula <<- random_effects_with_fix(
    id = "ts_random_formula",
    formula_data = ts_data$formula,
    model_choice = list(ts_latent_effects),
    number_random_effects = 1,
    random_effect_label = c("Time")
  )
  
  showModal(modalDialog(
    fluidPage(
      includeCSS(path = "modal/style_lm.css"),
      shinyjs::useShinyjs(),
      tabsetPanel(
        id = "ts_tabs",
        type = "tabs",
        tabPanel(
          title = translate("Select Variables", language = language_selected, words_one),
          tags$br(),
          new_chooser_UI(
            id = "ts_formula",
            respLabel = translate("Response", language = language_selected, words_one),
            resp_var = ts_data$formula$resp_var(),
            selected_right = ts_data$formula$cov_var(),
            selected_left = ts_data$formula$not_selected(),
            familyLabel = translate("Family", language = language_selected, words_one),
            familyChoices = ts_family,
            data = data_input()$data,
            resp_numeric = TRUE
          )
        ),
        tabPanel(
          title = translate("Random Effect", language_selected, words_one),
          shinyjs::useShinyjs(),
          navlistPanel(
            id = "ts_random_formula_tabs",
            widths = c(3, 9),
            fluid = FALSE,
            tabPanel(
              title = translate("Formula", language = language_selected, words_one),
              random_effects_with_fix_ui(id = "ts_random_formula"),
              actionButton(
                "confirm_model",
                translate("Confirm Model", language_selected, words_one)
              ),
              pickerInput(
                # Select the order of AR model
                inputId = "order",
                label = translate("Order", language = language_selected, dictionary = words_one),
                choices = 1:20,
                multiple = FALSE
              )
            ),
            tabPanel(
              title = translate("Random Effects Priors", language = language_selected, words_one)
            )
          )
        ),
        tabPanel(
          title = translate("Fixed Effects", language = language_selected, words_one),
          tags$br(),
          fixed_effects_priors_ui(id = "ts_fixed")
        ),
        tabPanel(
          title = translate("Hyperparameter Prior", language = language_selected, words_one),
          sel_hyper_ui(id = "ts_hyper")
        )
      ),
      tags$head(tags$style(
        HTML("
          .modal-header{
          border-bottom-color: #12a19b;
          }
          ")
      ))
    ),
    title = translate("Hierarchical Time Series Models", language = language_selected, words_one),
    size = "l",
    fade = FALSE,
    footer = tagList(
      actionButton(inputId = "ts_ok", label = "Ok"),
      modalButton(label = "Cancel")
    )
  ))
})

observeEvent(input$ts_tabs, {
  ts_data$fixed_priors_tab <<- ifelse(
      input$ts_tabs == translate("Fixed Effects", language = language_selected, words_one),
      TRUE,
      ts_data$fixed_priors_tab
    )
  ts_data$hyper_tab <<- ifelse(
      input$ts_tabs == translate("Hyperparameter Prior", language = language_selected, words_one),
      TRUE,
      ts_data$hyper_tab
    )
  ts_data$ts_tab <<- ifelse(
      input$ts_tabs == translate("Random Effect", language_selected, words_one),
      TRUE,
      ts_data$ts_tab
    )
})

ts_tabindex <- reactiveVal(1)

# shinyjs::disable("order")
# 
# Observer the change in the model to disable Order
observeEvent(input$confirm_model, {
  print(ts_data$random_formula()$model)
  if (ts_data$random_formula()$model == "ar") {
    shinyjs::enable("order")
    print("Ativar")
  } else{
    shinyjs::disable("order")
    print("Desativar")
  }
})

# Quando define os prametros e clica no OK
observeEvent(input$ts_ok, {
  useSweetAlert()
  
  if (ts_data$ts_tab == FALSE) {
    sendSweetAlert(
      session = session,
      title = translate("ERROR", language = language_selected, words_one),
      type = "error",
      text = translate("Random Effect not selected", language = language_selected, words_one),
      closeOnClickOutside = FALSE,
      showCloseButton = TRUE
    )
    return()
  }
  
  covariates_aux <- c(ts_data$formula$cov_var(), ts_data$random_formula()$cov)
  condition_aux <- !identical(sort(covariates_aux), sort(unique(covariates_aux)))
  if (condition_aux) {
    sendSweetAlert(
      session = session,
      title = translate("ERROR", language = language_selected, words_one),
      type = "error",
      text = translate(
        "ERROR: The same covariate was select more the once. If that's the intention create a column for each time that will be used and try again.",
        language = language_selected,
        words_one
      ),
      closeOnClickOutside = FALSE,
      showCloseButton = TRUE
    )
    return()
  }
  
  ts_formula <- paste0(
    ts_data$formula$resp_var(),
    " ~ ",
    paste0(ts_data$formula$cov_var(), collapse = " + "),
    ifelse(ts_data$formula$intercept(), " + 1", " - 1"),
    " + ",
    " f(",
    ts_data$random_formula()$cov,
    ", model = '",
    ts_data$random_formula()$model,
    "'",
    if (ts_data$random_formula()$model == "ar") {
      paste0(", order = ", input$order)
    }else if (ts_data$random_formula()$model == "seasonal") {
      paste0(", season.length = ", input$order)
    },
    ")"
  )
  print(ts_formula)
  
  
  ts_inla <- list()
  ts_inla_call_print <- list()
  ts_output_name <- paste("output_tab", ts_tabindex(), sep = "_")
  
  if (ts_data$fixed_priors_tab == FALSE) {
    ts_control_fixed <- inla.set.control.fixed.default()
  } else {
    ts_control_fixed <- control_fixed_input(
      prioris = ts_data$fixed_priors(),
      v.names = ts_data$formula$cov_var(),
      intercept = ts_data$formula$intercept()
    )
  }
  if (ts_data$hyper_tab == FALSE) {
    ts_control_family <- inla.set.control.family.default()
  } else {
    ts_control_family <- ts_data$hyper$control_family_input()
  }
  
  ts_inla[[ts_output_name]] <- try(inla(
    formula = as.formula(ts_formula),
    data = hot_to_r(input$data),
    family = ts_data$formula$family(),
    control.fixed = ts_control_fixed,
    control.compute = control_compute_input,
    control.inla = control_inla_input,
    control.family = ts_control_family
  ),
  silent = TRUE)
  if (class(ts_inla[[ts_output_name]]) == "try-error") {
    sendSweetAlert(
      session = session,
      title = translate("Error in inla", language = language_selected, words_one),
      text = tags$span(
        translate(
          "INLA has crashed. INLA try to run and failed.",
          language = language_selected,
          words_one
        )
      ),
      html = TRUE,
      type = "error",
      closeOnClickOutside = TRUE
    )
  } else {
    removeModal()
    
    ts_inla_call_print[[ts_output_name]] <- paste0(
      "inla(data = ",
      "dat",
      str_interp(", formula = \"${ts_formula}\""),
      paste0(", family = ", '"', ts_data$formula$family(), '"'),
      ifelse(
        ts_data$fixed_priors_tab == FALSE,
        "",
        paste0(", control.fixed = ",
               list_call(ts_control_fixed))
      ),
      ifelse(
        identical(paste0(input$ok_btn_options_modal), character(0)),
        "",
        paste0(
          ", control.compute = ",
          list_call(control_compute_input),
          ", control.inla = ",
          list_call(control_inla_input)
        )
      ),
      ifelse(
        ts_data$hyper_tab == FALSE,
        "",
        paste0(", control.family = ", list_call(ts_control_family))
      ),
      ")"
    )
  }
  
  appendTab(
    inputId = "mytabs",
    select = TRUE,
    tabPanel(
      title = paste0(
        translate("Hierarchical Time Series Models", language = language_selected, words_one),
        " ",
        ts_tabindex()
      ),
      useShinydashboard(),
      useShinyjs(),
      fluidRow(
        column(
          width = 6,
          box(
            id = paste0("ts_box_call_", ts_tabindex()),
            title = translate("Call", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textOutput(outputId = paste0("ts_call", ts_tabindex())),
            tags$b(
              tags$a(
                icon("code"),
                translate("Show code", language = language_selected, words_one),
                `data-toggle` = "collapse",
                href = paste0("#showcode_call", ts_tabindex())
              )
            ),
            tags$div(
              class = "collapse",
              id = paste0("showcode_call", ts_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("ts_inla_", ts_tabindex()),
                " <- ",
                ts_inla_call_print[[ts_output_name]],
                tags$br(),
                paste0("ts_inla_", ts_tabindex(), "$call")
              )
            ),
            footer = downloadBttn(
              outputId = paste0("download_script_", ts_tabindex()),
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
            id = paste0("ts_box_time_used", ts_tabindex()),
            title = translate("Time Used", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput(outputId = paste0("ts_time_used_", ts_tabindex())),
            tags$b(
              tags$a(
                icon("code"),
                translate("Show code", language = language_selected, words_one),
                `data-toggle` = "collapse",
                href = paste0("#showcode_time", ts_tabindex())
              )
            ),
            tags$div(
              class = "collapse",
              id = paste0("showcode_time", ts_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("ts_inla_", ts_tabindex()),
                " <- ",
                ts_inla_call_print[[ts_output_name]],
                tags$br(),
                paste0("ts_inla_", ts_tabindex(), "$cpu.sued")
              )
            )
          ),
          dropdownButton(
            icon = icon("download"),
            up = TRUE,
            status = "myclass",
            inputId = paste0("download_rdata_", ts_tabindex()),
            actionButton("test", "test"),
            tags$head(
              tags$style(
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
              )
            )
          )
        )
      ),
      # fluidrow ends here
      fluidRow(
        column(
          width = 12,
          box(
            id = paste0("ts_box_fix_effects_", ts_tabindex()),
            title = translate("Fixed Effects", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput(outputId = paste0("ts_fix_effects_", ts_tabindex())),
            tags$b(
              tags$a(
                icon("code"),
                translate("Show code", language = language_selected, words_one),
                `data-toggle` = "collapse",
                href = paste0("#showcode_fix_effects_", ts_tabindex())
              )
            ),
            tags$div(
              class = "collapse",
              id = paste0("showcode_fix_effects_", ts_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("ts_inla_", ts_tabindex()),
                " <- ",
                ts_inla_call_print[[ts_output_name]],
                tags$br(),
                paste0("ts_inla_", ts_tabindex(), "$summary.fixed")
              )
            ),
            footer = downloadBttn(
              outputId = paste0("download_summary_", ts_tabindex()),
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
                id = paste0("ts_box_model_hyper_", ts_tabindex()),
                title = translate("Model Hyperparameters", language = language_selected, words_one),
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dataTableOutput(outputId = paste0("ts_model_hyper_", ts_tabindex())),
                tags$b(
                  tags$a(
                    icon("code"),
                    translate("Show code", language = language_selected, words_one),
                    `data-toggle` = "collapse",
                    href = paste0("#showcode_model_hyper_", ts_tabindex())
                  )
                ),
                tags$div(
                  class = "collapse",
                  id = paste0("showcode_model_hyper_", ts_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("ts_inla_", ts_tabindex()),
                    " <- ",
                    ts_inla_call_print[[ts_output_name]],
                    tags$br(),
                    paste0("ts_inla_", ts_tabindex(), "$summary.hyperpar")
                  )
                )
              )
            ),
            box(
              id = paste0("ts_box_neffp_", ts_tabindex()),
              title = translate(
                "Expected Effective Number of Parameters in the Model",
                language = language_selected,
                words_one
              ),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              dataTableOutput(outputId = paste0("ts_neffp_", ts_tabindex())),
              tags$b(
                tags$a(
                  icon("code"),
                  translate("Show code", language = language_selected, words_one),
                  `data-toggle` = "collapse",
                  href = paste0("#showcode_neffp_", ts_tabindex())
                )
              ),
              tags$div(
                class = "collapse",
                id = paste0("showcode_neffp_", ts_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("ts_inla_", ts_tabindex()),
                  " <- ",
                  ts_inla_call_print[[ts_output_name]],
                  tags$br(),
                  paste0("ts_inla_", ts_tabindex(), "$neffp")
                )
              )
            ),
            conditionalPanel(
              condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
              box(
                id = paste0("ts_box_dic_waic_", ts_tabindex()),
                title = translate("DIC and WAIC", language = language_selected, words_one),
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dataTableOutput(outputId = paste0("ts_dic_waic_", ts_tabindex())),
                tags$b(
                  tags$a(
                    icon("code"),
                    translate("Show code", language = language_selected, words_one),
                    `data-toggle` = "collapse",
                    href = paste0("#showcode_dic_waic_", ts_tabindex())
                  )
                ),
                tags$div(
                  class = "collapse",
                  id = paste0("showcode_dic_waic_", ts_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("ts_inla_", ts_tabindex()),
                    " <- ",
                    ts_inla_call_print[[ts_output_name]],
                    tags$br(),
                    paste0("ts_inla_", ts_tabindex(), "$dic$dic"),
                    tags$br(),
                    paste0("ts_inla", ts_tabindex(), "$dic$dic.sat"),
                    tags$br(),
                    paste0("ts_inla", ts_tabindex(), "$dic$p.eff")
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
  output[[paste0("ts_call", ts_tabindex())]] <- renderText({
    ts_inla_call_print[[ts_output_name]]
  })
  
  # Download Script
  output[[paste0("download_script_", ts_tabindex())]] <-
    downloadHandler(
      filename = function() {
        paste0("model_", ts_tabindex(), "_script", ".r")
      },
      content = function(file) {
        write(paste(
          paste0("dat <- read.csv2(", input$file$datapath, ")"),
          paste0("inla_model_",
                 ts_tabindex(),
                 "<-",
                 ts_inla_call_print[[ts_output_name]]),
          sep = "\n"
        ), file)
      }
    )
  
  # Time Used
  output[[paste0("ts_time_used_", ts_tabindex())]] <-
    renderDataTable({
      data_time_used <- ts_inla[[ts_output_name]][["cpu.used"]] %>%
        t() %>%
        as.data.frame(row.names = c("Time")) %>%
        round(digits = 5)
      
      DT::datatable(data = data_time_used,
                    options = list(dom = "t",
                                   pageLength = 5))
    })
  
  # Fixed Effects
  output[[paste0("ts_fix_effects_", ts_tabindex())]] <-
    renderDataTable({
      ts_inla[[ts_output_name]][["summary.fixed"]] %>%
        round(digits = 5)
    },
    options = list(paging = FALSE,
                   dom = "t"))
  
  # Download Summary
  output[[paste0("download_summary_", ts_tabindex())]] <-
    downloadHandler(
      filename = function() {
        paste0("model_", ts_tabindex(), "summary.csv")
      },
      content = function(file) {
        write.csv2(as.data.frame(ts_inla[[ts_output_name]]$summary.fixed), file = file)
      }
    )
  
  # Model Hyper
  output[[paste0("ts_model_hyper_", ts_tabindex())]] <-
    renderDataTable({
      ts_inla[[ts_output_name]][["summary.hyperpar"]] %>%
        round(digits = 5)
    },
    options = list(dom = "t",
                   paging = FALSE))
  
  # Others (neffp)
  output[[paste0("ts_neffp_", ts_tabindex())]] <-
    renderDataTable({
      ts_neffp_dataframe <-
        ts_inla[[ts_output_name]][["neffp"]] %>%
        round(digits = 5)
      colnames(ts_neffp_dataframe) <- "Expected Value"
      ts_neffp_dataframe
    },
    options = list(dom = "t",
                   paging = FALSE))
  
  # Devicance Information Criterion (DIC)
  output[[paste0("ts_dic_waic_", ts_tabindex())]] <-
    renderDataTable({
      data.frame(
        "DIC" = ts_inla[[ts_output_name]][["dic"]][["dic"]],
        "DIC Saturated" = ts_inla[[ts_output_name]][["dic"]][["dic.sat"]],
        "Effective number of parameters (DIC)" = ts_inla[[ts_output_name]][["dic"]][["p.eff"]],
        "WAIC" = ts_inla[[ts_output_name]][["waic"]][["waic"]],
        "Effective number of parameters (WAIC)" = ts_inla[[ts_output_name]][["waic"]][["p.eff"]],
        row.names = "Expected Value"
      ) %>%
        round(digits = 5) %>%
        t()
    },
    options = list(dom = "t",
                   paging = FALSE))
  
  ts_tabindex(ts_tabindex() + 1)
})
