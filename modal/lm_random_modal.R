# Random effecs Linear Model
observeEvent(data_input(), {
  RE_lm_data <<- list()

  RE_lm_data$formula <<- list(
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
  RE_lm_data$fixed_priors <<- inla.set.control.fixed.default()
  RE_lm_data$hyper <<- inla.set.control.family.default()
  RE_lm_data$random_formula <<- NULL
  RE_lm_data$fixed_priors_tab <<- FALSE
  RE_lm_data$hyper_tab <<- FALSE
  RE_lm_data$RE_tab <<- FALSE
})

model_buttons$RE_lm <- smAction("RE_linear_action_btn", translate("Random Effect Linear Regression", language = language_selected, words_one))
model_boxes$RE_lm <- actionButton(
  inputId = "RE_lm_box_btn",
  box_model_ui(id = "RE_lm_box", name = translate("Random Effect Linear Model", language = language_selected, words_one), author = "Felipe Amorim", icon = "fa-chart-area", color = "#12a19b"),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)

observeEvent(c(input$RE_linear_action_btn, input$RE_lm_box_btn), {
  validate(need(sum(input$RE_linear_action_btn, input$RE_lm_box_btn) > 0, ""))

  RE_lm_data$formula <<- new_chooser(
    id = "RE_lm_formula",
    selected_right = RE_lm_data$formula$cov_var(),
    selected_left = RE_lm_data$formula$not_selected(),
    resp_var = RE_lm_data$formula$resp_var(),
    rightLabel = translate("Covariates Selected", language = language_selected, words_one),
    leftLabel = translate("Covariates", language = language_selected, words_one)
  )

  RE_lm_data$fixed_priors <<- fixed_effects_priors(
    id = "RE_lm_fixed",
    formula_data = RE_lm_data$formula
  )

  RE_lm_data$hyper <<- sel_hyper(
    id = "RE_lm_hyper",
    Link = FALSE,
    formula_data = RE_lm_data$formula,
    linkLabel = NULL
  )

  RE_lm_data$random_formula <<- random_effect(
    id = "RE_random_formula",
    formula_data = RE_lm_data$formula,
    model_choices = latent_effects
  )

  showModal(modalDialog(fluidPage(
    includeCSS(path = "modal/style_lm.css"),
    shinyjs::useShinyjs(),
    tabsetPanel(
      id = "RE_lm_tabs", type = "tabs",
      tabPanel(
        title = translate("Select Variables", language = language_selected, words_one),
        tags$br(),
        new_chooser_UI(
          id = "RE_lm_formula",
          respLabel = translate("Response", language = language_selected, words_one),
          resp_var = RE_lm_data$formula$resp_var(),
          selected_right = RE_lm_data$formula$cov_var(),
          selected_left = RE_lm_data$formula$not_selected(),
          familyLabel = translate("Family", language = language_selected, words_one),
          familyChoices = RE_lm_family,
          data = data_input()$data,
          resp_numeric = TRUE
        )
      ),
      tabPanel(
        title = translate("Fixed Effects", language = language_selected, words_one),
        tags$br(),
        fixed_effects_priors_ui(id = "RE_lm_fixed")
      ),
      tabPanel(
        title = translate("Hyperparameter Prior", language = language_selected, words_one),
        sel_hyper_ui(
          id = "RE_lm_hyper"
        )
      ),
      tabPanel(
        title = translate("Random Effect", language_selected, words_one),
        navlistPanel(
          id = "RE_random_formula_tabs", widths = c(3, 9), fluid = FALSE,
          tabPanel(
            title = translate("Formula", language = language_selected, words_one),
            random_effect_ui(id = "RE_random_formula")
          ),
          tabPanel(
            title = translate("Random Effects Priors", language = language_selected, words_one)
          )
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
  footer = tagList(actionButton(inputId = "RE_lm_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
})


observeEvent(input$RE_lm_tabs, {
  RE_lm_data$fixed_priors_tab <<- ifelse(input$RE_lm_tabs == translate("Fixed Effects", language = language_selected, words_one), TRUE, RE_lm_data$fixed_priors_tab)
  RE_lm_data$hyper_tab <<- ifelse(input$RE_lm_tabs == translate("Hyperparameter Prior", language = language_selected, words_one), TRUE, RE_lm_data$hyper_tab)
  RE_lm_data$RE_tab <<- ifelse(input$RE_lm_tabs == translate("Random Effect", language_selected, words_one), TRUE, RE_lm_data$RE_tab)
})

# observe({
#   shinyjs::useShinyjs()
#   covariates_aux <- c(RE_lm_data$formula$cov_var(), RE_lm_data$random_formula()[,1])
#   condition_aux <- !setequal(covariates_aux, unique(covariates_aux))
#   toggle(id = "RE_error_random_effect", condition = condition_aux)
#   toggleState(id = "RE_lm_ok", condition = condition_aux)
# })

RE_lm_tabindex <- reactiveVal(1)

observeEvent(input$RE_lm_ok, {
  useSweetAlert()
  covariates_aux <- c(RE_lm_data$formula$cov_var(), RE_lm_data$random_formula()$cov)
  condition_aux <- !identical(sort(covariates_aux), sort(unique(covariates_aux)))
  if (condition_aux) {
    sendSweetAlert(
      session = session,
      title = translate("ERROR", language = language_selected, words_one),
      type = "error",
      text = translate("ERROR: The same covariate was select more the once. If that's the intention create a column for each time that will be used and try again.", language = language_selected, words_one),
      closeOnClickOutside = FALSE,
      showCloseButton = TRUE
    )
    return()
  }

  if (RE_lm_data$RE_tab == FALSE) {
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
  browser()
  RE_lm_formula <- paste0(
    RE_lm_data$formula$resp_var(), " ~ ",
    paste0(RE_lm_data$formula$cov_var(), collapse = " + "), " + ",
    paste0(" f(", RE_lm_data$random_formula()$cov, ", model = '", RE_lm_data$random_formula()$model, "') ", collapse = "+"),
    ifelse(lm_data$formula$intercept(), " + 1", " - 1")
  )

  RE_lm_inla <- list()
  RE_lm_inla_call_print <- list()
  RE_lm_output_name <- paste("output_tab", RE_lm_tabindex(), sep = "_")
  if (RE_lm_data$fixed_priors_tab == FALSE) {
    RE_lm_control_fixed <- inla.set.control.fixed.default()
  } else {
    RE_lm_control_fixed <- control_fixed_input(
      prioris = RE_lm_data$fixed_priors(),
      v.names = RE_lm_data$formula$cov_var(),
      intercept = RE_lm_data$formula$intercept()
    )
  }
  if (RE_lm_data$hyper_tab == FALSE) {
    RE_lm_control_family <- inla.set.control.family.default()
  } else {
    RE_lm_control_family <- RE_lm_data$hyper$control_family_input()
  }

  RE_lm_inla[[RE_lm_output_name]] <- try(inla(
    formula = as.formula(RE_lm_formula),
    data = hot_to_r(input$data),
    family = RE_lm_data$formula$family(),
    control.fixed = RE_lm_control_fixed,
    control.compute = control_compute_input,
    control.inla = control_inla_input,
    control.family = RE_lm_control_family
  ), silent = TRUE)
  if (class(RE_lm_inla[[RE_lm_output_name]]) == "try-error") {
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
    removeModal()

    RE_lm_inla_call_print[[RE_lm_output_name]] <- paste0(
      "inla(data = ", "dat",
      ", formula = ", '"', RE_lm_data$formula$resp_var(),
      " ~ ", ifelse(RE_lm_data$formula$intercept(), ifelse(is.null(RE_lm_data$formula$cov_var()), "+1", ""), "-1 + "), paste0(RE_lm_data$formula$cov_var(), collapse = " + "), '"',
      paste0(", family = ", '"', RE_lm_data$formula$family(), '"'),
      ifelse(RE_lm_data$fixed_priors_tab == FALSE, "", paste0(
        ", control.fixed = ",
        list_call(RE_lm_control_fixed)
      )),
      ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
        paste0(", control.compute = ", list_call(control_compute_input), ", control.inla = ", list_call(control_inla_input))
      ),
      ifelse(RE_lm_data$hyper_tab == FALSE, "", paste0(", control.family = ", list_call(RE_lm_control_family))),
      ")"
    )
  }

  appendTab(
    inputId = "mytabs", select = TRUE,
    tabPanel(
      title = paste0(translate("Random Effect Linear Model", language = language_selected, words_one), " ", RE_lm_tabindex()),
      useShinydashboard(),
      useShinyjs(),
      fluidRow(
        column(
          width = 6,
          box(
            id = paste0("RE_lm_box_call_", RE_lm_tabindex()),
            title = translate("Call", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textOutput(outputId = paste0("RE_lm_call", RE_lm_tabindex())),
            tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_call", RE_lm_tabindex()))),
            tags$div(
              class = "collapse", id = paste0("showcode_call", RE_lm_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("RE_lm_inla_", RE_lm_tabindex()), " <- ", RE_lm_inla_call_print[[RE_lm_output_name]],
                tags$br(),
                paste0("RE_lm_inla_", RE_lm_tabindex(), "$call")
              )
            ),
            footer = downloadBttn(
              outputId = paste0("download_script_", RE_lm_tabindex()),
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
            id = paste0("RE_lm_box_time_used", RE_lm_tabindex()),
            title = translate("Time Used", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput(outputId = paste0("RE_lm_time_used_", RE_lm_tabindex())),
            tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_time", RE_lm_tabindex()))),
            tags$div(
              class = "collapse", id = paste0("showcode_time", RE_lm_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("RE_lm_inla_", RE_lm_tabindex()), " <- ", RE_lm_inla_call_print[[RE_lm_output_name]],
                tags$br(),
                paste0("RE_lm_inla_", RE_lm_tabindex(), "$cpu.sued")
              )
            )
          ),
          dropdownButton(
            icon = icon("download"),
            up = TRUE,
            status = "myclass",
            inputId = paste0("download_rdata_", RE_lm_tabindex()),
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
            id = paste0("RE_lm_box_fix_effects_", RE_lm_tabindex()),
            title = translate("Fixed Effects", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput(outputId = paste0("RE_lm_fix_effects_", RE_lm_tabindex())),
            tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", RE_lm_tabindex()))),
            tags$div(
              class = "collapse", id = paste0("showcode_fix_effects_", RE_lm_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("RE_lm_inla_", RE_lm_tabindex()), " <- ", RE_lm_inla_call_print[[RE_lm_output_name]],
                tags$br(),
                paste0("RE_lm_inla_", RE_lm_tabindex(), "$summary.fixed")
              )
            ),
            footer = downloadBttn(
              outputId = paste0("download_summary_", RE_lm_tabindex()),
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
                id = paste0("RE_lm_box_model_hyper_", RE_lm_tabindex()),
                title = translate("Model Hyperparameters", language = language_selected, words_one),
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dataTableOutput(outputId = paste0("RE_lm_model_hyper_", RE_lm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", RE_lm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_model_hyper_", RE_lm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("RE_lm_inla_", RE_lm_tabindex()), " <- ", RE_lm_inla_call_print[[RE_lm_output_name]],
                    tags$br(),
                    paste0("RE_lm_inla_", RE_lm_tabindex(), "$summary.hyperpar")
                  )
                )
              )
            ),
            box(
              id = paste0("RE_lm_box_neffp_", RE_lm_tabindex()),
              title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              dataTableOutput(outputId = paste0("RE_lm_neffp_", RE_lm_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", RE_lm_tabindex()))),
              tags$div(
                class = "collapse", id = paste0("showcode_neffp_", RE_lm_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("RE_lm_inla_", RE_lm_tabindex()), " <- ", RE_lm_inla_call_print[[RE_lm_output_name]],
                  tags$br(),
                  paste0("RE_lm_inla_", RE_lm_tabindex(), "$neffp")
                )
              )
            ),
            conditionalPanel(
              condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
              box(
                id = paste0("RE_lm_box_dic_waic_", RE_lm_tabindex()),
                title = translate("DIC and WAIC", language = language_selected, words_one),
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dataTableOutput(outputId = paste0("RE_lm_dic_waic_", RE_lm_tabindex())),
                tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", RE_lm_tabindex()))),
                tags$div(
                  class = "collapse", id = paste0("showcode_dic_waic_", RE_lm_tabindex()),
                  tags$code(
                    class = "language-r",
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("RE_lm_inla_", RE_lm_tabindex()), " <- ", RE_lm_inla_call_print[[RE_lm_output_name]],
                    tags$br(),
                    paste0("RE_lm_inla_", RE_lm_tabindex(), "$dic$dic"),
                    tags$br(),
                    paste0("RE_lm_inla", RE_lm_tabindex(), "$dic$dic.sat"),
                    tags$br(),
                    paste0("RE_lm_inla", RE_lm_tabindex(), "$dic$p.eff")
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
  output[[paste0("RE_lm_call", RE_lm_tabindex())]] <- renderText({
    RE_lm_inla_call_print[[RE_lm_output_name]]
  })

  # Download Script
  output[[paste0("download_script_", RE_lm_tabindex())]] <- downloadHandler(
    filename = function() {
      paste0("model_", RE_lm_tabindex(), "_script", ".r")
    },
    content = function(file) {
      write(paste(paste0("dat <- read.csv2(", input$file$datapath, ")"),
        paste0("inla_model_", RE_lm_tabindex(), "<-", RE_lm_inla_call_print[[RE_lm_output_name]]),
        sep = "\n"
      ), file)
    }
  )

  # Time Used
  output[[paste0("RE_lm_time_used_", RE_lm_tabindex())]] <- renderDataTable({
    data_time_used <- RE_lm_inla[[RE_lm_output_name]][["cpu.used"]] %>%
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
  output[[paste0("RE_lm_fix_effects_", RE_lm_tabindex())]] <- renderDataTable(
    {
      RE_lm_inla[[RE_lm_output_name]][["summary.fixed"]] %>%
        round(digits = 5)
    },
    options = list(
      paging = FALSE,
      dom = "t"
    )
  )

  # Download Summary
  output[[paste0("download_summary_", RE_lm_tabindex())]] <- downloadHandler(
    filename = function() {
      paste0("model_", RE_lm_tabindex(), "summary.csv")
    },
    content = function(file) {
      write.csv2(as.data.frame(RE_lm_inla[[RE_lm_output_name]]$summary.fixed), file = file)
    }
  )

  # Model Hyper
  output[[paste0("RE_lm_model_hyper_", RE_lm_tabindex())]] <- renderDataTable(
    {
      RE_lm_inla[[RE_lm_output_name]][["summary.hyperpar"]] %>%
        round(digits = 5)
    },
    options = list(
      dom = "t",
      paging = FALSE
    )
  )

  # Others (neffp)
  output[[paste0("RE_lm_neffp_", RE_lm_tabindex())]] <- renderDataTable(
    {
      RE_lm_neffp_dataframe <- RE_lm_inla[[RE_lm_output_name]][["neffp"]] %>%
        round(digits = 5)
      colnames(RE_lm_neffp_dataframe) <- "Expected Value"
      RE_lm_neffp_dataframe
    },
    options = list(
      dom = "t",
      paging = FALSE
    )
  )

  # Devicance Information Criterion (DIC)
  output[[paste0("RE_lm_dic_waic_", RE_lm_tabindex())]] <- renderDataTable(
    {
      data.frame(
        "DIC" = RE_lm_inla[[RE_lm_output_name]][["dic"]][["dic"]],
        "DIC Saturated" = RE_lm_inla[[RE_lm_output_name]][["dic"]][["dic.sat"]],
        "Effective number of parameters (DIC)" = RE_lm_inla[[RE_lm_output_name]][["dic"]][["p.eff"]],
        "WAIC" = RE_lm_inla[[RE_lm_output_name]][["waic"]][["waic"]],
        "Effective number of parameters (WAIC)" = RE_lm_inla[[RE_lm_output_name]][["waic"]][["p.eff"]],
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

  RE_lm_tabindex(RE_lm_tabindex() + 1)
})
