# Time Series Modal

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
              pickerInput(
                # Select the order of AR model
                inputId = "order",
                label = translate("Order", language = language_selected, dictionary = words_one),
                choices = 1:25,
                multiple = FALSE
              ),

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

 
# Quando define os prametros e clica no OK
observeEvent(input$ts_ok, {
  useSweetAlert()

  if (ts_data$random_formula()$model == "ar" && input$order > 5) {
    sendSweetAlert(
      session = session,
      title = translate("WARNING", language = language_selected, words_one),
      type = "warning ",
      text = translate("Some errors may be generated due to the chosen order for the autoregressive model.", language = language_selected, words_one),
      closeOnClickOutside = FALSE,
      showCloseButton = TRUE
    )
  }
  
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
  #print(ts_formula)
  
  
  ts_inla <- list()
  ts_inla_call_print <- list()
  ts_output_name <- paste("ts_output_tab", ts_tabindex(), sep = "_")
  
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
    # UI of the result tab
    appendTab(
      inputId = "mytabs",
      select = TRUE,
      tabPanel(
        title = paste0(
          translate("Time Series Model", language = language_selected, words_one),
          " ",
          ts_tabindex()
        ),
        useShinydashboard(),
        useShinyjs(),
        results_UI(
          id = ts_output_name,
          moduleID = "ts",
          INLAresult = ts_inla[[ts_output_name]],
          control_compute = control_compute_input,
          control_inla = control_inla_input,
          inla_call_print = ts_inla_call_print[[ts_output_name]],
          tab_index = ts_tabindex,
          data_input = data_input
        )
      )
    )
    results_server(
      id = ts_output_name,
      moduleID = "ts",
      INLAresult = ts_inla[[ts_output_name]],
      control_compute = control_compute_input,
      control_inla = control_inla_input,
      inla_call_print = ts_inla_call_print[[ts_output_name]],
      tab_index = ts_tabindex,
      data_input = data_input
    )
    ts_tabindex(ts_tabindex() + 1)
  }
})
