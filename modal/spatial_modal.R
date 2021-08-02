#Module for spatial models
observeEvent(data_input(),{
  spatial_RE_data <<- list()
  spatial_RE_data$formula <<- list(
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
  
  spatial_RE_data$fixed_priors <<- inla.set.control.fixed.default()
  spatial_RE_data$hyper <<- inla.set.control.family.default()
  spatial_RE_data$random_formula <<- NULL
  spatial_RE_data$fixed_priors_tab <<- FALSE
  spatial_RE_data$hyper_tab <<- FALSE
  spatial_RE_data$RE_tab <<- FALSE
})

model_buttons$spatial_RE <- smAction("spatial_RE_action_btn", translate("Spatial Model for Areal Data", language = language_selected, words_one))
model_boxes$spatial_RE <- actionButton(
  inputId = "spatial_RE_box_btn",
  box_model_ui(id = "spatial_RE_box", name = translate("Spatial Model for Areal Data", language = language_selected, words_one), author = "Felipe Amorim", icon = "fa-chart-area", color = "#12a19b"),
  style = "all:unset; color:black; cursor:pointer; outline:none;"
)

observeEvent(c(input$spatial_RE_action_btn, input$spatial_RE_box_btn), {
  validate(need(sum(input$spatial_RE_action_btn, input$spatial_RE_box_btn) > 0, ""))
  
  spatial_RE_data$formula <<- new_chooser(
    id = "spatial_RE_formula",
    selected_right = spatial_RE_data$formula$cov_var(),
    selected_left = spatial_RE_data$formula$not_selected(),
    resp_var = spatial_RE_data$formula$resp_var(),
    rightLabel = translate("Covariates Selected", language = language_selected, words_one),
    leftLabel = translate("Covariates", language = language_selected, words_one)
  )
  
  spatial_RE_data$fixed_priors <<- fixed_effects_priors(
    id = "spatial_RE_fixed",
    formula_data = spatial_RE_data$formula
  )
  
  spatial_RE_data$hyper <<- sel_hyper(
    id = "spatial_RE_hyper",
    Link = FALSE,
    formula_data = spatial_RE_data$formula,
    linkLabel = NULL
  )
  
  spatial_RE_data$random_formula <<- random_effects_with_fix(
    id = "spatial_RE_random_formula", 
    formula_data = spatial_RE_data$formula,
    model_choice = list(spatial_models),
    number_random_effects = 1,
    random_effect_label = c("Spatial")
  )
  
  showModal(modalDialog(fluidPage(
    includeCSS(path = "modal/style_lm.css"),
    shinyjs::useShinyjs(),
    tabsetPanel(
      id = "spatial_RE_tabs", type = "tabs",
      tabPanel(
        title = translate("Select Variables", language = language_selected, words_one),
        tags$br(),
        new_chooser_UI(
          id = "spatial_RE_formula",
          respLabel = translate("Response", language = language_selected, words_one),
          resp_var = spatial_RE_data$formula$resp_var(),
          selected_right = spatial_RE_data$formula$cov_var(),
          selected_left = spatial_RE_data$formula$not_selected(),
          familyLabel = translate("Family", language = language_selected, words_one),
          familyChoices = spatial_RE_family,
          data = data_input()$data,
          resp_numeric = TRUE
        )
      ),
      tabPanel(
        title = translate("Fixed Effects", language = language_selected, words_one),
        tags$br(),
        fixed_effects_priors_ui(id = "spatial_RE_fixed")
      ),
      tabPanel(
        title = translate("Hyperparameter Prior", language = language_selected, words_one),
        sel_hyper_ui(
          id = "spatial_RE_hyper"
        )
      ),
      tabPanel(
        title = translate("Random Effect", language_selected, words_one),
        navlistPanel(
          id = "RE_random_formula_tabs", widths = c(3, 9), fluid = FALSE,
          tabPanel(
            title = translate("Formula", language = language_selected, words_one),
            random_effects_with_fix_ui(id = "spatial_RE_random_formula")
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
  footer = tagList(actionButton(inputId = "spatial_RE_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
})

observeEvent(input$spatial_RE_tabs, {
  spatial_RE_data$fixed_priors_tab <<- ifelse(input$spatial_RE_tabs == translate("Fixed Effects", language = language_selected, words_one), TRUE, spatial_RE_data$fixed_priors_tab)
  spatial_RE_data$hyper_tab <<- ifelse(input$spatial_RE_tabs == translate("Hyperparameter Prior", language = language_selected, words_one), TRUE, spatial_RE_data$hyper_tab)
  spatial_RE_data$RE_tab <<- ifelse(input$spatial_RE_tabs == translate("Random Effect", language_selected, words_one), TRUE, spatial_RE_data$RE_tab)
})

spatial_RE_tabindex <- reactiveVal(1)

observeEvent(input$spatial_RE_ok, {
  useSweetAlert()
  
  if (spatial_RE_data$RE_tab == FALSE) {
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
  
  covariates_aux <- c(spatial_RE_data$formula$cov_var(), spatial_RE_data$random_formula()$cov)
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
  
  spatial_RE_formula <- paste0(
    spatial_RE_data$formula$resp_var(), " ~ ",
    paste0(spatial_RE_data$formula$cov_var(), collapse = " + "), " + ",
    paste0(" f(", spatial_RE_data$random_formula()$cov, ", model = '", spatial_RE_data$random_formula()$model, "', graph = '", data_input()$adj,"') "),
    ifelse(spatial_RE_data$formula$intercept(), " + 1", " - 1")
  )
  
  spatial_RE_inla <- list()
  spatial_RE_inla_call_print <- list()
  spatial_RE_output_name <- paste("output_tab", spatial_RE_tabindex(), sep = "_")
  if (spatial_RE_data$fixed_priors_tab == FALSE) {
    spatial_RE_control_fixed <- inla.set.control.fixed.default()
  } else {
    spatial_RE_control_fixed <- control_fixed_input(
      prioris = spatial_RE_data$fixed_priors(),
      v.names = spatial_RE_data$formula$cov_var(),
      intercept = spatial_RE_data$formula$intercept()
    )
  }
  if (spatial_RE_data$hyper_tab == FALSE) {
    spatial_RE_control_family <- inla.set.control.family.default()
  } else {
    spatial_RE_control_family <- spatial_RE_data$hyper$control_family_input()
  }
  spatial_RE_inla[[spatial_RE_output_name]] <- try(inla(
    formula = as.formula(spatial_RE_formula),
    data = hot_to_r(input$data),
    family = spatial_RE_data$formula$family(),
    control.fixed = spatial_RE_control_fixed,
    control.compute = control_compute_input,
    control.inla = control_inla_input,
    control.family = spatial_RE_control_family
  ), silent = TRUE)
  if (class(spatial_RE_inla[[spatial_RE_output_name]]) == "try-error") {
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
    spatial_RE_inla_call_print[[spatial_RE_output_name]] <- paste0(
      "inla(data = ", "dat",
      ", formula = ", '"', spatial_RE_data$formula$resp_var(),
      " ~ ", ifelse(spatial_RE_data$formula$intercept(), ifelse(is.null(spatial_RE_data$formula$cov_var()), "+1", ""), "-1 + "), paste0(paste0(spatial_RE_data$formula$cov_var(), collapse = " + "), " + ",
                                                                                                                              paste0(" f(", spatial_RE_data$random_formula()$cov, ", model = '", spatial_RE_data$random_formula()$model, "graph = ", data_input()$adj, "') ")), '"',
      paste0(", family = ", '"', spatial_RE_data$formula$family(), '"'),
      ifelse(spatial_RE_data$fixed_priors_tab == FALSE, "", paste0(
        ", control.fixed = ",
        list_call(spatial_RE_control_fixed)
      )),
      ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
             paste0(", control.compute = ", list_call(control_compute_input), ", control.inla = ", list_call(control_inla_input))
      ),
      ifelse(spatial_RE_data$hyper_tab == FALSE, "", paste0(", control.family = ", list_call(spatial_RE_control_family))),
      ")"
    )
  
  
  appendTab(
    inputId = "mytabs", select = TRUE,
    tabPanel(
      title = paste0(translate("Spatial Model for Areal Data", language = language_selected, words_one), " ", spatial_RE_tabindex()),
      useShinydashboard(),
      useShinyjs(),
      results_UI(id = spatial_RE_output_name, moduleID = "spatial_RE", INLAresult = spatial_RE_inla[[spatial_RE_output_name]],
                 control_compute = control_compute_input, control_inla = control_inla_input,
                 inla_call_print = spatial_RE_inla_call_print[[spatial_RE_output_name]], tab_index = spatial_RE_tabindex, data_input = data_input)
    )
  )
  
  results_server(id = spatial_RE_output_name, moduleID = "spatial_RE", INLAresult = spatial_RE_inla[[spatial_RE_output_name]],
                 control_compute = control_compute_input, control_inla = control_inla_input,
                 inla_call_print = spatial_RE_inla_call_print[[spatial_RE_output_name]], tab_index = spatial_RE_tabindex, data_input = data_input)
  spatial_RE_tabindex(spatial_RE_tabindex() + 1)
  }
})