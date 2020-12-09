# Modal for Linear Model
observeEvent(input$linear_action_btn_2, {
  showModal(modalDialog(
    tabsetPanel(
      id = "lm_tabs", type = "tabs",
      tabPanel(
        title = "Select Variables",
        sel_formula_ui(id = "lm_formula")
      ),
      tabPanel(
        title = "Fixed Effects",
        uiOutput("lm_fixed_ui")
        # fixed_effects_priors_ui(id = "lm_fixed")
      ),
      tabPanel(
        title = "Family and Hyperpriors",
        sel_family_ui(
          id = "lm_family",
          familyLabel = "Select Family",
          familyChoices = lm_family
        )
      )
    ),
    title = "Linear Model",
    size = "l",
    fade = FALSE,
    footer = tagList(actionButton(inputId = "lm_ok", label = "Ok"), modalButton(label = "Cancel"))
  ))
})



lm_formula_data <- sel_formula(
  id = "lm_formula",
  variables = data_input()$covariates,
  resp_label = "Select Response Variable",
  left_label = "Variables to Choose",
  right_label = "Variables Selected"
)



lm_fixed_priors_data <- reactive({
  p <- fixed_effects_priors(
    id = "lm_fixed",
    resp_variables = lm_formula_data$cov_var,
    intercept = lm_formula_data$intercept
  )
  return(p)
})

lm_control_family <- sel_family(id = "lm_family", Link = FALSE)
lm_tabindex <- reactiveVal(0)
observeEvent(input$lm_ok, {
  browser()
})