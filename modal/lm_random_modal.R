#Random effecs Linear Model
observeEvent(data_input(), {
  RE_lm_data <<- list()
  
  RE_lm_data$formula <<- list(
    resp_var = reactive({data_input()$covariates[1]}),
    cov_var = reactive({NULL}),
    not_selected = reactive({data_input()$covariates}),
    intercept = reactive({TRUE}),
    family = reactive({"Gaussian"})
  )
  RE_lm_data$fixed_priors <<- inla.set.control.fixed.default()
  RE_lm_data$hyper <<- inla.set.control.family.default()
  RE_lm_random_formula <<- NULL
  RE_lm_data$fixed_priors_tab <<- FALSE
  RE_lm_data$hyper_tab <<- FALSE
  
  
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
    cov_var = lm_data$formula$cov_var(),
    intercept = lm_data$formula$intercept()
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
        random_effect_ui(id = "RE_random_formula", 
                         covariates = c(RE_lm_data$formula$cov_var(), RE_lm_data$formula$not_selected()),
                         model_choices = c("linear", "iid"))
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
  lm_data$fixed_priors <<- fixed_effects_priors(
    id = "RE_lm_fixed",
    cov_var = RE_lm_data$formula$cov_var(),
    intercept = RE_lm_data$formula$intercept()
  )
  
  RE_lm_data$hyper <<- sel_hyper(id = "RE_lm_hyper",
                              Link = FALSE,
                              sel_family = RE_lm_data$formula$family(),
                              linkLabel = NULL)
  
  
})

RE_lm_random_formula <<- random_effect(id = "RE_random_formula", 
                                       covariates = c(RE_lm_data$formula$cov_var(), RE_lm_data$formula$not_selected()),
                                       model_choices = c("linear", "iid"))

observeEvent(input$RE_lm_tabs,{
  RE_lm_data$fixed_priors_tab <<- ifelse(input$RE_lm_tabs == translate("Fixed Effects", language = language_selected, words_one), TRUE, RE_lm_data$fixed_priors_tab )
  RE_lm_data$hyper_tab <<- ifelse(input$RE_lm_tabs == translate("Hyperparameter Prior", language = language_selected, words_one), TRUE, RE_lm_data$hyper_tab)
})

observeEvent(input$RE_lm_ok, {
  browser()
})