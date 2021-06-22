# Random Effects Module
random_effect_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(outputId = ns("RE_ui")),
    uiOutput(outputId = ns("new_covariates_here"))
  )
}

random_effect <- function(id, formula_data, model_choices) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      not_selected <- reactive({
        p <- formula_data$not_selected()
        return(p)
      })
      
      output$RE_ui <- renderUI({
        fluidRow(
          column(
            width = 4,
            pickerInput( # Select the first random effect
              inputId = ns("covariate_1"),
              label = paste0(translate("Random Effect", language = language_selected, dictionary = words_one), " 1"),
              choices = not_selected(),
              multiple = FALSE
            ),
            pickerInput(
              inputId = ns("model_1"),
              label = paste0(translate("Type of Random Effect", language = language_selected, dictionary = words_one), " 1"),
              choices = model_choices,
              multiple = FALSE
            ),
            tags$hr()
          ),
          column(
            width = 1,
            actionButton( # "+" to add more random effects
              inputId = ns("add_random_effect"),
              label = NULL,
              icon = icon("plus-circle"),
              style = "all:unset; color:black; cursor:pointer; outline:none; font-size: 25px;"
            ),
            tags$br(),
            tags$br()
          )
        )
      })
      
      remove_lines <- reactiveValues()
      observeEvent(input$add_random_effect, {
        RE_n <- input$add_random_effect + 1 # Counting the number of Random Effects (RE)
        insertUI(
          selector = paste0("#", ns("new_covariates_here")),
          where = "beforeEnd",
          ui = tags$div(
            id = paste0("random_effect_", RE_n),
            fluidRow(
              column(
                width = 4,
                pickerInput(
                  inputId = ns(paste0("covariate_", RE_n)),
                  label = paste0(translate("Random Effect", language = language_selected, dictionary = words_one), " ", RE_n),
                  choices = not_selected(),
                  multiple = FALSE
                ),
                pickerInput(
                  inputId = ns(paste0("model_", RE_n)),
                  label = paste0(translate("Type of Random Effect", language = language_selected, dictionary = words_one), " ", RE_n),
                  choices = model_choices,
                  multiple = FALSE
                ),
                tags$hr()
              ),
              column(
                width = 1,
                actionButton(
                  inputId = ns(paste0("remove_random_effect_", RE_n)),
                  label = NULL,
                  icon = icon("minus-circle"),
                  style = "all:unset; color:black; cursor:pointer; outline:none; font-size: 25px"
                )
              )
            )
          )
        )

        observeEvent(input[[paste0("remove_random_effect_", RE_n)]], {
          shiny::removeUI(
            selector = paste0("#random_effect_", RE_n)
          )
          remove_lines$n <- c(remove_lines$n, RE_n)
        })
      })
      
      #update covariate choices
      
      
      RE_return <- reactive({
        RE_n <- input$add_random_effect + 1
        aux_cov <- NULL
        aux_model <- NULL
        for(i in 1:RE_n){
          aux_cov[i] <- input[[ paste0("covariate_", i) ]]
          aux_model[i] <- input[[ paste0("model_", i) ]]
        }
        if(!is.null(remove_lines$n)){
          aux_cov <- aux_cov[-remove_lines$n]
          aux_model <- aux_model[-remove_lines$n]
        }
        aux <- data.frame(aux_cov, aux_model)
        names(aux) <- c("cov","model")
        return(aux)
      })
      
      return(RE_return)
      
    }
  )
}


# Testing the Module inside a Modal

ui <- fluidPage(
  actionButton(inputId = "open_modal", "Open Modal")
)

server <- function(input, output, session) {
  observeEvent(input$open_modal, {
    showModal(
      modalDialog(
        title = "Testing Random Effects Module",
        footer = tagList(actionButton(inputId = "browser_stop", "Ok"), modalButton(label = "Cancel")),
        size = "l",
        easyClose = FALSE,
        fade = FALSE,
        random_effect_ui(id = "test")
      )
    )
  })

  observeEvent(input$browser_stop, {
    browser()
  })

  result_test <- random_effect(id = "test", formula_data = list(not_selected = reactive({c('X1','X2','X3')})), model_choices = c("iid", "rm"))
}

shinyApp(ui, server)
