# Random Effects Module
random_effect_ui <- function(id, covariates, model_choices) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 4,
        pickerInput( # Select the first random effect
          inputId = ns("covariate_1"),
          label = paste0(translate("Random Effect", language = language_selected, dictionary = words_one), " 1"),
          choices = covariates,
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
    ),
    uiOutput(outputId = ns("new_covariates_here"))
  )
}

random_effect <- function(id, covariates, model_choices) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
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
                  choices = covariates,
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
      
      RE_return <- reactive({
        RE_n <- input$add_random_effect + 1
        aux <- matrix(NA_character_, nrow = RE_n, ncol = 2)
        for(i in 1:RE_n){
          aux[i,1] <- input[[ paste0("covariate_", i) ]]
          aux[i,2] <- input[[ paste0("model_", i) ]]
        }
        browser()
        aux <- aux[-remove_lines$n,]
        return(aux)
      })
      
      return(RE_return)
      
    }
    

  )
}


# Testing the Module inside a Modal

# ui <- fluidPage(
#   actionButton(inputId = "open_modal", "Open Modal")
# )
# 
# server <- function(input, output, session) {
#   observeEvent(input$open_modal, {
#     showModal(
#       modalDialog(
#         title = "Testing Random Effects Module",
#         footer = tagList(actionButton(inputId = "browser_stop", "Ok"), modalButton(label = "Cancel")),
#         size = "l",
#         easyClose = FALSE,
#         fade = FALSE,
#         random_effect_ui(id = "test", covariates = c("X1", "X2", "X3", "X4"), model_choices = c("iid", "rm"))
#       )
#     )
#   })
# 
#   observeEvent(input$browser_stop, {
#     browser()
#   })
# 
#   result_test <- random_effect(id = "test", covariates = c("X1", "X2", "X3", "X4"), model_choices = c("iid", "rm"))
# }
# 
# shinyApp(ui, server)
