source("new_chooser.R")
#Module for select response variable
sel_formula_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(outputId = ns("ui_cov_var"))
}

sel_formula <- function(id, variables, resp_label, left_label, right_label, familyLabel, familyChoices){ #Return the selected response variable
  moduleServer(
    id,
    function(input,output, session){
      
      output$ui_cov_var <- renderUI({
        ns <- session$ns
        new_chooser_UI(id = ns("cov_var"),
                       respLabel = resp_label,
                       selected_left = variables, 
                       selected_right = NULL,
                       familyLabel,
                       familyChoices
                        )
      })
      
      selected_formula <- new_chooser(id = "cov_var",
                    selected_left = variables,
                    selected_right = NULL,
                    leftLabel = left_label,
                    rightLabel = right_label)
      
      
      return(selected_formula)
    }
  )
}

# Test ----
# dados <- read.csv2("https://raw.githubusercontent.com/felipeamorimbr/inla-interface/master/data/ex-database.csv")
# load("data/lm_modal_words.RData")
# language_selected <- "en"
# 
# ui <- fluidPage(
#   sel_formula_ui(
#     id = "lm_formula"
#   ),
#   actionButton("test", "ok")
# )
# 
# server <- function(input, output, session){
#   imprimir <- sel_formula(
#     id = "lm_formula",
#     variables = row.names(USArrests),
#     resp_label = "Response Variable",
#     left_label = "Variables to Choose",
#     right_label = "Selected Variable",
#     familyLabel = "Family",
#     familyChoices = c("t", "gaussian")
#   )
#   observeEvent(input$test, {
#     browser()
#   })
# }
# 
# shinyApp(ui, server)
# 
