random_effects_with_fix_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(outputId = ns("random_effects_here"))
  )
             
}

#Model choices need to be a list
random_effects_with_fix <- function(id, formula_data, model_choice, number_random_effects, random_effect_label){
  moduleServer(
    id,
    function(input, output, session){
      output$random_effects_here <- renderUI({
        ns <- session$ns
        
        not_selected <- reactive({
          p <- formula_data$not_selected()
          return(p)
        })
        
        list_ui <- NULL
          for(i in 1:number_random_effects){
            list_ui[[i]] <- fluidRow(
              column(width = 4,
                     pickerInput(
                       inputId = ns(paste0("covariate_", i)),
                       label = paste0(translate("Select the covariate to the Random Effect ", language_selected, words_one),
                                      random_effect_label[i]),
                       choices = not_selected(),
                       multiple = FALSE
                     ),
                     pickerInput(
                       inputId = ns(paste0("model_",i)),
                       label = paste0(translate("Select the model to the Random Effect ", language_selected, words_one),
                                      random_effect_label[i]),
                       choices = model_choice[[i]],
                       multiple = FALSE
                     ))
            )
          }
        list_ui
      })
      
      RE_return <- reactive({
        aux_cov <- NULL
        aux_model <- NULL
        for(i in 1:number_random_effects){
          aux_cov[i] <- input[[ paste0("covariate_", i) ]]
          aux_model[i] <- input[[ paste0("model_", i) ]]
        }
        
        aux <- data.frame(aux_cov, aux_model)
        names(aux) <- c("cov","model")
        return(aux)
      })
      
      return(RE_return)
    }
  )
}

#Testing
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
#         random_effects_with_fix_ui(id = "test")
#       )
#     )
#   })
# 
#   observeEvent(input$browser_stop, {
#     browser()
#   })
# 
#   result_test <- random_effects_with_fix(id = "test",
#                                          covariates = c("X1", "X2", "X3"),
#                                          model_choice = list(c("iid", "linear"), c("iid", "linear")),
#                                          number_random_effects = 2,
#                                          random_effect_label = c("A", "B"))
#                                         
# }
# 
# shinyApp(ui, server)
