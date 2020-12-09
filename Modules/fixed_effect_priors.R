#Module for select fixed priors "betas"
fixed_effects_priors_ui <- function(id){
  ns <- NS(id)
  column(
    width = 5, 
         style = "margin-left: padding-top:50px",
    column(
      width = 4,
      h4("Mean"),
      uiOutput(ns("left_column_ui"))
    ),
    column(
      width = 5,
      offset = 1,
      h4("Precision"),
      uiOutput((ns("right_column_ui")))
    )
    )
}

fixed_effects_priors <- function(id, resp_variables, intercept){
  moduleServer(
    id,
    function(input, output, session){
      number_variables <- reactive({
        length(resp_variables) + intercept
      })
      
      
      output$left_column_ui <- renderUI({
        ns <- session$ns
        browser()
        lapply(1:number_variables(), function(number){
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns(paste0("mean_", ifelse(number == 1 && intercept == 1, "intercept", resp_variables[number-intercept]))),
                label = ifelse(number == 1 && intercept == 1, "intercept", resp_variables[number-intercept]),
                value = 0,
                width = '100%'
              )
            )
          )

        })
      })
      
      output$right_column_ui <- renderUI({
        ns <- session$ns
        lapply(1:number_variables(), function(number){
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns(paste0("prec_", ifelse(number == 1 && intercept == 1, "intercept", resp_variables[number-intercept]))),
                label = ifelse(number == 1 && intercept == 1, "intercept", resp_variables[number-intercept]),
                value = ifelse(number == 1 && intercept == 1, 0, 0.001),
                width = '100%'
              )
            )
          )
        })
      })
      
      #Return prioris
      priors <- reactive({
        aux <- matrix(NA_real_, nrow = number_variables, ncol = 2)
        for(i in 1:number_variables()){
          aux[i,1] <- input[[ paste0("mean_", ifelse(number == 1 && intercept == 1, "intercept", resp_variables[number-intercept]))]]
          aux[i,2] <- input[[ paste0("prec_", ifelse(number == 1 && intercept == 1, "intercept", resp_variables[number-intercept]))]]
        }
        return(aux)
      })
      return(priors)
    }
  )
}

#Test ----
# ui <- fluidPage(
#   fixed_effects_priors_ui("teste")
# )
# 
# server <- function(input, output, session){
#   testando <- fixed_effects_priors("teste", c("X1", "X2"), 1)
# }
# 
# shinyApp(ui, server)