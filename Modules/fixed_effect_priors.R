# Module for select fixed priors "betas"
fixed_effects_priors_ui <- function(id) {
  ns <- NS(id)
  column(
    width = 7,
    style = "margin-left: padding-top:50px",
    column(
      width = 5,
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

fixed_effects_priors <- function(id, formula_data) {
  moduleServer(
    id,
    function(input, output, session) {
      number_variables <- reactive({
        p.1 <- ifelse(is.null(cov_var()), 0, length(cov_var()))
        p.2 <- ifelse(is.null(intercept()), 0, intercept())
        return(p.1 + p.2)
      })

      cov_var <- reactive({
        p <- formula_data$cov_var()
        return(p)
      })

      intercept <- reactive({
        p <- formula_data$intercept()
        return(p)
      })

      output$left_column_ui <- renderUI({
        ns <- session$ns

        if (number_variables() == 0) {
          return()
        } else {
          lapply(1:number_variables(), function(number) {
            fluidRow(
              column(
                width = 6,
                numericInput(
                  inputId = ns(paste0("mean_", ifelse(number == 1 && intercept() == 1, "intercept", cov_var()[number - intercept()]))),
                  label = ifelse(number == 1 && intercept() == 1, "intercept", cov_var()[number - intercept()]),
                  value = 0,
                  width = "100%"
                )
              )
            )
          })
        }
      })

      output$right_column_ui <- renderUI({
        ns <- session$ns
        if (number_variables() == 0) {
          return(NULL)
        }
        lapply(1:number_variables(), function(number) {
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns(paste0("prec_", ifelse(number == 1 && intercept() == 1, "intercept", cov_var()[number - intercept()]))),
                label = ifelse(number == 1 && intercept() == 1, "intercept", cov_var()[number - intercept()]),
                value = ifelse(number == 1 && intercept() == 1, 0, 0.001),
                width = "100%"
              )
            )
          )
        })
      })

      # Return prioris
      priors <- reactive({
        if (number_variables() == 0) {
          return(NULL)
        }
        aux <- matrix(NA_real_, nrow = number_variables(), ncol = 2)
        for (i in 1:number_variables()) {
          aux[i, 1] <- input[[paste0("mean_", ifelse(i == 1 && intercept() == 1, "intercept", cov_var()[i - intercept()]))]]
          aux[i, 2] <- input[[paste0("prec_", ifelse(i == 1 && intercept() == 1, "intercept", cov_var()[i - intercept()]))]]
        }
        return(aux)
      })
      return(priors)
    }
  )
}

# Test ----
# ui <- fluidPage(
#   fixed_effects_priors_ui("teste"),
#   actionButton("browser", "ok")
# )
#
# server <- function(input, output, session){
#   testando <- fixed_effects_priors("teste", NULL, FALSE)
#   observeEvent(input$browser, {
#     browser()
#   })
# }
#
# shinyApp(ui, server)
