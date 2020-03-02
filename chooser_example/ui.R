shinyUI(
  fluidPage(theme = "www/styles.css",
            column(width = 6, offset = 1, 
                   chooserInput(inputId = "chooserid", 
                                leftLabel = "Variáveis", 
                                # rightLabel = "oi", 
                                rightLabel1 = "Variável resposta",
                                rightLabel2 = "Covariáveis",
                                leftChoices = paste0("X", 1:10),
                                # rightChoices = c(),
                                rightChoices1 = c(),
                                rightChoices2 = c(),
                                multiple = TRUE
                   )
            ),
            verbatimTextOutput("selection")
  )
)