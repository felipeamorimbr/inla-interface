ui <- fluidPage(
  titlePanel(title = "INLA"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(uiOutput("uiResponse"),
               uiOutput("uiCovariates")),
      fluidRow(column(4, uiOutput("uiPrioriMean")),
               column(4, uiOutput("uiPrioriPrec"))
      ),
      actionButton("goButton", "Go !")
    ),
    mainPanel(
      fluidRow(column(4, "Código", code(textOutput("code.INLA"))),
               column(4, "Resultado",  tableOutput("result.INLA")))
    )
  )
)