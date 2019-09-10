ui <- fluidPage(
  titlePanel(title = "INLA"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", label = h3("File input")),
      column(4, uiOutput("ui1")),
      column(4, uiOutput("ui2")),
      actionButton("goButton", "Go !")
    ),
    mainPanel(
      fluidRow(column(4, "Código", code(textOutput("code.INLA"))),
               column(4, "Resultado",  tableOutput("result.INLA")))
    )
  )
)