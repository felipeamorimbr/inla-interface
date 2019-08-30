library(INLA)
library(shiny)

ui <- fluidPage(
  titlePanel(title = "INLA"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", label = h3("File input")),
      column(4, uiOutput("ui1")),
      column(4, uiOutput("ui2"))
    ),
    mainPanel(
      fluidRow(column(4, verbatimTextOutput("value")))
    )
  )
)

server <- function(input, output){
  output$value <- renderPrint({
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T)
  })
  
  output$ui1 <- renderUI({
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T) #Tem como não duplicar isso?
    n.variables <- ncol(indata)
    if(is.null(n.variables))
      return()
    Rows <- lapply(1:n.variables, function(number){
      fluidRow(
        column(4,textInput(inputId = paste0("mean", number), label = paste0("mean", number)))
      )
    })
    
  })
}

shinyApp(ui, server)
