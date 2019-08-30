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
  data.input <- eventReactive(input$file, {
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T)
    n.variables <- ncol(indata)
    n.obs <- nrow(indata)
    list(data = indata, n.variables = n.variables, n.obs = n.obs, infile.path = infile$datapah)
  })
  
  
  output$ui1 <- renderUI({
    if(is.null(data.input()$n.variables))
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("mean", number), label = paste0("mean", number), width = "100%"))
      )
    })
  })
  
  output$ui2 <- renderUI({
    if(is.null(data.input()$n.variables))
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("prec", number), label = paste0("prec", number)))
      )
    })
  })
  
  output$value<- renderText({as.character(data.input()$infile.pah)})
}

shinyApp(ui, server)

