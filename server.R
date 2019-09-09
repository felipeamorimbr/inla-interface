server <- function(input, output){
  in.formula <- eventReactive(data.input, {
    
  })
  
  output$ui1 <- renderUI({
    if(is.null(data.input()$n.variables))
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("mean", number), label = paste0("mean", number)))
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
  
  data.input <- eventReactive(input$file, {
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T)
    n.variables <- ncol(indata)
    n.obs <- nrow(indata)
    list(data = indata, n.variables = n.variables,
         n.obs = n.obs,
         infile.path = infile$datapah,
         incontrol.fixed = )
  })
}