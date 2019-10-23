library(INLA)
library(shiny)

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
      fluidRow(column(4, textOutput("value")))
    )
  )
)

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
    
    #Function that return the input of control.fixed()
    control.fixed.input <- function(prioris, formula){
      n <- nrow(prioris) #Number of variables + 1 (intercept)
      formula.terms <- labels(terms(formula)) #Getting the names of variables
      mean.prioris <- list() #Creating the list for the prioris means
      prec.prioris <- list() #Creating the list for the prioris precisions
      for(i in 2:n){ #Puting the terms inside the list
        mean.prioris[[i-1]] <- ifelse(is.na(prioris[i,1]), inla.set.control.fixed.default()$mean, prioris[i,1])
        prec.prioris[[i-1]] <- ifelse(is.na(prioris[i,2]), inla.set.control.fixed.default()$prec, prioris[i,2])
      }
      names(mean.prioris) <- formula.terms #Putting names on the list
      names(prec.prioris) <- formula.terms
      return(list(mean.intercept = ifelse(is.na(prioris[1,1]), inla.set.control.fixed.default()$mean.intercept, prioris[1,1]), #Organizing intercept's priori
                  prec.intercept = ifelse(is.na(prioris[1,2]), inla.set.control.fixed.default()$prec.intercept, prioris[1,2]),
                  mean = mean.prioris,
                  prec = prec.prioris))
    }
    
    
    list(data = indata, n.variables = n.variables,
         n.obs = n.obs,
         infile.path = infile$datapah,
         incontrol.fixed = )
  })
  
  

  
  output$value<- renderText({
    model.inla <- inla(formula(data.input()$data), data = data.input()$data, control.fixed = )
  })
}

shinyApp(ui, server)

