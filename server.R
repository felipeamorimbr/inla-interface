server <- function(input, output){
  
  output$ui1 <- renderUI({ #Generate the input boxes for the mean according to the number of columns of input file
    if(is.null(data.input()$n.variables)) 
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("mean", number), label = paste0("mean", data.input()$names.variables[number] )))
      )
    })
  })
  
  output$ui2 <- renderUI({ #Generate the input boxes for the precision according to the number of columns of input file
    if(is.null(data.input()$n.variables))
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("prec", number), label = paste0("prec", data.input()$names.variables[number] )))
      )
    })
  })
  
  data.input <- eventReactive(input$file, { #Organizing the input from input$file
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T)
    names.variables <- names(model.matrix(formula(indata), data = indata)[1,])
    n.variables <- length(names.variables)
    n.obs <- nrow(indata)
    
    list(data = indata,
         n.variables = n.variables,
         n.obs = n.obs,
         infile.path = infile$datapah,
         names.variables = names.variables)
  })
  
  priori.input <- eventReactive(input$mean1, { #Organizing the priori matrix
    prioris <- matrix(NA, nrow = data.input()$n.variables, ncol = 2)
    for(i in 1:data.input()$n.variables){
      prioris[i,1] <- input[[ paste0("mean",i) ]]
      prioris[i,2] <- input[[ paste0("prec",i) ]]
    }
    list(prioris = prioris)
  })
  
  output$value <- renderText({
  
  })
  
}