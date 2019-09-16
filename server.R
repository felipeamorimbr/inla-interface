server <- function(input, output){
  #Modal dialog of input file
  file_modal <- modalDialog(
    title = "Carregando os dados",
    fade = FALSE,
    fileInput("file", label = h3("Selecione o Arquivo com os dados")),
    dataTableOutput("datafile"),
    size = "l"
  )
  showModal(file_modal)
  
  #Output table of modal dialog input file
  output$datafile <- renderDataTable({
    data.input()$data
  }, options = list(
    searching = FALSE,
    pageLength = 5
  ))
  
  #The input of prioris of betas (mean)
  output$ui1 <- renderUI({ #Generate the input boxes for the mean according to the number of columns of input file
    if(is.null(data.input()$n.variables)) 
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("mean", number), label = paste0("mean", data.input()$names.variables[number] )))
      )
    })
  })
  
  #The input of prioris of betas (precision)
  output$ui2 <- renderUI({ #Generate the input boxes for the precision according to the number of columns of input file
    if(is.null(data.input()$n.variables))
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("prec", number),
                           label = paste0("prec", data.input()$names.variables[number] ),
                           placeholder = "Precisão deve ser maior que zero"))
      )
    })
  })
  
  #Data from input
  data.input <- eventReactive(input$file, { 
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T)
    names.variables <- names(model.matrix(formula(indata), data = indata)[1,])
    n.variables <- length(names.variables)
    n.obs <- nrow(indata)
    name.file <- infile$name

    list(data = indata, #The data from data file
         n.variables = n.variables, 
         n.obs = n.obs,
         infile.path = infile$datapah,
         names.variables = names.variables,
         name.file = name.file)
  })
  
  #Prioris of the Beta's
  priori.input <- eventReactive(input$mean1, { #Organizing the priori matrix
    prioris <- matrix(NA, nrow = data.input()$n.variables, ncol = 2)
    for(i in 1:data.input()$n.variables){
      prioris[i,1] <- input[[ paste0("mean",i) ]]
      prioris[i,2] <- input[[ paste0("prec",i) ]]
    }
    list(prioris = prioris)
  })
  
  #Result from inla
  lm.inla <- eventReactive(input$goButton , {
    inla(formula = formula(data.input()$data),
         data = data.input()$data,
         control.fixed = control.fixed.input(prioris = priori.input()$prioris, 
                                             v.names = data.input()$names.variables))
  })
  
  #summary.fixed from inla
  output$result.INLA <- renderTable({
    input$goButton
  lm.inla()$summary.fixed
  })
  
  #The code used to make the model (need to fix the formula and the control fixed input)
  output$code.INLA <- renderText({
    input$goButton
    isolate(paste0("inla(data = ", data.input()$name.file, "control.fixed = ", 
                   list.call(control.fixed.input(prioris = priori.input()$prioris, 
                                                 v.names = data.input()$names.variables) )))
  })
}