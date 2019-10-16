server <- function(input, output){
  #Modal Dialog of Input File ----
  
  #Modal ----
  file_modal <- modalDialog(
    useShinyjs(),
    title = "Carregando os dados",
    fade = FALSE,
    size = "l",
    footer = disabled(actionButton("ok", "OK")),
    fluidPage(
      splitLayout(
        fileInput("file", label = h3("Selecione o Arquivo com os dados")),
        dataTableOutput("datafile")
        )
      )
  )
  
  showModal(file_modal)
  
  #Modal Ok Button ----
  observeEvent(input$ok, {
      removeModal()
  })
  
  observeEvent(input$file, {
    if(!is.null(input$file))
      shinyjs::enable("ok")
  })
  
  
  #Modal Table ----
  output$datafile <- renderDataTable({
    data.input()$data
  }, options = list(
    searching = FALSE,
    pageLength = 5
  ))
  
  #Modal data input file ----
  data.input <- eventReactive(input$file, { 
    infile <- input$file
    indata <- read.csv2(infile$datapath, header = T)
    names.variables <- names(model.matrix(formula(indata), data = indata)[1,])
    covariates <- names(indata)
    n.variables <- length(names.variables)
    n.obs <- nrow(indata)
    name.file <- infile$name
    
    list(data = indata, #The data from data file
         n.variables = n.variables, 
         n.obs = n.obs,
         infile.path = infile$datapah,
         names.variables = names.variables,
         name.file = name.file,
         covariates = covariates)
  })
  
  #Side Bar Panel----
  
  #Side Options Response Variable ----
  output$uiResponse <- renderUI({
    if(is.null(data.input()$n.variables))
      return()
    radioGroupButtons(
      inputId = "responseVariable",
      label = "Selecione a varíavel resposta",
      choices = data.input()$covariates,
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok",
                   lib = "glyphicon")
      )
    )
  })
  
  #Side Options Covariates Variable ----
  output$uiCovariates <- renderUI({
    if(is.null(data.input()$n.variables))
      return()
    checkboxGroupButtons(
      inputId = "covariates",
      label = "Selecione as covariáveis",
      choices = data.input()$covariates[data.input()$covariates != input$responseVariable],
      selected = data.input()$covariates[data.input()$covariates != input$responseVariable],
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok",
                   lib = "glyphicon")
      )
    )
  })
  
  #Side Boxes Mean Priori ----
  output$uiPrioriMean <- renderUI({ #Generate the input boxes for the mean 
    if(is.null(data.input()$n.variables)) # according to the number of columns of input file
      return()
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,textInput(inputId = paste0("mean", number),
                           label = paste0("mean", data.input()$names.variables[number] )))
      )
    })
  })
  
  #Side Boxes Precision Priori ----
  output$uiPrioriPrec <- renderUI({ #Generate the input boxes for the precision according to the number of columns of input file
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
  
  #Side Data Formula ----
  inla.formula <- eventReactive(c(input$responseVariable, input$covariates), {
    f.covariates <- paste0(input$covariates, collapse = "+")
    f.response <- paste0(input$responseVariable)
    as.formula(paste0(f.response,rawToChar(as.raw(126)) , f.covariates))
  })
  
  #Side Data Prioris ----
  priori.input <- eventReactive(input$mean1, { #Organizing the priori matrix
    prioris <- matrix(NA, nrow = data.input()$n.variables, ncol = 2)
    for(i in 1:data.input()$n.variables){
      prioris[i,1] <- input[[ paste0("mean",i) ]]
      prioris[i,2] <- input[[ paste0("prec",i) ]]
    }
    list(prioris = prioris)
  })
  
  #Inla Model ----
  #Result from inla
  lm.inla <- eventReactive(input$goButton , {
    inla(formula = inla.formula(),
         data = data.input()$data,
         control.fixed = control_fixed_input(prioris = priori.input()$prioris, 
                                             v.names = data.input()$names.variables))
  })
  
  #Main Panel ----
  
  #Main summary from Inla ----
  output$result.INLA <- renderTable({
    input$goButton
    isolate(lm.inla()$summary.fixed)
  })
  

  # Main Code to make Inla----
  output$code.INLA <- renderText({
    input$goButton
    isolate(paste0("inla(data = data, ", "formula =" , input$responseVariable,
           "~ ", paste0(input$covariates, collapse = " + "), ", control.fixed = ",
           list.call(control_fixed_input(prioris = priori.input()$prioris, 
                                         v.names = data.input()$names.variables)),
           ")"))
  })
}