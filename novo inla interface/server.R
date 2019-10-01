server <- function(input, output){
  #Modal Dialog File Input ----
  #Modal Dialog ----
  file_modal <- modalDialog(
    useShinyjs(),
    title = "Carregando os dados",
    fade = FALSE,
    size = "l",
    footer = tagList(disabled(actionButton("open_file", "Abrir")),
                     modalButton("Cancelar")),
    fluidPage(
      splitLayout(
        fileInput("file", label = h3("Selecione o Arquivo com os dados")),
        dataTableOutput("datafile")
      )
    )
  )
  
  #Modal Observe Events ----
  observeEvent(input$file_action_btn, {
    showModal(file_modal)
  })
  
  observeEvent(input$open_file, {
    removeModal()
  })
  
  observeEvent(input$file, {
    if(!is.null(input$file))
      shinyjs::enable("open_file")
  })
  
  #Modal Table output ----
  output$datafile <- renderDataTable({
    data.input()$data
  }, options = list(
    searching = FALSE,
    pageLength = 5
  ))

  #Data ----
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
  
  #Main Panel ----
  #Table with Data ----
  observeEvent(input$open_file, {
    output$data <- renderRHandsontable({
      rhandsontable(data.input()$data, digits = 10) %>%
        hot_cols(format = "0.0000")
    })
  })
  
  #Modal Dialog Linear Model ----
  #Modal Dialog ----
  linear_model_modal <- modalDialog(
    title = "Regressão Linear",
    fade = FALSE,
    size = "l",
    footer = tagList(modalButton("Cancelar"),
                     actionButton("lm_ok", "Ok")),
    navlistPanel(
      id = "linear_panel",
      selected = "Selecione Varíaveis",
      tabPanel(title = "Selecione Varíaveis",
               fluidRow(uiOutput("uiResponse"),
                        uiOutput("uiCovariates"))
      ),
      tabPanel(title = "Prioris",
               fluidRow(column(4, uiOutput("uiPrioriMean")),
                        column(4, uiOutput("uiPrioriPrec"))
               )
      )
    )
  )
  
  #Modal Render UI Response ----
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
  
  #Modal Render UI Covariates ----
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
  
  observeEvent(input$linear_action_btn,{
    showModal(linear_model_modal)
  })
  
  inla.formula <- eventReactive(c(input$responseVariable, input$covariates), {
    f.covariates <- paste0(input$covariates, collapse = "+")
    f.response <- paste0(input$responseVariable)
    as.formula(paste0(f.response,rawToChar(as.raw(126)) , f.covariates))
  })
  
  tabindex <- reactiveVal(0)
  observeEvent(input$lm_ok, {
    removeModal()
    lm_inla <- inla(formula = inla.formula(),
                    data = data.input()$data)
    
    tabindex(tabindex() + 1)
    appendTab(inputId = "mytabs", select = TRUE,
              tabPanel(title = paste0("Modelo", tabindex()), 
                       fluidRow(column(4, "Resultado",  tableOutput("result.INLA")))))
  })
  
  lm_inla <- eventReactive(input$lm_ok, {
    inla(formula = inla.formula(),
         data = data.input()$data)
  })
  
  output$result.INLA <- renderTable({
    lm_inla()$summary.fixed
  })
}