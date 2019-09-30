server <- function(input, output){
  
  observeEvent(input$file_action_btn, {
    showModal(
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
    )
  })
  
  observeEvent(input$ok, {
    removeModal()
  })
  
  observeEvent(input$file, {
    if(!is.null(input$file))
      shinyjs::enable("ok")
  })
  
  output$datafile <- renderDataTable({
    data.input()$data
  }, options = list(
    searching = FALSE,
    pageLength = 5
  ))
  
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
  
  output$data <- renderRHandsontable({
      rhandsontable(data.input()$data, digits = 10) %>%
      hot_cols(format = "0.0000")
  })
}