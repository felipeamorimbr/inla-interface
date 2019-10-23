server <- function(input, output){
  ##-- Modal Dialog File Input ----
  ##-- Modal Dialog ----
  file_modal <- modalDialog(
    useShinyjs(),
    title = "Carregando os dados",
    fade = FALSE,
    size = "l",
    footer = tagList(shinyjs::disabled(actionButton("open_file", "Abrir")),
                     modalButton("Cancelar")),
    fluidPage(
      fluidRow(
        column(width = 4,
               fileInput("file", label = h4("Selecione o Arquivo com os dados")),
               shinyjs::hidden(actionLink(inputId = "file_adv_options", label = "Opções Avançadas")),
               textOutput(outputId = "file_error_extension"),
               shinyjs::hidden(uiOutput("file_options"))
        ),
        column(width = 8, 
               DT::dataTableOutput(outputId = "datafile")
        )
      )
    )
  )
  
  ##-- Modal Observe Events ----
  observeEvent(input$file_action_btn, {
    showModal(file_modal)
  })
  
  observeEvent(input$open_file, {
    removeModal()
  })
  
  observeEvent(input$file, {
    if(!is.null(input$file) & (file_ext(input$file$datapath) %in% accetable_formats)){
      shinyjs::enable("open_file")
      shinyjs::show("file_adv_options")
    }
    if(!(file_ext(input$file$datapath) %in% accetable_formats))
      output$file_error_extension <- renderText("Extensão Inválida")
  })
  
  ##-- Modal File Options
  observeEvent(input$file_adv_options, {
    output$file_options <- renderUI({
      switch(file_ext(input$file$datapath),
             "txt" = ,
             "csv" = fluidPage(
               fluidRow(
                 checkboxInput(inputId = "csv_header",
                               label = "Cabeçalho",
                               value = TRUE)
               ),
               fluidRow(
                 textInput(inputId = "csv_sep",
                           label = "Separador",
                           value = ";",
                           width = "20%")
               ),
               fluidRow(
                 textInput(inputId = "csv_quote",
                           label = "Quote",
                           value = "\"",
                           width = "20%")
               ),
               fluidRow(
                 textInput(inputId = "csv_dec",
                           label = "Decimal",
                           value = ",",
                           width = "20%")
               )
             )
      )
    })
  })
  
  ##-- Modal Table output ----
  make_dt <- reactive({
    data_teste <- data.input()$data
    cols_numeric <- which(sapply(data_teste, class) == "numeric")
    data_teste <- DT::datatable(data_teste, 
                                options = list(searching = FALSE, 
                                               pageLength = 5,
                                               lengthMenu = c(5, 10)))
    
    data_teste <- DT::formatRound(table = data_teste, columns = cols_numeric, digits = 4)
    
    return(data_teste)
  })
  
  output$datafile <- DT::renderDataTable(make_dt())
  
  observeEvent(input$file_adv_options,{
    shinyjs::toggle("file_options", anim = TRUE)
  })
  
  #Modal Dialog Options ----
  options_modal <- modalDialog(
    title = "Opções",
    fade = FALSE,
    size = "l",
    footer = tagList(actionButton("ok_btn_options_modal", "Ok"),
                     modalButton("Cancelar")),
    fluidPage(
      fluidRow(selectInput(inputId = "ccompute_input_1",
                           label = "Estratégia Computacional",
                           choices = list("Pequena" = "small",
                                          "Média" = "medium",
                                          "Grande" = "large",
                                          "Imenso" = "huge",
                                          "Padrão" = "default"),
                           selected = "default",
                           multiple = FALSE,
                           width = '30%')),
      fluidRow(checkboxInput(inputId = "ccompute_input_2",
                             label = "Calcular a Marginal dos Hiperparâmetros",
                             value = TRUE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_3",
                             label = "Retornar as marginais do campo latente",
                             value = TRUE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_4",
                             label = "Calcular o valor-DIC",
                             value = FALSE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_5",
                             label = "Calcular as marginais da Verssimilhança",
                             value = TRUE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_6",
                             label = "Calcular o CPO",
                             value = FALSE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_7",
                             label = "Calcular a preditive ordinate",
                             value = FALSE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_8",
                             label = "Calcular o WAIC",
                             value = FALSE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_9",
                             label = "Gerar as imagens da matriz de precição, matriz de precição reordenada
                             e o triangulo de Cholesky",
                             value = FALSE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_10",
                             label = "Guardar as aproximações do Gaussian Markov Random Field",
                             value = FALSE)),
      fluidRow(selectInput(inputId = "ccompute_input_11",
                           label = "Estratégia para resolver a matriz esparça",
                           choices = list("Taucs" = "taucs",
                                          "Band" = "band",
                                          "Pardiso" = "pardiso",
                                          "Padrão" = "default"),
                           selected = inla.getOption("smtp"),
                           multiple = FALSE,
                           width = '30%')),
      fluidRow(checkboxInput(inputId = "ccompute_input_12",
                             label = "Retornar os Gráficos",
                             value = TRUE)),
      fluidRow(checkboxInput(inputId = "ccompute_input_13",
                             label = "Retornar as densidades Gaussianas",
                             value = FALSE))
    )
  )
  
  control_compute_input <- eventReactive(input$ok_btn_options_modal, {
    list(openmp.strategy = input$ccompute_input_1,
         hyperpar = input$ccompute_input_2,
         return.marginals = input$ccompute_input_3,
         dic = input$ccompute_input_4, 
         mlik = input$ccompute_input_5,
         cpo = input$ccompute_input_6,
         po = input$ccompute_input_7,
         waic = input$ccompute_input_8,
         q = input$ccompute_input_9,
         config = input$ccompute_input_10,
         smtp = input$ccompute_input_11,
         graph = input$ccompute_input_12,
         gdensity = input$ccompute_input_13)
  
  })
  
  observeEvent(input$ok_btn_options_modal, {
    removeModal()
  })
  
  observeEvent(input$options_action_btn, {
    showModal(options_modal)
  })
  
  ##-- Data ----
  data.input <- eventReactive(c(input$file,
                                input$csv_header, input$csv_quote, input$csv_dec
                                ), {
    if(!(file_ext(input$file$datapath) %in% accetable_formats)){
      return(NULL)
    } else {
      indata <- switch(file_ext(input$file$datapath),
                       "txt" =,
                       "csv" = read.table(input$file$datapath,
                                          header = ifelse(is.null(input$csv_header), TRUE, input$csv_header),
                                          sep = ifelse(is.null(input$csv_sep), ";", input$csv_sep),
                                          quote = ifelse(is.null(input$csv_quote), "\"", input$csv_quote),
                                          dec = ifelse(is.null(input$csv_quote), ",", input$csv_quote))
      )
      names.variables <- names(model.matrix(formula(indata), data = indata)[1,])
      covariates <- names(indata)
      names.variables <- c(names.variables[1], covariates[1], names.variables[-1])
      n.variables <- length(names.variables)
      n.obs <- nrow(indata)
      name.file <- input$file$name
      
      list(data = indata, # The data from data file
           n.variables = n.variables, 
           n.obs = n.obs,
           infile.path = input$file$datapath,
           names.variables = names.variables,
           name.file = name.file,
           covariates = covariates)
    }
  })
  
  ##-- Main Panel ----
  ##-- Table with Data ----
  observeEvent(input$open_file, {
    output$data <- renderRHandsontable({
      rhandsontable(data.input()$data, digits = 4, height = 800, stretchH = "all") %>%
        hot_cols(format = "0.0000")
    })
  })
  
  ##-- Modal Dialog Linear Model ----
  ##-- Modal Dialog ----
  linear_model_modal <- modalDialog(
    title = "Regressão Linear",
    fade = FALSE,
    size = "l",
    footer = tagList(actionButton("lm_ok", "Ok") 
                     ,modalButton("Cancelar")),
    tabsetPanel(
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
  
  ##-- Modal Render UI Response ----
  output$uiResponse <- renderUI({
    if(is.null(data.input()$n.variables))
      return()
    radioGroupButtons(
      inputId = "responseVariable",
      label = "Selecione a varíavel resposta",
      choices = data.input()$covariates,
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon")
      )
    )
  })
  
  ##-- Modal Render UI Covariates ----
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
    if(is.null(data.input()$n.variables)) return() # according to the number of columns of input file
    
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6,numericInput(inputId = paste0("mean", number),
                           label = paste0("mean", data.input()$names.variables[number] ),
                           value = character(0)))
      )
    })
  })
  
  output$uiPrioriPrec <- renderUI({ #Generate the input boxes for the precision according to the number of columns of input file
    if(is.null(data.input()$n.variables)) return()
    
    Rows <- lapply(1:data.input()$n.variables, function(number){
      fluidRow(
        column(6, numericInput(inputId = paste0("prec", number),
                            label = paste0("prec", data.input()$names.variables[number] ),
                            value = character(0)))
      )
    })
  })
  
  observeEvent(input$linear_action_btn, {
    showModal(linear_model_modal)
  })
  
  inla.formula <- eventReactive(c(input$responseVariable, input$covariates), {
    f.covariates <- paste0(input$covariates, collapse = "+")
    f.response <- paste0(input$responseVariable)
    as.formula(paste0(f.response, rawToChar(as.raw(126)), f.covariates))
  })
  
  tabindex <- reactiveVal(0)
  observeEvent(input$lm_ok, {
    removeModal()
    
    tabindex(tabindex() + 1)
    output_name <- paste("output_tab", tabindex(), sep = "_")
    
    lm_inla <- list()
    lm_inla_call_print <- list()
    prioris <- matrix(NA_real_, nrow = data.input()$n.variables, ncol = 2)
    for(i in 1:data.input()$n.variables){
      prioris[i,1] <- ifelse("prec1" %in% names(input), input[[ paste0("mean",i) ]], NA_real_)
      prioris[i,2] <- ifelse("prec1" %in% names(input), input[[ paste0("prec",i) ]], NA_real_)
    }
    if(identical(paste0(input$ok_btn_options_modal), character(0))){
      lm_control_compute <- inla.set.control.compute.default()
    }else{
      lm_control_compute <- control_compute_input()
    }

    lm_inla[[output_name]] <- inla(formula = inla.formula(),     ## Atualizando o escopo global
                                   data = hot_to_r(input$data),
                                   control.fixed = control_fixed_input(prioris = prioris,
                                                                       v.names = data.input()$names.variables),
                                   control.compute = lm_control_compute
                                   )
    lm_inla_call_print[[output_name]] <- paste0("inla(data = ", data.input()$name.file,
                                                ", formula = ", input$responseVariable,
                                                " ~ ", paste0(input$covariates, collapse = " + "),
                                                ifelse(all(is.na(prioris)), "",paste0(", control.fixed = ",
                                                       list_call(control_fixed_input(prioris = prioris,
                                                                                     v.names = data.input()$names.variables))))
                                                ,
                                                ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
                                                       paste0(", control.compe = ", list_call(control_compute_input()))),")"
    )
    output[[output_name]] <- renderPrint({
      lm_inla[[output_name]]$call <- lm_inla_call_print[[output_name]]
      summary(lm_inla[[output_name]]) ## Da pra jogar o teu Call aqui dentro
      
    }, width = 200)

    appendTab(inputId = "mytabs", select = TRUE, 
              tabPanel(title = paste0("Modelo", tabindex()), 
                       fluidRow(column(4, 
                                       "Resultado", 
                                       # tags$data(lm_inla[[tabindex()]]$summary.fixed)
                                       verbatimTextOutput(outputId = output_name)
                       )
                       )
              )
    )
  })
}