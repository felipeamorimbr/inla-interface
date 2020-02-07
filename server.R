server <- function(input, output, session) {
  ## -- Modal Dialog File Input ----
  ## -- Modal Dialog ----
  file_modal <- modalDialog(
    useShinyjs(),
    title = "Carregando os dados",
    fade = FALSE,
    size = "l",
    footer = tagList(
      shinyjs::disabled(actionButton("file_load_btn", "Abrir")),
      modalButton("Cancelar")
    ),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          fileInput("file", label = h4("Selecione o Arquivo com os dados")),
          shinyjs::hidden(actionLink(inputId = "file_adv_options_btn", label = "Opções Avançadas")),
          textOutput(outputId = "file_error_extension_txt"),
          shinyjs::hidden(uiOutput("file_adv_options_ui"))
        ),
        column(
          width = 8,
          DT::dataTableOutput(outputId = "file_datatable")
        )
      )
    )
  )

  ## -- Modal Observe Events ----
  observeEvent(input$file_action_btn, {
    showModal(file_modal)
  })

  observeEvent(input$file_load_btn, {
    removeModal()
  })

  observeEvent(input$file, {
    if (!is.null(input$file) && (file_ext(input$file$datapath) %in% accetable_formats)) {
      shinyjs::enable("file_load_btn")
      shinyjs::show("file_adv_options_btn")
    }
    if (!(file_ext(input$file$datapath) %in% accetable_formats)) {
      output$file_error_extension_txt <- renderText("Extensão Inválida")
    }
  })

  ## -- Modal File Options
  observeEvent(input$file, {
    output$file_adv_options_ui <- renderUI({
      switch(file_ext(input$file$datapath),
        "txt" = ,
        "csv" = fluidPage(
          fluidRow(
            checkboxInput(
              inputId = "csv_header",
              label = "Cabeçalho",
              value = TRUE
            )
          ),
          fluidRow(
            textInput(
              inputId = "csv_sep",
              label = "Separador",
              value = ";",
              width = "20%"
            )
          ),
          fluidRow(
            textInput(
              inputId = "csv_quote",
              label = "Quote",
              value = "\"",
              width = "20%"
            )
          ),
          fluidRow(
            textInput(
              inputId = "csv_dec",
              label = "Decimal",
              value = ",",
              width = "20%"
            )
          )
        )
      )
    })
  })

  ## -- Modal Table output ----
  make_dt <- reactive({
    data_teste <- data_input()$data
    cols_numeric <- which(sapply(data_teste, class) == "numeric")
    data_teste <- DT::datatable(data_teste,
      options = list(
        searching = FALSE,
        pageLength = 5,
        lengthMenu = c(5, 10)
      )
    )

    data_teste <- DT::formatRound(table = data_teste, columns = cols_numeric, digits = 4)

    return(data_teste)
  })

  output$file_datatable <- DT::renderDataTable(make_dt())

  observeEvent(input$file_adv_options_btn, {
    shinyjs::toggle("file_adv_options_ui", anim = TRUE)
  })


  observeEvent(input$ok_btn_options_modal, {
    # Update control_compute_input in global envioronment
    rlang::env_bind(.env = globalenv(), control_compute_input = list(
      openmp.strategy = input$ccompute_input_1,
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
      gdensity = input$ccompute_input_13
    ))

    # uptdate control_inla_input in global enviroment
    rlang::env_bind(.env = globalenv(), control_inla_input = list(
      strategy = input$cinla_input_1,
      int.strategy = input$cinla_input_2,
      fast = input$cinla_input_5
    ))

    removeModal()
  })

  observeEvent(input$options_action_btn, {
    options_modal <- modalDialog( ## -- DOUGLAS: Criando o modal aqui dentro. Desta forma ele vai ser recriado toda vez que clicar em opções
      title = "Opções",
      fade = FALSE,
      size = "l",
      footer = tagList(
        actionButton("ok_btn_options_modal", "Ok"),
        modalButton("Cancelar")
      ),
      fluidPage(
        tabsetPanel(
          tabPanel(
            "Control Compute",
            fluidRow(selectInput(
              inputId = "ccompute_input_1",
              label = "Estratégia Computacional",
              choices = list(
                "Pequena" = "small",
                "Média" = "medium",
                "Grande" = "large",
                "Imenso" = "huge",
                "Padrão" = "default"
              ),
              selected = control_compute_input[[1]],
              multiple = FALSE,
              width = "30%"
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_2",
              label = "Calcular a Marginal dos Hiperparâmetros",
              value = control_compute_input[[2]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_3",
              label = "Retornar as marginais do campo latente",
              value = control_compute_input[[3]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_4",
              label = "Calcular o valor-DIC",
              value = globalenv()$control_compute_input[[4]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_5",
              label = "Calcular as marginais da Verssimilhança",
              value = control_compute_input[[5]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_6",
              label = "Calcular o CPO",
              value = control_compute_input[[6]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_7",
              label = "Calcular a preditive ordinate",
              value = control_compute_input[[7]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_8",
              label = "Calcular o WAIC",
              value = control_compute_input[[8]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_9",
              label = "Gerar as imagens da matriz de precição, matriz de precição reordenada
                               e o triangulo de Cholesky",
              value = control_compute_input[[9]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_10",
              label = "Guardar as aproximações do Gaussian Markov Random Field",
              value = control_compute_input[[10]]
            )),
            fluidRow(selectInput(
              inputId = "ccompute_input_11",
              label = "Estratégia para resolver a matriz esparça",
              choices = list(
                "Taucs" = "taucs",
                "Band" = "band",
                "Pardiso" = "pardiso",
                "Padrão" = "default"
              ),
              selected = control_compute_input[[11]],
              multiple = FALSE,
              width = "30%"
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_12",
              label = "Retornar os Gráficos",
              value = control_compute_input[[12]]
            )),
            fluidRow(checkboxInput(
              inputId = "ccompute_input_13",
              label = "Retornar as densidades Gaussianas",
              value = control_compute_input[[13]]
            ))
          ),
          tabPanel(
            "Control INLA",
            fluidRow(selectInput(
              inputId = "cinla_input_1",
              label = "Estatégia de aproximação",
              choices = list(
                "Guassiana" = "gaussian",
                "Laplace Simplificada" = "simplified.laplace",
                "Laplace" = "laplace"
              ),
              selected = "simplified.laplace",
              multiple = FALSE,
              width = "30%"
            )),
            fluidRow(selectInput(
              inputId = "cinla_input_2",
              label = "Estratégia de Integração",
              choices = list(
                "auto" = "auto",
                "ccd" = "ccd",
                "grid" = "grid",
                "eb" = "eb",
                "user" = "user",
                "user.std" = "user.std"
              ),
              selected = "auto",
              multiple = FALSE,
              width = "30%"
            )),
            fluidRow(checkboxInput(
              inputId = "cinla_input_5",
              label = "Substituir Condicional na aproximação de Laplace
                                          pela esperança da condicional",
              value = TRUE
            ))
          )
        )
      )
    )

    showModal(options_modal)
  })

  ## -- Data ----
  data_input <- eventReactive(c(
    input$file,
    input$csv_header, input$csv_quote, input$csv_dec
  ), {
    if (!(file_ext(input$file$datapath) %in% accetable_formats)) {
      return(NULL)
    } else {
      indata <- switch(file_ext(input$file$datapath),
        "txt" = ,
        "csv" = read.table(input$file$datapath,
          header = ifelse(is.null(input$csv_header), TRUE, input$csv_header),
          sep = ifelse(is.null(input$csv_sep), ";", input$csv_sep),
          quote = ifelse(is.null(input$csv_quote), "\"", input$csv_quote),
          dec = ifelse(is.null(input$csv_dec), ",", input$csv_dec)
        )
      )
      names.variables <- names(model.matrix(formula(indata), data = indata)[1, ])
      covariates <- names(indata)
      names.variables <- c(names.variables[1], covariates[1], names.variables[-1])
      n.variables <- length(names.variables)
      n.obs <- nrow(indata)
      name.file <- input$file$name

      list(
        data = indata, # The data from data file
        n.variables = n.variables,
        n.obs = n.obs,
        infile.path = input$file$datapath,
        names.variables = names.variables,
        name.file = name.file,
        covariates = covariates
      )
    }
  })

  ## -- Main Panel ----
  ## -- Table with Data ----
  observeEvent(input$file_load_btn, {
    output$data <- renderRHandsontable({
      rhandsontable(data_input()$data, digits = 4, height = 800, stretchH = "all") %>%
        hot_cols(format = "0.0000")
    })
  })

  ## -- Modal Dialog Linear Model ----
  ## -- Modal Dialog ----
  linear_model_modal <- modalDialog(
    title = "Regressão Linear",
    fade = FALSE,
    size = "l",
    footer = tagList(
      actionButton("lm_ok", "Ok"),
      modalButton("Cancelar")
    ),
    tabsetPanel(
      id = "linear_panel",
      selected = "Selecione Varíaveis",
      tabPanel(
        title = "Selecione Varíaveis",
        fluidRow(
          uiOutput("uiResponse"),
          uiOutput("uiCovariates")
        ),
        fluidRow(
          title = "Selecione a família",
          selectInput(
            inputId = "lm_family_input",
            label = "Familia",
            choices = lm_family,
            selected = "normal"
          )
        )
      ),
      tabPanel(
        title = "Prioris",
        fluidRow(
          column(4, uiOutput("uiPrioriMean")),
          column(4, uiOutput("uiPrioriPrec"))
        )
      ),
      tabPanel(
        title = "Prioris Hyperparâmetros",
        fluidRow(column(6, uiOutput("ui_hyper_prior")))
      )
    )
  )

  observeEvent(input$linear_action_btn, {
    showModal(linear_model_modal)
  })

  ## -- Modal Render UI Response ----
  output$uiResponse <- renderUI({
    if (is.null(data_input()$n.variables)) {
      return()
    }
    radioGroupButtons(
      inputId = "responseVariable",
      label = "Selecione a varíavel resposta",
      choices = data_input()$covariates,
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon")
      )
    )
  })

  ## -- Modal Render UI Covariates ----
  output$uiCovariates <- renderUI({
    if (is.null(data_input()$n.variables)) {
      return()
    }
    checkboxGroupButtons(
      inputId = "covariates",
      label = "Selecione as covariáveis",
      choices = data_input()$covariates[data_input()$covariates != input$responseVariable],
      selected = data_input()$covariates[data_input()$covariates != input$responseVariable],
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok",
          lib = "glyphicon"
        )
      )
    )
  })

  output$uiPrioriMean <- renderUI({ # Generate the input boxes for the mean
    if (is.null(data_input()$n.variables)) {
      return()
    } # according to the number of columns of input file

    Rows <- lapply(1:data_input()$n.variables, function(number) {
      fluidRow(
        column(6, numericInput(
          inputId = paste0("mean", number),
          label = paste0("mean", data_input()$names.variables[number]),
          value = character(0)
        ))
      )
    })
  })

  output$uiPrioriPrec <- renderUI({ # Generate the input boxes for the precision according to the number of columns of input file
    if (is.null(data_input()$n.variables)) {
      return()
    }

    Rows <- lapply(1:data_input()$n.variables, function(number) {
      fluidRow(
        column(6, numericInput(
          inputId = paste0("prec", number),
          label = paste0("prec", data_input()$names.variables[number]),
          value = character(0)
        ))
      )
    })
  })

  # Create the UI with options to user select hyper priors distributions
  output$ui_hyper_prior <- renderUI({
    lapply(1:n_hyper(input$lm_family_input), function(number) {
      fluidRow(column(
        6, selectInput(
          inputId = paste0("lm_hyper_dist_", number),
          label = paste0("Selecione a Distribuicao do ", name_hyper(input$lm_family_input, number)),
          choices = priors_distributions,
          selected = hyper_default(input$lm_family_input, number),
          multiple = FALSE
        ),
        uiOutput(outputId = paste0("numeric_input_hyper_", number))
      ))
    })
  })

  # Create the UI with options to user input the values of the first hyperparamether
  output$numeric_input_hyper_1 <- renderUI({
    if (n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_1")]]), hyper_default(input$lm_family_input, 1), input[[ paste0("lm_hyper_dist_", 1)]])) == 0) {
      return()
    }
    lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_1")]]), hyper_default(input$lm_family_input, 1), input[[ paste0("lm_hyper_dist_", 1)]])), function(n_param) {
      numericInput(
        inputId = paste0("input_hyper_1_param_", n_param),
        label = paste0("Parametro ", n_param),
        value = hyper_default_param(input$lm_family_input, 1)[n_param]
      )
    })
  })

  # Create the UI with options to user input the values of the second hyperparamether
  output$numeric_input_hyper_2 <- renderUI({
    if (n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_2")]]), hyper_default(input$lm_family_input, 2), input[[ paste0("lm_hyper_dist_", 2)]])) == 0) {
      return()
    }
    lapply(1:n_param_prior(ifelse(is.null(input[[ paste0("lm_hyper_dist_2")]]), hyper_default(input$lm_family_input, 2), input[[ paste0("lm_hyper_dist_", 2)]])), function(n_param) {
      numericInput(
        inputId = paste0("input_hyper_2_param_", n_param),
        label = paste0("Parametro ", n_param),
        value = hyper_default_param(input$lm_family_input, 2)[n_param]
      )
    })
  })

  # Create the input of the fomula used on inla funtion
  inla.formula <- eventReactive(c(input$responseVariable, input$covariates), {
    f.covariates <- paste0(input$covariates, collapse = "+")
    f.response <- paste0(input$responseVariable)
    as.formula(paste0(f.response, rawToChar(as.raw(126)), f.covariates))
  })

  # What happens after the user clicks in ok to make the model
  tabindex <- reactiveVal(0)
  observeEvent(input$lm_ok, {
    # Close the modal with lm options
    removeModal()

    # Count the number of tabs
    tabindex(tabindex() + 1)
    output_name <- paste("output_tab", tabindex(), sep = "_")
    # Create the matrix used in control_fixed_input
    prioris <- matrix(NA_real_, nrow = data_input()$n.variables, ncol = 2)
    for (i in 1:data_input()$n.variables) {
      prioris[i, 1] <- ifelse("prec1" %in% names(input), input[[ paste0("mean", i) ]], NA_real_)
      prioris[i, 2] <- ifelse("prec1" %in% names(input), input[[ paste0("prec", i) ]], NA_real_)
    }

    # Create values to the result of the model and the edited call of the model
    lm_inla <- list()
    lm_inla_call_print <- list()

    # Created the model according to user input
    lm_inla[[output_name]] <- inla(
      formula = inla.formula(),
      data = hot_to_r(input$data),
      family = input$lm_family_input,
      control.fixed = control_fixed_input(
        prioris = prioris,
        v.names = data_input()$names.variables
      ),
      control.compute = control_compute_input,
      control.inla = control_inla_input,
      control.family = control_family_input(family_input = input$lm_family_input, input)
    )

    # Create the new call to the model
    lm_inla_call_print[[output_name]] <- paste0(
      "inla(data = ", "dat",
      ", formula = ", '"', input$responseVariable,
      " ~ ", paste0(input$covariates, collapse = " + "), '"',
      ifelse(input$lm_family_input == "gaussian", "", noquote(paste0(", family = ", '"', input$lm_family_input, '"'))),
      ifelse(all(is.na(prioris)), "", paste0(
        ", control.fixed = ",
        list_call(control_fixed_input(
          prioris = prioris,
          v.names = data_input()$names.variables
        ))
      )),
      ifelse(identical(paste0(input$ok_btn_options_modal), character(0)), "",
        paste0(", control.compe = ", list_call(control_compute_input))
      ),
      ifelse(checking_control_family(input), "", paste0(", control.family = ", list_call(control_family_input(family_input = input$lm_family_input, input)))),
      ")"
    )
    
    # UI of the result tab
    appendTab(
      inputId = "mytabs", select = TRUE,
      tabPanel(
        title = paste0("Modelo", tabindex()),
        useShinydashboard(),
        useShinyjs(),
        fluidRow(
          column(
            width = 9,
            box(
              title = "Call",
              status = "primary",
              solidHeader = TRUE,
              width = 9,
              textOutput(outputId = paste0("lm_call", tabindex())),
              tags$b(tags$a(icon("code"), "Show code", `data-toggle`="collapse", href="#showcode_dropdown")),
              tags$div(
                class="collapse", id="showcode_dropdown",
                tags$code(class = "language-r", 
                    paste0("dat <- ", '"', input$file$name, '"'),
                    tags$br(),
                    paste0("lm_inla_",tabindex()), " <- ",  lm_inla_call_print[[output_name]],
                    tags$br(),
                    paste0("lm_inla_",tabindex(), "$call")
                )
              )
            )
          ),
          column(width = 3,
                 box(
                   title = "Time Used",
                   solidHeader = TRUE,
                   width = 3, #CHANGE TO TABLE OUTPUT
                   Output(outputId = paste0("lm_time_used_", tabindex()))
                 ))
        )
      )
    )

    # "Server" of result tab
    
    # Call
    output[[ paste0("lm_call", tabindex()) ]] <- renderText({
      lm_inla_call_print[[output_name]]
    })
    
    # Time Used
    output[[ paste0("lm_time_used_", tabindex()) ]] <- renderText({
      lm_inla[[output_name]][["cpu.used"]]
    })
  })
}
