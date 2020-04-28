# File modal and objects
## -- Modal Dialog File Input ----
## -- Modal Dialog ----
file_modal <- modalDialog(
  useShinyjs(),
  title = translate("Loading the data", language = language_selected, dictionary),
  fade = FALSE,
  size = "l",
  footer = tagList(
    shinyjs::disabled(actionButton(
      "file_load_btn",
      translate("Open", language = language_selected, dictionary)
    )),
    modalButton(translate("Cancel", language = language_selected, dictionary))
  ),
  fluidPage(
    fluidRow(
      column(
        width = 4,
        fileInput("file", label = h4(translate("Select the file", language = language_selected, dictionary))),
        shinyjs::hidden(actionLink(inputId = "file_adv_options_btn", label = translate("Advanced options", language = language_selected, dictionary))),
        textOutput(outputId = "file_error_extension_txt"),
        shinyjs::hidden(uiOutput("file_adv_options_ui")),
        selectInput(inputId = "language",
                    label = translate("Language", language = language_selected, dictionary),
                    choices = c("English" = "en",
                                "Português (Brasil)" = "pt-br"),
                    multiple = FALSE
                    )
      ),
      column(
        width = 8,
        DT::dataTableOutput(outputId = "file_datatable")
      )
    )
  )
)

## -- Modal Observe Events ----
showModal(file_modal)

observeEvent(input$file_action_btn, {
  showModal(file_modal)
})

observeEvent(input$file_load_btn, {
  rlang::env_bind(.env = globalenv(), language_selected = input$language)
  removeModal()
})

# observeEvent(input$language, {
# 
# })

observeEvent(input$file, {
  if (!is.null(input$file) && (file_ext(input$file$datapath) %in% accetable_formats)) {
    shinyjs::enable("file_load_btn")
    shinyjs::show("file_adv_options_btn")
  }
  if (!(file_ext(input$file$datapath) %in% accetable_formats)) {
    output$file_error_extension_txt <- renderText(translate("Invalid extension file", language = language_selected, dictionary))
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
            label = translate("Header", language = language_selected, dictionary),
            value = TRUE
          )
        ),
        fluidRow(
          textInput(
            inputId = "csv_sep",
            label = translate("Separator", language = language_selected, dictionary),
            value = ";",
            width = "20%"
          )
        ),
        fluidRow(
          textInput(
            inputId = "csv_quote",
            label = translate("Quote", language = language_selected, dictionary),
            value = "\"",
            width = "20%"
          )
        ),
        fluidRow(
          textInput(
            inputId = "csv_dec",
            label = translate("Decimal", language = language_selected, dictionary),
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
  
  output$file_data_summary_ui <- renderDataTable(
    {
      data.frame(unclass(summary(data_input()$data)),
                 check.names = FALSE,
                 stringsAsFactors = FALSE,
                 row.names = NULL
      )
    },
    options = list(
      dom = "t"
    )
  )
})
