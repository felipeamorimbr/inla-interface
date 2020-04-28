observeEvent(input$file_load_btn, {
  output$main_ui <- renderUI({
    fluidPage(
      theme = "styles.css",
      fluidRow(
        useShinyjs(),
        useShinydashboard(),
        smNavBar("menu", "INLA",
                 full.width = TRUE, fixed = FALSE,
                 smNavDropdown(
                   label = translate("File", language = language_selected, dictionary),
                   smAction("file_action_btn", translate("File", language = language_selected, dictionary))
                 ),
                 smNavDropdown(
                   label = translate("Models", language = language_selected, dictionary),
                   smAction("linear_action_btn", translate("Linear Regression", language = language_selected, dictionary)),
                   smAction("glm_action_btn", translate("General Linear Models", language = language_selected, dictionary))
                 ),
                 actionButton("options_action_btn", translate("Options", language = language_selected, dictionary))
        )
      ),
      tabsetPanel(
        type = "pills", id = "mytabs",
        tabPanel(
          translate("Data", language = language_selected, dictionary),
          fluidRow(
            box(
              id = "box_summary",
              title = translate("Summary", language = language_selected, dictionary),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput(outputId = "file_data_summary_ui")
            )
          ),
          fluidRow(
            box(
              id = "box_data",
              title = translate("Data", language = language_selected, dictionary),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              rHandsontableOutput(outputId = "data")
            )
          )
        ),
        hr()
      )
    )
  })
})