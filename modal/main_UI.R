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
                   label = translate("File", language = language_selected, main_UI_words),
                   smAction("file_action_btn", translate("File", language = language_selected, main_UI_words))
                 ),
                 smNavDropdown(
                   label = translate("Models", language = language_selected, main_UI_words),
                   smAction("linear_action_btn", translate("Linear Regression", language = language_selected, main_UI_words)),
                   smAction("glm_action_btn", translate("Hierarchical Linear Models", language = language_selected, main_UI_words))
                 ),
                 actionButton("options_action_btn", translate("Options", language = language_selected, main_UI_words))
        )
      ),
      tabsetPanel(
        type = "pills", id = "mytabs",
        tabPanel(
          translate("Data", language = language_selected, main_UI_words),
          fluidRow(
            box(
              id = "box_summary",
              title = translate("Summary", language = language_selected, main_UI_words),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput(outputId = "file_data_summary_ui")
            )
          ),
          fluidRow(
            box(
              id = "box_data",
              title = translate("Data", language = language_selected, main_UI_words),
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