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
                   # smAction("linear_action_btn", translate("Linear Regression", language = language_selected, main_UI_words)),
                   smAction("linear_action_btn_2", "Linear Regression"),
                   smAction("glm_action_btn", translate("Hierarchical Linear Models", language = language_selected, main_UI_words))
                 ),
                 actionButton("options_action_btn", translate("Options", language = language_selected, main_UI_words))
                 # ,smNavDropdown(
                 #   label = "Download",
                 #   smHeader("Data"),
                 #   downloadButton(outputId = "download_data", label = translate("Download data", language = language_selected, main_UI_words)),
                 #   uiOutput(outputId = "download_lm_model_data_ui"),
                 #   uiOutput(outputId = "download_glm_model_data_ui"),
                 #   textOutput(outputId = "no_download_avaliable")
                 # )
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



# observeEvent(c(input$lm_ok, input$glm_ok),{
#   if(lm_tabindex() + glm_tabindex() == 0){
#     output$no_downoad_avaliable <- renderText("No Download Avaliable")
#   }else{
#     if(lm_tabindex() > 0){
#       output$download_lm_model_data_ui <- renderUI({
#         lapply(1:lm_tabindex(), function(n_lm_models){
#           
#         })
#       })
#     }
#   }
#   
# })