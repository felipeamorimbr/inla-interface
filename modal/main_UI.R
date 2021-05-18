observeEvent(input$file_load_btn, {
  output$main_ui <- renderUI({
    fluidPage(
      theme = "styles.css",
      fluidRow(
        useShinyjs(),
        useShinydashboard(),
        smNavBar("menu", "INLAmod",
          inverse = TRUE,
          full.width = TRUE, fixed = FALSE,
          smNavDropdown(
            label = translate("File", language = language_selected, words_one),
            smAction("file_action_btn", translate("File", language = language_selected, words_one))
          ),
          smNavDropdown(
            label = translate("Models", language = language_selected, words_one),
            # smAction("linear_action_btn", translate("Linear Regression", language = language_selected, words_one)),
            # smAction("linear_action_btn_2", "Linear Regression"),
            # smAction("glm_action_btn", translate("Hierarchical Linear Models", language = language_selected, words_one))
            model_buttons
          ),
          actionButton("options_action_btn",
            label = "", icon = icon("cogs"),
            style = "all:unset; color:#B8B8B8; cursor:pointer; outline:none; font-size: 18px; padding-top: 12px"
          )
          # ,smNavDropdown(
          #   label = "Download",
          #   smHeader("Data"),
          #   downloadButton(outputId = "download_data", label = translate("Download data", language = language_selected, words_one)),
          #   uiOutput(outputId = "download_lm_model_data_ui"),
          #   uiOutput(outputId = "download_glm_model_data_ui"),
          #   textOutput(outputId = "no_download_avaliable")
          # )
        )
      ),
      tags$style(HTML("
        .tabbable > .nav > li > a {background-color: #FFF  ;  color: #12a19b;}
        .tabbable > .nav > li[class=active]    > a {background-color: #12a19b; color:white}
        }")),
      tabsetPanel(
        type = "pills", id = "mytabs", selected = translate("Data", language = language_selected, words_one),
        tabPanel(
          title = "",
          icon = icon("home"),
          fluidPage(
            model_boxes
          )
        ),
        tabPanel(
          translate("Data", language = language_selected, words_one),
          br(),
          tags$style(HTML("
                
                
                .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#12a19b
                                    }
                
                .box.box-solid.box-primary{
                border-bottom-color:#12a19b;
                border-left-color:#12a19b;
                border-right-color:#12a19b;
                border-top-color:#12a19b;
                }")),
          fluidRow(
            column(
              width = 12,
              box(
                id = "box_summary",
                title = translate("Summary", language = language_selected, words_one),
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                dataTableOutput(outputId = "file_data_summary_ui")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                id = "box_data",
                title = translate("Data", language = language_selected, words_one),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                rHandsontableOutput(outputId = "data")
              )
            )
          )
        ),
        hr()
      )
    )
  })
})
box_model_server("lm_box")


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