ui <- fluidPage(
  titlePanel(
    "Translate your modal or module"
  ),
  sidebarPanel(
    fluidRow(
      column(
        width = 10,
        fileInput(
          inputId = "file_modal_1",
          label = "Select the modal or module file"
        )
      ),
      column(
        width = 2,
        actionButton(
          inputId = "add_file_modal",
          label = NULL,
          icon = icon("plus-circle"),
          style = "all:unset; color:black; cursor:pointer; outline:none; font-size: 25px"
        ),
        actionButton(
          inputId = "remove_file_modal",
          label = NULL,
          icon = icon("minus-circle"),
          style = "all:unset; color:black; cursor:pointer; outline:none; font-size: 25px"
        )
      ),
      column(
        width = 10,
        uiOutput(outputId = "file_modal_ui")
      ),
      column(
        width = 12,
        fileInput(
          inputId = "file_json",
          label = "Select the JSON file to authenticate and use Google Translate API"
        )
      ),
      column(
        width = 12,
        textInput(
          inputId = "file_dictonary_name",
          label = "Name of the dictionary (optional)"
        )
      ),
      column(
        width = 12,
        actionButton(
          inputId = "translate_btn",
          label = "Translate"
        )
      )
    )
  ),
  mainPanel(
    column(
      width = 12,
      fluidRow(
        tableOutput(outputId = "words_discovered"),
        downloadButton("download_r_file", "Download R File"),
        tableOutput(outputId = "table_translated_txt")
      )
    )
  )
)
