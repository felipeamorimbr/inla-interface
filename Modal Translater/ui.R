ui <- fluidPage(
  titlePanel(
    "Translate your modal"
  ),
  sidebarPanel(
    fluidRow(
      fileInput(
        inputId = "file_modal",
        label = "Select the modal file"
      ),
      fileInput(
        inputId = "file_json",
        label = "Select the JSON file to authenticate and use Google Translate API"
      ),
      textInput(
        inputId = "file_dictonary_name",
        label = "Name of the dictionary (optional)"
      ),
      actionButton(inputId = "translate_btn",
                   label = "Translate")
    )
  ),
  mainPanel(
    fluidRow(
    downloadButton("download_r_file", "Download R File"),
      tableOutput(outputId = "table_translated_txt")
    )

  )
)
