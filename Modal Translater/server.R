server <- function(input, output, session) {
  observeEvent(input$translate_btn, {
    assign(x = input$file_dictonary_name, translate_modal(
      modal = input$file_modal$datapath,
      json = input$file_json$datapath
    ), envir = globalenv())
    
    
  })
  observeEvent(input$translate_btn, {
    output$table_translated_txt <- renderTable({
      eval(parse(text = paste0(input$file_dictonary_name)))
    })
  })
  output$download_r_file <- downloadHandler(
    filename = function() {
      paste0(input$file_dictonary_name, ".RData")
    },
    content = function(file) {
      save(list = input$file_dictonary_name, file = file)
    }
  )
}
