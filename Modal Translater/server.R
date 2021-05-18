server <- function(input, output, session) {

  # Number of modal or module files
  values <- reactiveValues(n = 1, words = NULL)

  #Creating the observeEvent to extract the words from file_modal_1
  observeEvent(input$file_modal_1, {
    new_words <- extract_words(input$file_modal_1$datapath)
    values$words <- c(values$words, new_words)
    values$words <- unique(values$words)
  })
  
  # Adding one more file to translate
  observeEvent(input$add_file_modal, {
    values$n <- values$n + 1
    insertUI(
      selector = "#file_modal_ui",
      where = "beforeEnd",
      ui = div(
        id = paste0("file_div_", values$n),
          fileInput(
            inputId = paste0("file_modal_", values$n),
            label = "Select the modal or module file"
          )
      )
    )
  })
  
  #Removing the last file_modal to translate
  observeEvent(input$remove_file_modal, {
    shiny::removeUI(selector = paste0("#file_div_", values$n))
    values$n <- values$n - 1
  })

  #Modal to verify the words
  observeEvent(input$translate_btn, {
    showModal(
      modalDialog(
        title = "Confirm the words and expressions that will be translated",
        size = "l",
        fade = "FALSE",
        footer = tagList(
          actionButton("confirm", "Confirm"),
          modalButton("Cancel")
        ),
        textOutput(outputId = "words_extracted")
      )
    )

    output$words_extracted <- renderText({
      modal_words <<- NULL
      for(i in 1:values$n){
        if(!is.null(input[[ paste0("file_modal_", i)  ]])){
          modal_words <<- c(modal_words, extract_words( input[[ paste0("file_modal_", i) ]]$datapath  ))
        }
      }
      modal_words
    }, sep = "; ")
  })
  
  
  observeEvent(input$confirm, {
    assign(x = input$file_dictonary_name, translate_modal(
      words_to_translate = modal_words,
      json = input$file_json$datapath
    ), envir = globalenv())
    removeModal()
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
