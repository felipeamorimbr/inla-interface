# New Chooser
new_chooser_UI <- function(id, respLabel, selected_left, selected_right = NULL) {
  ns <- NS(id)
  
  column(
    width = 4, # Tamanho total
    column(
      width = 12,
      column(
        width = 6, offset = 3,
        selectInput(
          inputId = ns("resp_var"),
          label = respLabel,
          choices = unique(c(selected_left, selected_right)), 
          selected = selected_left[1],
          multiple = FALSE
        ), 
        checkboxInput(
          inputId = ns("intercept"),
          label = "Intercept",
          value = TRUE
        )
      )
    ),
    column(
      width = 5, # Tamanho da coluna da esquerda
      style = 'height: 180px',
      column(
        width = 12,
        selectInput(
          inputId = ns("leftInput"),
          label = "",
          choices = selected_left[-1],
          multiple = TRUE,
          selectize = FALSE,
          size = 8,
          width = '100%'
        )
      ),
      column(
        width = 6,
        actionButton(
          inputId = ns("left_sel_all"),
          icon = icon("check-square"),
          label = "All"
        )
      ),
      column(
        width = 6,
        actionButton(
          inputId = ns("left_desel_all"),
          icon = icon("square"),
          label = "All"
        )
      )
    ),
    column(
      width = 2, # Tamanho da coluna do meio
      style = 'height: 200px; padding-top:50px',
      actionButton(
        inputId = ns("right_arrow"), 
        label = NULL, 
        icon = icon("arrow-circle-o-right", "right-arrow fa-2x"),
        style = "all:unset; color:black; cursor:pointer; outline:none; font-size: 18px"
      ),
      tags$br(),
      actionButton(
        inputId = ns("left_arrow"),
        label = NULL,
        icon = icon("arrow-circle-o-left", "left-arrow fa-2x"),
        style = "all:unset; color:black; cursor:pointer; outline:none; font-size: 18px"
      )
    ),
    column(
      width = 5, # Tamanho da coluna da direita
      style = 'height: 180px',
      column(
        width = 12,
        selectInput(
          inputId = ns("rightInput"),
          label = "",
          choices = selected_right,
          multiple = TRUE,
          selectize = FALSE,
          size = 8,
          width = '100%'
        )
      ),
      column(
        width = 6,
        actionButton(
          inputId = ns("right_sel_all"),
          icon = icon("check-square"),
          label = "All"
        )
      ),
      column(
        width = 6,
        actionButton(
          inputId = ns("right_desel_all"),
          icon = icon("square"),
          label = "All"
        )
      )
    )
  )
}

new_chooser <- function(id, selected_left, selected_right, leftLabel, rightLabel){
  moduleServer(
    id,
    function(input, output, session){
      variables <- reactiveValues(
        left = NULL, 
        right = NULL
      )
      
      observeEvent(input$resp_var, {
        todas_vars <- c(selected_left, selected_right)
        
        variables$right <- variables$right[variables$right != input$resp_var]
        variables$left <- todas_vars[!(todas_vars %in% c(input$resp_var, variables$right))]
        
        updateSelectInput(session,
                          inputId = "leftInput",
                          label = leftLabel,
                          choices = variables$left,
                          selected = NULL)
        
        updateSelectInput(session,
                          inputId = "rightInput",
                          label = rightLabel,
                          choices = variables$right,
                          selected = NULL)
      })
      
      observeEvent(input$right_arrow, {
        new_variable_right <- input$leftInput
        if(is.null(input$leftInput))
          return()
        
        variables$left <- variables$left[! variables$left %in% new_variable_right]
        variables$right <- c(variables$right, new_variable_right)
        updateSelectInput(session,
                          inputId = "leftInput",
                          label = leftLabel,
                          choices = variables$left,
                          selected = NULL)
        
        updateSelectInput(session,
                          inputId = "rightInput",
                          label = rightLabel,
                          choices = variables$right,
                          selected = new_variable_right)
      })
      
      observeEvent(input$left_arrow, {
        new_variable_left <- input$rightInput
        if(is.null(input$rightInput))
          return()
        
        variables$right <- variables$right[! variables$right %in% new_variable_left]
        variables$left <- c(variables$left, new_variable_left)
        
        
        updateSelectInput(session, 
                          inputId = "rightInput",
                          label = rightLabel,
                          choices = variables$right,
                          selected = NULL)
        
        updateSelectInput(session, 
                          inputId = "leftInput",
                          label = leftLabel,
                          choices = variables$left,
                          selected = new_variable_left)
      })
      
      observeEvent(input$left_sel_all, {
        updateSelectInput(session,
                          inputId = "leftInput",
                          label = leftLabel,
                          choices = variables$left,
                          selected = variables$left)
      })
      
      observeEvent(input$left_desel_all, {
        updateSelectInput(session,
                          inputId = "leftInput",
                          label = leftLabel,
                          choices = variables$left,
                          selected = NULL)
      })
      
      observeEvent(input$right_sel_all, {
        updateSelectInput(session,
                          inputId = "rightInput",
                          label = rightLabel,
                          choices = variables$right,
                          selected = variables$right)
      })
      
      observeEvent(input$right_desel_all, {
        updateSelectInput(session,
                          inputId = "rightInput",
                          label = rightLabel,
                          choices = variables$right,
                          selected = NULL)
      })
      # return(list(resp_sel = resp_selected, cov_var = cov_var_selected, intercept = intercept_selected))
    }
  )
}