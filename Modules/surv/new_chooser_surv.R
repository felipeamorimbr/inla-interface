# New Chooser
library(stringr)
strings_subset <- c(names(inla.models()$likelihood))

new_chooser_UI_surv <- function(id, respLabel, familyLabel, selected_left, familyChoices, resp_var, status_var, selected_right = NULL) {
  ns <- NS(id)
  
  column(
    width = 12, # Tamanho total
    
    #Time
    column(
      width = 5, # Tamanho da coluna/bloco 1
      column(width = 10,
             selectInput(
               inputId = ns("resp_var"),
               label = respLabel,
               choices = unique(c(resp_var, selected_left, selected_right)), 
               selected = resp_var,
               multiple = FALSE
             ), 
             checkboxInput(
               inputId = ns("intercept"),
               label = "Intercept",
               value = TRUE
             )
      ),
      column(
        width = 10,
        selectInput(
          inputId = ns("leftInput"),
          label = "",
          choices = selected_left,
          multiple = TRUE,
          selectize = FALSE,
          size = 8,
          width = '100%'
        )
      ),
      column(
        width = 5,
        actionButton(
          inputId = ns("left_sel_all"),
          icon = icon("check-square"),
          label = "All"
        )
      ),
      column(
        width = 5,
        actionButton(
          inputId = ns("left_desel_all"),
          icon = icon("square"),
          label = "All"
        )
      )
    ),
    column(
      width = 2, # Tamanho da coluna do meio
      style = 'height: 200px; padding-top:150px',
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
    
    #Status
    column(
      width = 5, # Tamanho da coluna 2
      column(width = 10,
             selectInput(
               inputId = ns("status_var"),
               label = 'Status',
               choices = unique(c(status_var, selected_left, selected_right), 
                                selected = status_var,
                                multiple = FALSE
               )
             )
      )
    ),    
    
    #Family
    
    column(
      width = 5, # Tamanho da coluna 3
      column(
        width = 12,
        selectInput(
          inputId = ns("sel_family"),
          label = familyLabel,
          choices = familyChoices,
          selected = "weibullsurv",
          multiple = FALSE,
          selectize = FALSE
        ),
        tags$br(),
        tags$br()
      ),
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
        width = 5,
        actionButton(
          inputId = ns("right_sel_all"),
          icon = icon("check-square"),
          label = "All"
        )
      ),
      column(
        width = 5,
        actionButton(
          inputId = ns("right_desel_all"),
          icon = icon("square"),
          label = "All"
        )
      )
    )
  )
}

new_chooser_surv <- function(id, selected_left, selected_right, leftLabel, resp_var, status_var, rightLabel) {
  moduleServer(
    id,
    function(input, output, session){
      variables <- reactiveValues(
        resp_var = resp_var,
        status_var = status_var,
        left = selected_left, 
        right = selected_right
      )
      
      #Time      
      observeEvent(input$resp_var, {
        todas_vars <- c(variables$resp_var, variables$status_var, variables$left, variables$right)
        if(input$resp_var == input$status_var){
          variables$status_var <- variables$resp_var
          variables$resp_var <- input$resp_var
          
          updateSelectInput(session,
                            inputId = "status_var",
                            selected = variables$status_var)
        }else{
          variables$right <- variables$right[variables$right != input$resp_var]
          variables$left <- todas_vars[!(todas_vars %in% c(input$resp_var, input$status_var, variables$right))]
          variables$resp_var <- input$resp_var
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
        }
      })
      
      #Status      
      observeEvent(input$status_var, {
        todas_vars <- c(variables$status_var, variables$resp_var, variables$left, variables$right)
        
        if(input$resp_var == input$status_var){
          variables$resp_var <- variables$status_var
          variables$status_var <- input$status_var
          
          updateSelectInput(session,
                            inputId = "resp_var",
                            selected = variables$resp_var)
        }else{
          variables$right <- variables$right[variables$right != input$status_var]
          variables$left <- todas_vars[!(todas_vars %in% c(input$status_var,input$resp_var, variables$right))]
          variables$status_var <- input$status_var
          
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
        }
      })      
      
      #Variables      
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
      
      return(
        list(
          resp_var = reactive({variables$resp_var}),
          intercept = reactive({input$intercept}),
          status_var = reactive({variables$status_var}),
          cov_var = reactive(variables$right),
          family = reactive(input$sel_family),
          not_selected = reactive(variables$left),
          variables = variables
        ))
    }
  )
}

#Test with a Modal ----
ui <- fluidPage(
  actionButton("Open_modal", "Open")
)

server <- function(input, output, session){
  vals <- list(variables = list(resp_var = "X1",
                                status_var = "X2", 
                                left = c("X1", "X2", "X3"),
                                right = NULL))
  
  
  
  observeEvent(input$Open_modal,{
    
    vals <<- new_chooser_surv(
      id = "Test",
      resp_var = vals$variables$resp_var,
      status_var = vals$variables$status_var,
      selected_left = vals$variables$left,
      selected_right = vals$variables$right,
      leftLabel = "Covariates",
      rightLabel = "Covariates selected"
    )
    
    
    showModal(modalDialog(
      footer = actionButton("browser", "Run"),
      actionButton("close", "X"),
      new_chooser_UI_surv(id = "Test",
                          respLabel = "Time",
                          resp_var = vals$variables$resp_var,
                          status_var = vals$variables$status_var,
                          familyLabel = "Family",
                          selected_left = vals$variables$left,
                          selected_right = vals$variables$right,
                          familyChoices = str_subset(string = strings_subset, pattern = c("surv")))
    ))
  })
  
  observeEvent(input$browser, {
  })
  observeEvent(input$close, {
    removeModal()
    
  })
}

shinyApp(ui, server)

