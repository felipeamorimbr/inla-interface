# New Chooser
new_chooser_UI <- function(id, respLabel,
                           familyLabel, resp_var,
                           selected_left, familyChoices, data,
                           selected_right = NULL, resp_numeric = FALSE) {
  ns <- NS(id)

  column(
    width = 7, # Tamanho total
    column(
      width = 5, # Tamanho da coluna da esquerda
      column(
        width = 12,
        selectInput(
          inputId = ns("sel_family"),
          label = familyLabel,
          choices = familyChoices,
          selected = "gaussian",
          multiple = FALSE,
          selectize = FALSE
        ),
        tags$br(),
        tags$br()
      ),
      column(
        width = 12,
        selectInput(
          inputId = ns("leftInput"),
          label = "",
          choices = selected_left,
          multiple = TRUE,
          selectize = FALSE,
          size = 8,
          width = "100%"
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
      style = "height: 200px; padding-top:150px",
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
      column(
        width = 12,
        selectInput(
          inputId = ns("resp_var"),
          label = respLabel,
          choices = if(isFALSE(resp_numeric)){unique(c(resp_var, selected_left, selected_right))}else{data %>% select_if(is.numeric) %>% names()},
          selected = resp_var,
          multiple = FALSE
        ),
        checkboxInput(
          inputId = ns("intercept"),
          label = translate("Intercept", language = language_selected, words_one),
          value = TRUE
        )
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
          width = "100%"
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

new_chooser <- function(id, resp_var, selected_left, selected_right, leftLabel, rightLabel) {
  moduleServer(
    id,
    function(input, output, session) {
      variables <- reactiveValues(
        resp_var = resp_var,
        left = selected_left,
        right = selected_right
      )

      observeEvent(input$resp_var, {
        todas_vars <- c(variables$resp_var, variables$left, variables$right)
        variables$right <- variables$right[variables$right != input$resp_var]
        variables$left <- todas_vars[!(todas_vars %in% c(input$resp_var, variables$right))]
        variables$resp_var <- input$resp_var

        updateSelectInput(session,
          inputId = "leftInput",
          label = leftLabel,
          choices = variables$left,
          selected = NULL
        )

        updateSelectInput(session,
          inputId = "rightInput",
          label = rightLabel,
          choices = variables$right,
          selected = NULL
        )
      })

      observeEvent(input$right_arrow, {
        new_variable_right <- input$leftInput
        if (is.null(input$leftInput)) {
          return()
        }

        variables$left <- variables$left[!variables$left %in% new_variable_right]
        variables$right <- c(variables$right, new_variable_right)
        updateSelectInput(session,
          inputId = "leftInput",
          label = leftLabel,
          choices = variables$left,
          selected = NULL
        )

        updateSelectInput(session,
          inputId = "rightInput",
          label = rightLabel,
          choices = variables$right,
          selected = new_variable_right
        )
      })

      observeEvent(input$left_arrow, {
        new_variable_left <- input$rightInput
        if (is.null(input$rightInput)) {
          return()
        }

        variables$right <- variables$right[!variables$right %in% new_variable_left]
        variables$left <- c(variables$left, new_variable_left)


        updateSelectInput(session,
          inputId = "rightInput",
          label = rightLabel,
          choices = variables$right,
          selected = NULL
        )

        updateSelectInput(session,
          inputId = "leftInput",
          label = leftLabel,
          choices = variables$left,
          selected = new_variable_left
        )
      })

      observeEvent(input$left_sel_all, {
        updateSelectInput(session,
          inputId = "leftInput",
          label = leftLabel,
          choices = variables$left,
          selected = variables$left
        )
      })

      observeEvent(input$left_desel_all, {
        updateSelectInput(session,
          inputId = "leftInput",
          label = leftLabel,
          choices = variables$left,
          selected = NULL
        )
      })

      observeEvent(input$right_sel_all, {
        updateSelectInput(session,
          inputId = "rightInput",
          label = rightLabel,
          choices = variables$right,
          selected = variables$right
        )
      })

      observeEvent(input$right_desel_all, {
        updateSelectInput(session,
          inputId = "rightInput",
          label = rightLabel,
          choices = variables$right,
          selected = NULL
        )
      })

      return(
        list(
          resp_var = reactive({
            input$resp_var
          }),
          intercept = reactive({
            input$intercept
          }),
          cov_var = reactive(variables$right),
          family = reactive(input$sel_family),
          not_selected = reactive(variables$left),
          variables = variables
        )
      )
    }
  )
}

# Test with a Modal ----
# ui <- fluidPage(
#   actionButton("Open_modal", "Open")
# )
# 
# server <- function(input, output, session) {
#   vals <- list(variables = list(resp_var = "X1", left = c("X1", "X2", "X3"), right = NULL))
# 
#   observeEvent(input$Open_modal, {
# 
#     vals <<- new_chooser(
#       id = "Test",
#       resp_var = vals$variables$resp_var,
#       selected_left = vals$variables$left,
#       selected_right = vals$variables$right,
#       leftLabel = "Covariaveis",
#       rightLabel = "Covariaveis selecionadas"
#     )
# 
#     showModal(
#       modalDialog(
#         footer = actionButton("close", "Close"),
#         new_chooser_UI(
#           id = "Test",
#           respLabel = "Respostas",
#           familyLabel = "Familia",
#           resp_var = vals$variables$resp_var,
#           selected_left = vals$variables$left,
#           selected_right = vals$variables$right,
#           familyChoices = c("t", "Gaussian")
#         ),
#         actionButton("browser", "ok")
#       ))
#   })
# 
#   observeEvent(input$close, {
#     removeModal()
#   })
# 
#   observeEvent(input$browser, {
#     browser()
#   })
# }
# 
# shinyApp(ui, server)
