source("functions/inla_models_functions.R")
#Module for select family, link function (or not) and priors
sel_family_ui <- function(id, familyLabel, familyChoices, linkLabel = NULL){
  ns <- NS(id)
  useShinyjs()
  column(
    width = 7,
    style = "padding-top: 25px",
    column(
      width = 5,
      offset = 2,
      selectInput(
        inputId = ns("sel_family"),
        label = familyLabel,
        choices = familyChoices,
        selected = "gaussian",
        multiple = FALSE,
        selectize = FALSE
      ),
      selectInput(
        inputId = ns("sel_link"),
        label = linkLabel, 
        choices = link_avaliable("gaussian"),
        multiple = FALSE,
        selectize = FALSE
      )
    ),
    
    column(
      width = 5,
      uiOutput(outputId = ns("hyperprior_ui"))
    )
  )
}

sel_family <- function(id, Link){
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(input$sel_family, { #Update link function available when family changes
        updateSelectInput(session, 
                          inputId = "sel_link",
                          choices = link_avaliable(input$sel_family))
      })
      
      observe({ #Hide or show selectInput for link function
        validate(need(input$sel_lnk, FALSE))
        shinyjs::toggle("sel_link", condition = Link)
      })
      
      n_hyperprior <- reactiveValues(n_hyperprior = 0)
      
      observeEvent(input$sel_family, {
        n_hyperprior$n_hyperprior <- n_hyper(input$sel_family) 
        
        
        output$hyperprior_ui <- renderUI({
          ns <- session$ns
          if(n_hyperprior$n_hyperprior == 0)
            return()
          list_ui <- list()
          for(i in 1:n_hyperprior$n_hyperprior){
            aux_hyper_dist <- paste0("hyper_dist_", i)
            aux_hyper_ui <- paste0("hyper_ui_", i)
            list_ui[[i]] <- column(6, selectInput(inputId = ns(aux_hyper_dist), choices = priors_distributions, selected = hyper_default(input$sel_family, i),
                                                  label = paste0("Selct Priros for ", name_hyper(input$sel_family, i))),
                                   uiOutput(outputId = ns(aux_hyper_ui)))
          }
          list_ui
        })
      })
      
      observeEvent(input[[ "hyper_dist_1" ]], {
        output$hyper_ui_1 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_1) == 0)
            return()

          list_ui_hyper_1 <- list()
          for(j in 1:n_param_prior(input$hyper_dist_1)){
            aux_hyper_id <- paste0("hyper_dist_1_param_", j)
            list_ui_hyper_1[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 1)[j])
          }
          list_ui_hyper_1
        })
      })

      observeEvent(input[[ "hyper_dist_2" ]], {
        output$hyper_ui_2 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_2) == 0)
            return()

          list_ui_hyper_2 <- list()
          for(j in 1:n_param_prior(input$hyper_dist_2)){
            aux_hyper_id <- paste0("hyper_dist_1_param_", j)
            list_ui_hyper_2[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 2)[j])
          }
          list_ui_hyper_2
        })
      })

      observeEvent(input[[ "hyper_dist_3" ]], {
        output$hyper_ui_3 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_3) == 0)
            return()

          list_ui_hyper_3 <- list()
          for(j in 3:n_param_prior(input$hyper_dist_3)){
            aux_hyper_id <- paste0("hyper_dist_3_param_", j)
            list_ui_hyper_3[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 3)[j])
          }
          list_ui_hyper_3
        })
      })

      observeEvent(input[[ "hyper_dist_4" ]], {
        output$hyper_ui_4 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_4) == 0)
            return()

          list_ui_hyper_4 <- list()
          for(j in 4:n_param_prior(input$hyper_dist_4)){
            aux_hyper_id <- paste0("hyper_dist_4_param_", j)
            list_ui_hyper_4[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 4)[j])
          }
          list_ui_hyper_4
        })
      })

      observeEvent(input[[ "hyper_dist_5" ]], {
        output$hyper_ui_5 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_5) == 0)
            return()

          list_ui_hyper_5 <- list()
          for(j in 5:n_param_prior(input$hyper_dist_5)){
            aux_hyper_id <- paste0("hyper_dist_5_param_", j)
            list_ui_hyper_5[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 5)[j])
          }
          list_ui_hyper_5
        })
      })

      observeEvent(input[[ "hyper_dist_6" ]], {
        output$hyper_ui_6 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_6) == 0)
            return()

          list_ui_hyper_6 <- list()
          for(j in 6:n_param_prior(input$hyper_dist_6)){
            aux_hyper_id <- paste0("hyper_dist_6_param_", j)
            list_ui_hyper_6[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 6)[j])
          }
          list_ui_hyper_6
        })
      })

      observeEvent(input[[ "hyper_dist_7" ]], {
        output$hyper_ui_7 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_7) == 0)
            return()

          list_ui_hyper_7 <- list()
          for(j in 7:n_param_prior(input$hyper_dist_7)){
            aux_hyper_id <- paste0("hyper_dist_7_param_", j)
            list_ui_hyper_7[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 7)[j])
          }
          list_ui_hyper_7
        })
      })

      observeEvent(input[[ "hyper_dist_8" ]], {
        output$hyper_ui_8 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_8) == 0)
            return()

          list_ui_hyper_8 <- list()
          for(j in 8:n_param_prior(input$hyper_dist_8)){
            aux_hyper_id <- paste0("hyper_dist_8_param_", j)
            list_ui_hyper_8[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 8)[j])
          }
          list_ui_hyper_8
        })
      })

      observeEvent(input[[ "hyper_dist_9" ]], {
        output$hyper_ui_9 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_9) == 0)
            return()

          list_ui_hyper_9 <- list()
          for(j in 9:n_param_prior(input$hyper_dist_9)){
            aux_hyper_id <- paste0("hyper_dist_9_param_", j)
            list_ui_hyper_9[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                 label = paste0("Parameter", j),
                                                 value = hyper_default_param(input$sel_family, 9)[j])
          }
          list_ui_hyper_9
        })
      })

      observeEvent(input[[ "hyper_dist_10" ]], {
        output$hyper_ui_10 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_10) == 0)
            return()
          list_ui_hyper_10 <- list()
          for(j in 10:n_param_prior(input$hyper_dist_10)){
            aux_hyper_id <- paste0("hyper_dist_10_param_", j)
            list_ui_hyper_10[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 10)[j])
          }
          list_ui_hyper_10
        })
      })

      observeEvent(input[[ "hyper_dist_11" ]], {
        output$hyper_ui_11 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_11) == 0)
            return()

          list_ui_hyper_11 <- list()
          for(j in 11:n_param_prior(input$hyper_dist_11)){
            aux_hyper_id <- paste0("hyper_dist_11_param_", j)
            list_ui_hyper_11[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 11)[j])
          }
          list_ui_hyper_11
        })
      })

      observeEvent(input[[ "hyper_dist_12" ]], {
        output$hyper_ui_12 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_12) == 0)
            return()

          list_ui_hyper_12 <- list()
          for(j in 12:n_param_prior(input$hyper_dist_12)){
            aux_hyper_id <- paste0("hyper_dist_12_param_", j)
            list_ui_hyper_12[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 12)[j])
          }
          list_ui_hyper_12
        })
      })

      observeEvent(input[[ "hyper_dist_13" ]], {
        output$hyper_ui_13 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_13) == 0)
            return()

          list_ui_hyper_13 <- list()
          for(j in 13:n_param_prior(input$hyper_dist_13)){
            aux_hyper_id <- paste0("hyper_dist_13_param_", j)
            list_ui_hyper_13[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 13)[j])
          }
          list_ui_hyper_13
        })
      })

      observeEvent(input[[ "hyper_dist_14" ]], {
        output$hyper_ui_14 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_14) == 0)
            return()

          list_ui_hyper_14 <- list()
          for(j in 14:n_param_prior(input$hyper_dist_14)){
            aux_hyper_id <- paste0("hyper_dist_14_param_", j)
            list_ui_hyper_14[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 14)[j])
          }
          list_ui_hyper_14
        })
      })

      observeEvent(input[[ "hyper_dist_15" ]], {
        output$hyper_ui_15 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_15) == 0)
            return()

          list_ui_hyper_15 <- list()
          for(j in 15:n_param_prior(input$hyper_dist_15)){
            aux_hyper_id <- paste0("hyper_dist_15_param_", j)
            list_ui_hyper_15[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 15)[j])
          }
          list_ui_hyper_15
        })
      })

      observeEvent(input[[ "hyper_dist_16" ]], {
        output$hyper_ui_16 <- renderUI({
          ns <- session$ns
          if(n_param_prior(input$hyper_dist_16) == 0)
            return()

          list_ui_hyper_16 <- list()
          for(j in 16:n_param_prior(input$hyper_dist_16)){
            aux_hyper_id <- paste0("hyper_dist_16_param_", j)
            list_ui_hyper_16[[j]] <- numericInput(inputId = ns(aux_hyper_id),
                                                  label = paste0("Parameter", j),
                                                  value = hyper_default_param(input$sel_family, 16)[j])
          }
          list_ui_hyper_16
        })
      })
      return(reactive(control_family_input(input)))
    }
  )
}

#Teast ----
# ui <- fluidPage(
#   sel_family_ui(id = "teste",familyLabel = "Escolha a Família", familyChoices = glm_family, linkLabel = "Seleciona a função de ligação"),
#   verbatimTextOutput("imprimir")
# )
# 
# server <- function(input, output, session){
#   output$imprimir <- renderPrint({
#     sel_family("teste", TRUE)()
#   })
# }
# 
# shinyApp(ui, server)
# 
