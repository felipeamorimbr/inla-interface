# Results Modal
#UI ----
results_UI <- function(id, INLAresult, inla_call_print, tab_index, data_input) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          id = ns("box_call"),
          title = translate("Call", language = language_selected, words_one),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          textOutput(outputId = ns("model_call")),
          tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#",ns("showcode_call")))),
          tags$div(
            class = "collapse", id = ns("showcode_call"),
            tags$code(
              class = "language-r",
              paste0("dat <- ", '"', data_input()$infile.path, '"'),
              tags$br(),
              paste0("model_inla_", tab_index()), " <- ", inla_call_print,
              tags$br(),
              paste0("model_inla_", tab_index(), "$call")
            )
          ),
          footer = downloadBttn(
            outputId = ns("download_script"),
            label = translate("Download Script", language = language_selected, words_one),
            style = "material-flat",
            color = "primary",
            size = "xs"
          )
        )
      ),
      column(
        width = 6,
        box(
          id = ns("lm_box_time_used"),
          title = translate("Time Used", language = language_selected, words_one),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput(outputId = ns("model_time_used")),
          tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#", ns("showcode_time")))),
          tags$div(
            class = "collapse", id = ns("showcode_time"),
            tags$code(
              class = "language-r",
              paste0("dat <- ", '"', data_input()$infile.path, '"'),
              tags$br(),
              paste0("model_inla_", tab_index()), " <- ", inla_call_print,
              tags$br(),
              paste0("model_inla_", tab_index(), "$cpu.sued")
            )
          )
        ),
        dropdownButton(
          icon = icon("download"),
          up = TRUE,
          status = "myclass",
          inputId = ns("download_rdata"),
          actionButton(ns("test"), "test"),
          tags$head(tags$style(
            "
                             .btn-myclass {
                                position: fixed;
                                bottom: 20px;
                                right: 30px;
                                z-index: 99;
                                font-size: 18px;
                                border: none;
                                outline: none;
                                background-color: #12a19b;
                                color: white;
                                cursor: pointer;
                                padding: 15px;
                                border-radius: 4px;
                             }
                              .dropup .dropdown-menu, .navbar-fixed-bottom .dropdown .dropdown-menu {
                      top: auto;
                      bottom: 8vh;
                      right: 80px;
                      left: auto;
                      position: fixed;
                  }
                             "
          ))
        )
      )
    ), # fluidrow ends here
    fluidRow(
      column(
        width = 12,
        box(
          id = ns("box_fix_effects_"),
          title = translate("Fixed Effects", language = language_selected, words_one),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput(outputId = ns("model_fix_effects")),
          tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#", ns("showcode_fix_effects")))),
          tags$div(
            class = "collapse", id = ns("showcode_fix_effects"),
            tags$code(
              class = "language-r",
              paste0("dat <- ", '"', data_input()$infile.path, '"'),
              tags$br(),
              paste0("model_inla_", tab_index()), " <- ", inla_call_print,
              tags$br(),
              paste0("model_inla_", tab_index(), "$summary.fixed")
            )
          ),
          footer = downloadBttn(
            outputId = ns("model_download_summary"),
            label = translate("Save Summary data", language = language_selected, words_one),
            style = "material-flat",
            size = "xs"
          )
        )
      ),
      column(
        width = 12,
        useShinyjs(),
        fluidRow(
          conditionalPanel(
            condition = "(input.ccompute_input_2 != '') || (input.ccompute_input_2 == '' &&  input.ccompute_input_2 == true)",
            box(
              id = ns("box_model_hyper"),
              title = translate("Model Hyperparameters", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              dataTableOutput(outputId = ns("model_hyper")),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#", ns("showcode_model_hyper")))),
              tags$div(
                class = "collapse", id = ns("showcode_model_hyper"),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', data_input()$infile.path, '"'),
                  tags$br(),
                  paste0("model_inla_", tab_index()), " <- ", inla_call_print,
                  tags$br(),
                  paste0("model_inla_", tab_index(), "$summary.hyperpar")
                )
              )
            )
          ),
          box(
            id = ns("box_neffp"),
            title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            dataTableOutput(outputId = ns("model_neffp")),
            tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#", ns("showcode_neffp")))),
            tags$div(
              class = "collapse", id = ns("showcode_neffp"),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', data_input()$infile.path, '"'),
                tags$br(),
                paste0("model_inla_", tab_index()), " <- ", inla_call_print,
                tags$br(),
                paste0("model_inla_", tab_index(), "$neffp")
              )
            )
          ),
          conditionalPanel(
            condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
            box(
              id = ns("box_dic_waic"),
              title = translate("DIC and WAIC", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              dataTableOutput(outputId = ns("model_dic_waic")),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#",ns("showcode_dic_waic")))),
              tags$div(
                class = "collapse", id = ns("showcode_dic_waic"),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', data_input()$infile.path, '"'),
                  tags$br(),
                  paste0("model_inla_", tab_index()), " <- ", inla_call_print,
                  tags$br(),
                  paste0("model_inla_", tab_index(), "$dic$dic"),
                  tags$br(),
                  paste0("model_inla_", tab_index(), "$dic$dic.sat"),
                  tags$br(),
                  paste0("model_inla_", tab_index(), "$dic$p.eff")
                )
              )
            )
          )
        )
      )
    )
  )
}

#Server ----

results_server <- function(id, INLAresult, inla_call_print, tab_index, data_input){
  moduleServer(
    id,
    function(input, output, session){
     
      output$model_call <- renderText({
        inla_call_print
      })
      
      # Download Script
      output$download_script <- downloadHandler(
        filename = function() {
          paste0("model_", tab_index(), "_script", ".r")
        },
        content = function(file) {
          write(paste(paste0("dat <- read.csv2(", data_input()$infile.path, ")"),
                      paste0("inla_model_", tab_index(), "<-", inla_call_print),
                      sep = "\n"
          ), file)
        }
      )
      
      # Time Used
      output$model_time_used <- renderDataTable({
        data_time_used <- INLAresult[["cpu.used"]] %>%
          t() %>%
          as.data.frame(row.names = c("Time")) %>%
          round(digits = 5)
        
        DT::datatable(
          data = data_time_used,
          options = list(
            dom = "t",
            pageLength = 5
          )
        )
      })
      
      # Fixed Effects
      output$model_fix_effects <- renderDataTable(
        {
          INLAresult[["summary.fixed"]] %>%
            round(digits = 5)
        },
        options = list(
          paging = FALSE,
          dom = "t"
        )
      )
      
      # Download Summary
      output$model_download_summary <- downloadHandler(
        filename = function() {
          paste0("model_", tab_index(), "summary.csv")
        },
        content = function(file) {
          write.csv2(as.data.frame(INLAresult$summary.fixed), file = file)
        }
      )
      
      # Model Hyper
      output$model_hyper <- renderDataTable(
        {
          INLAresult[["summary.hyperpar"]] %>%
            round(digits = 5)
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
      
      # Others (neffp)
      output$model_neffp <- renderDataTable(
        {
          lm_neffp_dataframe <- INLAresult[["neffp"]] %>%
            round(digits = 5)
          colnames(lm_neffp_dataframe) <- "Expected Value"
          lm_neffp_dataframe
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
      
      # Devicance Information Criterion (DIC)
      output$model_dic_waic <- renderDataTable(
        {
          data.frame(
            "DIC" = INLAresult[["dic"]][["dic"]],
            "DIC Saturated" = INLAresult[["dic"]][["dic.sat"]],
            "Effective number of parameters (DIC)" = INLAresult[["dic"]][["p.eff"]],
            "WAIC" = INLAresult[["waic"]][["waic"]],
            "Effective number of parameters (WAIC)" = INLAresult[["waic"]][["p.eff"]],
            row.names = "Expected Value"
          ) %>%
            round(digits = 5) %>%
            t()
        },
        options = list(
          dom = "t",
          paging = FALSE
        )
      )
    }
  )
}
