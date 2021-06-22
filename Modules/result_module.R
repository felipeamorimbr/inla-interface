# Results Modal
results_UI <- function(id, INLAresult, control_compute, control_inla, inla_call_print, tab_index, data) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          id = ns(paste0("lm_box_call_", lm_tabindex())),
          title = translate("Call", language = language_selected, words_one),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          textOutput(outputId = ns(paste0("lm_call", lm_tabindex()))),
          tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_call", lm_tabindex()))),
          tags$div(
            class = "collapse", id = ns(paste0("showcode_call", lm_tabindex())),
            tags$code(
              class = "language-r",
              paste0("dat <- ", '"', input$file$name, '"'),
              tags$br(),
              paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
              tags$br(),
              paste0("lm_inla_", lm_tabindex(), "$call")
            )
          ),
          footer = downloadBttn(
            outputId = paste0("download_script_", lm_tabindex()),
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
          id = paste0("lm_box_time_used", lm_tabindex()),
          title = translate("Time Used", language = language_selected, words_one),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput(outputId = paste0("lm_time_used_", lm_tabindex())),
          tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_time", lm_tabindex()))),
          tags$div(
            class = "collapse", id = paste0("showcode_time", lm_tabindex()),
            tags$code(
              class = "language-r",
              paste0("dat <- ", '"', input$file$name, '"'),
              tags$br(),
              paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
              tags$br(),
              paste0("lm_inla_", lm_tabindex(), "$cpu.sued")
            )
          )
        ),
        dropdownButton(
          icon = icon("download"),
          up = TRUE,
          status = "myclass",
          inputId = paste0("download_rdata_", lm_tabindex()),
          actionButton("test", "test"),
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
          id = paste0("lm_box_fix_effects_", lm_tabindex()),
          title = translate("Fixed Effects", language = language_selected, words_one),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput(outputId = paste0("lm_fix_effects_", lm_tabindex())),
          tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_fix_effects_", lm_tabindex()))),
          tags$div(
            class = "collapse", id = paste0("showcode_fix_effects_", lm_tabindex()),
            tags$code(
              class = "language-r",
              paste0("dat <- ", '"', input$file$name, '"'),
              tags$br(),
              paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
              tags$br(),
              paste0("lm_inla_", lm_tabindex(), "$summary.fixed")
            )
          ),
          footer = downloadBttn(
            outputId = paste0("download_summary_", lm_tabindex()),
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
              id = paste0("lm_box_model_hyper_", lm_tabindex()),
              title = translate("Model Hyperparameters", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              dataTableOutput(outputId = paste0("lm_model_hyper_", lm_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_model_hyper_", lm_tabindex()))),
              tags$div(
                class = "collapse", id = paste0("showcode_model_hyper_", lm_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                  tags$br(),
                  paste0("lm_inla_", lm_tabindex(), "$summary.hyperpar")
                )
              )
            )
          ),
          box(
            id = paste0("lm_box_neffp_", lm_tabindex()),
            title = translate("Expected Effective Number of Parameters in the Model", language = language_selected, words_one),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            dataTableOutput(outputId = paste0("lm_neffp_", lm_tabindex())),
            tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_neffp_", lm_tabindex()))),
            tags$div(
              class = "collapse", id = paste0("showcode_neffp_", lm_tabindex()),
              tags$code(
                class = "language-r",
                paste0("dat <- ", '"', input$file$name, '"'),
                tags$br(),
                paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                tags$br(),
                paste0("lm_inla_", lm_tabindex(), "$neffp")
              )
            )
          ),
          conditionalPanel(
            condition = "(input.ccompute_input_4 != '' &&  input.ccompute_input_4 == true)",
            box(
              id = paste0("lm_box_dic_waic_", lm_tabindex()),
              title = translate("DIC and WAIC", language = language_selected, words_one),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              dataTableOutput(outputId = paste0("lm_dic_waic_", lm_tabindex())),
              tags$b(tags$a(icon("code"), translate("Show code", language = language_selected, words_one), `data-toggle` = "collapse", href = paste0("#showcode_dic_waic_", lm_tabindex()))),
              tags$div(
                class = "collapse", id = paste0("showcode_dic_waic_", lm_tabindex()),
                tags$code(
                  class = "language-r",
                  paste0("dat <- ", '"', input$file$name, '"'),
                  tags$br(),
                  paste0("lm_inla_", lm_tabindex()), " <- ", lm_inla_call_print[[lm_output_name]],
                  tags$br(),
                  paste0("lm_inla_", lm_tabindex(), "$dic$dic"),
                  tags$br(),
                  paste0("lm_inla", lm_tabindex(), "$dic$dic.sat"),
                  tags$br(),
                  paste0("lm_inla", lm_tabindex(), "$dic$p.eff")
                )
              )
            )
          )
        )
      )
    )
  )
}
