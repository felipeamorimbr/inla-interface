ui <- fluidPage(
  fluidRow(useShinyjs(),
    smNavBar("menu", "INLA", full.width = TRUE, fixed = FALSE,
             smNavDropdown("Arquivo",
                           actionButton("file_action_btn", "Arquivo")),
             smNavDropdown("Modelos",
                           actionButton("linear_action_btn", "Regressão Linear")))
  ),
  tabsetPanel(type = "pills", id = "mytabs",
              tabPanel("Dados", rHandsontableOutput(outputId = "data"))
  )
)