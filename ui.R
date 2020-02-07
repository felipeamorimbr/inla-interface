ui <- fluidPage(
  theme = "styles.css",
  fluidRow(
    useShinyjs(),
    smNavBar("menu", "INLA",
      full.width = TRUE, fixed = FALSE,
      smNavDropdown(
        label = "Arquivo",
        smAction("file_action_btn", "Arquivo")
      ),
      smNavDropdown(
        label = "Modelos",
        smAction("linear_action_btn", "Regressão Linear")
      ),
      actionButton("options_action_btn", "Opções")
    )
  ),
  tabsetPanel(
    type = "pills", id = "mytabs",
    tabPanel("Dados", rHandsontableOutput(outputId = "data")),
    hr()
  )
)
