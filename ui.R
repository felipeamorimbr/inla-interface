ui <- fluidPage(
  theme = "styles.css",
  fluidRow(
    useShinyjs(),
    useShinydashboard(),
    smNavBar("menu", "INLA",
      full.width = TRUE, fixed = FALSE,
      smNavDropdown(
        label = translate("File", "en", dictionary),
        smAction("file_action_btn", translate("File", "en", dictionary))
      ),
      smNavDropdown(
        label = translate("Models", "en", dictionary),
        smAction("linear_action_btn", translate("Linear Regression", "en", dictionary)),
        smAction("glm_action_btn", translate("General Linear Models", "en", dictionary))
      ),
      actionButton("options_action_btn", translate("Options", "en", dictionary))
    )
  ),
  tabsetPanel(
    type = "pills", id = "mytabs",
    tabPanel(
      translate("Data", "en", dictionary),
      fluidRow(
        box(
          id = "box_summary",
          title = translate("Summary", "en", dictionary),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput(outputId = "file_data_summary_ui")
        )
      ),
      fluidRow(
        box(
          id = "box_data",
          title = translate("Data", "en", dictionary),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          rHandsontableOutput(outputId = "data")
        )
      )
    ),
    hr()
  )
)
