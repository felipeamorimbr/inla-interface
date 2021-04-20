#Create a box
box_model_ui <- function(id, name, author, icon, color){
  ns <- NS(id)
  fluidPage(
    includeCSS("Modules/box_models.css"),
    tags$head(tags$script(src = "https://kit.fontawesome.com/a076d05399.js")),
    HTML(paste0(
      '<a id="', id,'" href="#" class="action-button">
        <div class="box-models" style= "background-color: ', color, ';">
          <div class="topleft-box">', name,   
          '</div>
          <div class="topright-box">
            <i class="fas ', icon,'"></i>
          </div>
          <div class="bottomleft-box"> 
            by:',author,
          '</div>
        </div>
      </a>'
    ))
  )
}

box_model_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      
    }
  )
}

#Test----
# ui <- fluidPage(
#   box_model_ui("test", "Linear Model", "Felipe", icon = "fa-chart-area", "#12a19b"),
#   box_model_ui("test2", "Hyerarquical Model", "Felipe", "fa-chart-area", "#12a19b")
# )
# 
# server <- function(input, output, session){
#   box_model_server("test")
#   box_model_server("test2")
# }
# 
# shinyApp(ui, server)
