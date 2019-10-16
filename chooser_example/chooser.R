chooserInput <- function(inputId, leftLabel, 
                         rightLabel1, rightLabel2, 
                         leftChoices, 
                         rightChoices1, rightChoices2,
                         size = 5, multiple = FALSE,
                         height = "400px",
                         width = "200px") {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices1 <- lapply(rightChoices1, tags$option)
  rightChoices2 <- lapply(rightChoices2, tags$option)
  
  multiple <- ifelse(multiple, "multiple", NULL)
  
  tagList(
    singleton(tags$head(
      tags$script(src = "chooser-binding.js")
    )),
    div(id = inputId, class = "chooser", 
        style = sprintf("display: table-caption; height: %s", height),
        
        div(class = "chooser-container chooser-left-container", 
            style = paste0("display: table-cell; vertical-align: middle; width: 100%; height: ", height),
            
            h5(leftLabel),
            tags$select(class = "left", size = size, multiple = multiple, leftChoices, 
                        style = paste0("width: ", width, "; height: -webkit-fill-available;"))
        ),
        
        div(class = "chooser-container chooser-center-container",
            style = paste0("padding: 0 10px 0 10px; display: table-cell; vertical-align: middle; width: 100%; height: ", height),
            
            div(style = "display: grid",
                div(style = "display: table",
                    div(style = "display: table-cell; vertical-align: middle; width: 50%; height: 60px;",
                        icon("arrow-circle-o-right", "right1-arrow fa-2x"),
                        tags$br(),
                        icon("arrow-circle-o-left", "left1-arrow fa-2x")
                    )
                ),
                div(style = "display: table",
                    div(style = "display: table-cell; vertical-align: middle; width: 50%; height: 340px;",
                        icon("arrow-circle-o-right", "right2-arrow fa-2x"),
                        tags$br(),
                        icon("arrow-circle-o-left", "left2-arrow fa-2x")
                    )
                )
            )
        ),
        
        div(class = "chooser-container chooser-right-container", 
            style = paste0("display: table-cell; vertical-align: middle; width: 100%; height: ", height),
            
            div(style = "display: table",
                h5(rightLabel1),
                tags$select(class = "right1", size = size, multiple = multiple, rightChoices1, 
                            style = paste0("width: ", width, "; height: 50px;"))
            ),
            
            div(style = "display: table",
                h5(rightLabel2),
                tags$select(class = "right2", size = size, multiple = multiple, rightChoices2, 
                            style = paste0("width: ", width, "; height: 315px;"))
            )
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if(is.null(data))
    NULL
  else
    list(left = as.character(data$left), right1 = as.character(data$right1), right2 = as.character(data$right2))
}, force = TRUE)
