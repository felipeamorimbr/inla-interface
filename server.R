server <- function(input, output){
    output$image1 <- renderImage(
      return(list(
        src = "regression-icon.png",
        filetype = "image/png",
        alt = "face"
      ))
    )
}
