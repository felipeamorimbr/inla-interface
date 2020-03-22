server <- function(input, output, session) {
  #File Modal and data
  source("modal/file_modal.R", local = TRUE)
  
  #Options modal
  source("modal/options_modal.R", local = TRUE)

  #Linear model Modal
  source("modal/lm_modal.R", local = TRUE)
  
  #General Linear Model Modal
  source("modal/glm_modal.R", local = TRUE)
}