server <- function(input, output, session) {
  #Main UI
  source("modal/main_UI.R", local = TRUE)
  
  #File Modal and data
  source("modal/file_modal.R", local = TRUE, encoding = "UTF-8")
  
  #Options modal
  source("modal/options_modal.R", local = TRUE, encoding = "UTF-8")
  
  #Linear model Modal
  # source("modal/lm_modal.R", local = TRUE, encoding = "UTF-8")
  source("modal/lm_new_modal.R", local = TRUE, encoding = "UTF-8")
  #General Linear Model Modal
  source("modal/glm_modal.R", local = TRUE)
}