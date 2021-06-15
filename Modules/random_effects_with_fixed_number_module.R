random_effects_with_fix_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(outputId = ns("random_effects_here"))
  )
             
}

random_effects_with_fix <- function(id, covariates, model_choice, number_random_effects, random_effect_label){
  moduleServer(
    id,
    function(input, output, session){
      output$random_effects_here <- renderUI({
        ns <- session$ns
        
        list_ui <- NULL
        if(!(is.integer(number_random_effects))){
          return(NULL)
        }else{
          for(i in 1:number_random_effects){
            list_ui[[i]] <- fluidRow(
              column(width = 4,
                     pickerInput(
                       inputId = ns(paste0("covariate_", i)),
                       label = random_effect_label[i],
                       choices = covariates,
                       multiple = FALSE
                     ),
                     pickerInput(
                       inputId = ns(paste0("model_",i)),
                       label = model_choice[i],
                       choices = ,
                       multiple = FALSE
                     ))
            )
          }
        }
      })
    }
  )
}