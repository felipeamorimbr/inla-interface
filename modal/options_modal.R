observeEvent(input$ok_btn_options_modal, {
  # Update control_compute_input in global envioronment
  rlang::env_bind(.env = globalenv(), control_compute_input = list(
    openmp.strategy = input$ccompute_input_1,
    hyperpar = input$ccompute_input_2,
    return.marginals = input$ccompute_input_3,
    dic = input$ccompute_input_4,
    # mlik = input$ccompute_input_5,
    # cpo = input$ccompute_input_6,
    # po = input$ccompute_input_7,
    waic = input$ccompute_input_4
    # q = input$ccompute_input_9,
    # config = input$ccompute_input_10,
    # smtp = input$ccompute_input_11,
    # graph = input$ccompute_input_12,
    # gdensity = input$ccompute_input_13
  ))
  # uptdate control_inla_input in global enviroment
  rlang::env_bind(.env = globalenv(), control_inla_input = list(
    strategy = input$cinla_input_1,
    int.strategy = input$cinla_input_2,
    fast = input$cinla_input_5
  ))
  
  removeModal()
})

# observeEvent(input$language, {
#   rlang::env_bind(.env = globalenv(), language_selected = input$language)
#   useSweetAlert()
#   
#   confirmSweetAlert(
#     session = session,
#     inputId = "change_language_confirm",
#     title = translate("Confirm the language change?", language = language_selected, options_modal_words),
#     text = NULL,
#     type = "question"
#   )
# })
# 
# observeEvent(input$change_language_confirm, {
#   if(input$change_language_confirm){
#     session$reload()
#   }else{
#     return()
#   }
# })

observeEvent(input$options_action_btn, {
  options_modal <- modalDialog( ## -- DOUGLAS: Criando o modal aqui dentro. Desta forma ele vai ser recriado toda vez que clicar em opções
    useShinyjs(),
    title = translate("Options", language = language_selected, options_modal_words),
    fade = FALSE,
    size = "l",
    footer = tagList(
      actionButton("ok_btn_options_modal", translate("Ok", language = language_selected, options_modal_words)),
      modalButton(translate("Cancel", language = language_selected, options_modal_words))
    ),
    fluidPage(
      tabsetPanel(
        tabPanel(
          "Control Compute",
          fluidRow(selectInput(
            inputId = "ccompute_input_1",
            label = translate("Computational strategy", language = language_selected, options_modal_words),
            choices = list(
              "small" = translate("Small", language = language_selected, options_modal_words),
              "medium" = translate("Medium", language = language_selected, options_modal_words),
              "large" = translate("Large", language = language_selected, options_modal_words),
              "huge" =  translate("Huge", language = language_selected, options_modal_words),
              "default" = translate("Default", language = language_selected, options_modal_words)
            ),
            selected = control_compute_input[[1]],
            multiple = FALSE,
            width = "30%"
          )),
          fluidRow(checkboxInput(
            inputId = "ccompute_input_2",
            label = translate("Compute the marginal of hyperparameters", language = language_selected, options_modal_words),
            value = control_compute_input[[2]]
          )),
          fluidRow(checkboxInput(
            inputId = "ccompute_input_3",
            label = translate("Return the marginal of latent field", language = language_selected, options_modal_words),
            value = control_compute_input[[3]]
          )),
          fluidRow(checkboxInput(
            inputId = "ccompute_input_4",
            label = translate("Compute the DIC-value and WAIC", language = language_selected, options_modal_words),
            value = globalenv()$control_compute_input[[4]]
          ))
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_5",
          #   label = translate("Compute the marginal likelihood", language = language_selected, options_modal_words),
          #   value = control_compute_input[[5]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_6",
          #   label = translate("Compute the CPO", language = language_selected, options_modal_words),
          #   value = control_compute_input[[6]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_7",
          #   label = "Calcular a preditive ordinate",
          #   value = control_compute_input[[7]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_8",
          #   label = "Calcular o WAIC",
          #   value = control_compute_input[[8]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_9",
          #   label = "Gerar as imagens da matriz de precição, matriz de precição reordenada
          #   e o triangulo de Cholesky",
          #   value = control_compute_input[[9]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_10",
          #   label = "Guardar as aproximações do Gaussian Markov Random Field",
          #   value = control_compute_input[[10]]
          # )),
          # fluidRow(selectInput(
          #   inputId = "ccompute_input_11",
          #   label = "Estratégia para resolver a matriz esparça",
          #   choices = list(
          #     "Taucs" = "taucs",
          #     "Band" = "band",
          #     "Pardiso" = "pardiso",
          #     "Padrão" = "default"
          #   ),
          #   selected = control_compute_input[[11]],
          #   multiple = FALSE,
          #   width = "30%"
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_12",
          #   label = "Retornar os Gráficos",
          #   value = control_compute_input[[12]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_13",
          #   label = "Retornar as densidades Gaussianas",
          #   value = control_compute_input[[13]]
          # ))
        ),
        tabPanel(
          "Control INLA",
          fluidRow(selectInput(
            inputId = "cinla_input_1",
            label = translate("Approximation strategy", language = language_selected, options_modal_words),
            choices = list(
              "gaussian" = translate("Gaussian", language = language_selected, options_modal_words),
              "simplified.laplace" = translate("Simplified Laplace", language = language_selected, options_modal_words),
              "Laplace" = "laplace"
            ),
            selected = "simplified.laplace",
            multiple = FALSE,
            width = "30%"
          )),
          fluidRow(selectInput(
            inputId = "cinla_input_2",
            label = translate("Integration strategy", language = language_selected, options_modal_words),
            choices = list(
              "auto" = "auto",
              "ccd" = "ccd",
              "grid" = "grid",
              "eb" = "eb",
              "user" = "user",
              "user.std" = "user.std"
            ),
            selected = "auto",
            multiple = FALSE,
            width = "30%"
          )),
          fluidRow(checkboxInput(
            inputId = "cinla_input_5",
            label = translate("Replace conditional modes in the Laplace approximation with conditional expectation", language = language_selected, options_modal_words),
            value = TRUE
          ))
        )
      )
    )
  )

  showModal(options_modal)
})