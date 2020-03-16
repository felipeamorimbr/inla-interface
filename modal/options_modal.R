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

observeEvent(input$options_action_btn, {
  options_modal <- modalDialog( ## -- DOUGLAS: Criando o modal aqui dentro. Desta forma ele vai ser recriado toda vez que clicar em opções
    useShinyjs(),
    title = translate("Options", "en", dictionary),
    fade = FALSE,
    size = "l",
    footer = tagList(
      actionButton("ok_btn_options_modal", translate("Ok", "en", dictionary)),
      modalButton(translate("Cancel", "en", dictionary))
    ),
    fluidPage(
      tabsetPanel(
        tabPanel(
          "Control Compute",
          fluidRow(selectInput(
            inputId = "ccompute_input_1",
            label = translate("Computational strategy", "en", dictionary),
            choices = list(
              "small" = translate("Small", "en", dictionary),
              "medium" = translate("Medium", "en", dictionary),
              "large" = translate("Large", "en", dictionary),
              "huge" =  translate("Huge", "en", dictionary),
              "default" = translate("Default", "en", dictionary)
            ),
            selected = control_compute_input[[1]],
            multiple = FALSE,
            width = "30%"
          )),
          fluidRow(checkboxInput(
            inputId = "ccompute_input_2",
            label = translate("Compute the marginal of hyperparameters", "en", dictionary),
            value = control_compute_input[[2]]
          )),
          fluidRow(checkboxInput(
            inputId = "ccompute_input_3",
            label = translate("Return the marginal of latent field", "en", dictionary),
            value = control_compute_input[[3]]
          )),
          fluidRow(checkboxInput(
            inputId = "ccompute_input_4",
            label = translate("Compute the DIC-value and WAIC", "en", dictionary),
            value = globalenv()$control_compute_input[[4]]
          ))
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_5",
          #   label = translate("Compute the marginal likelihood", "en", dictionary),
          #   value = control_compute_input[[5]]
          # )),
          # fluidRow(checkboxInput(
          #   inputId = "ccompute_input_6",
          #   label = translate("Compute the CPO", "en", dictionary),
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
            label = translate("Approximation strategy", "en", dictionary),
            choices = list(
              "gaussian" = translate("Gaussian", "en", dictionary),
              "simplified.laplace" = translate("Simplified Laplace", "en", dictionary),
              "Laplace" = "laplace"
            ),
            selected = "simplified.laplace",
            multiple = FALSE,
            width = "30%"
          )),
          fluidRow(selectInput(
            inputId = "cinla_input_2",
            label = translate("Integration strategy", "en", dictionary),
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
            label = translate("Replace conditional modes in the Laplace approximation with conditional expectation", "en", dictionary),
            value = TRUE
          ))
        )
      )
    )
  )
  
  showModal(options_modal)
})