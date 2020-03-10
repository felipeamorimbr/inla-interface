translate <- function(words, language, dictionary = dictionary){
  return(dictionary[[words]][[language]])
}

#Dictionary
dictionary <- list(
  "File" = list(
    "en" = "File",
    "pt-br" = "Arquivo"
  ),
  "Models" = list(
    "en" = "Models",
    "pt-br" = "Modelos"
  ),
  "Linear Regression" = list(
    "en" = "Linear Regression",
    "pt-br" = "Regressão Linear"
  ),
  "Options" = list(
    "en" = "Options",
    "pt-br" = "Opções"
  ),
  "Data" = list(
    "en" = "Data",
    "pt-br" = "Dados"
  ),
  "Summary" = list(
    "en" = "Summary",
    "pt-br" = "Resumo"
  ),
  "Loading the data" = list(
    "en" = "Loading the data",
    "pt-br" = "Carregando os dados"
  ),
  "Open" = list(
    "en" = "Open",
    "pt-br" = "Abrir"
  ),
  "Cancel" = list(
    "en" = "Cancel",
    "pt-br" = "Cancelar"
  ),
  "Select the file" = list(
    "en" = "Select the file",
    "pt-br" = "Selecione o arquivo"
  ),
  "Advanced options" = list(
    "en" = "Advanced options",
    "pt-br" = "Opções Avançadas"
  ),
  "Invalid extension file" = list(
    "en" = "Invalid extension file",
    "pt-br" = "Extensão de arquivo inválida"
  ),
  "Header" = list(
    "en" = "Header",
    "pt-br" = "Cabeçalho"
  ),
  "Separator" = list(
    "en" = "Separator",
    "pt-br" = "Separador"
  ),
  "Quote" = list(
    "en" = "Quote",
    "pt-br" = "Citação"
  ),
  "Decimal" = list(
    "en" = "Decimal",
    "pt-br" = "Decimal"
  ),
  "Ok" = list(
    "en" = "Ok",
    "pt-br" = "Ok"
  ),
  "Computational strategy" = list(
    "en" = "Computational strategy",
    "pt-br" = "Estratégia computacional"
  ),
  "Small" = list(
    "en" = "Small",
    "pt-br" = "Pequena"
  ),
  "Medium" = list(
    "en" = "Medium",
    "pt-br" = "Média"
  ),
  "Large" = list(
    "en" = "Large",
    "pt-br" = "Grande"
  ),
  "Huge" = list(
    "en" = "Huge",
    "pt-br" = "Imensa"
  ),
  "Default" = list(
    "en" = "Default",
    "pt-br" = "Padrão"
  ),
  "Compute the marginal of hyperparameters" = list(
    "en" = "Compute the marginal of hyperparameters",
    "pt-br" = "Calcular a marginal dos hiperparamêtros"
  ),
  "Return the marginal of latent field" = list(
    "en" = "Return the marginal of latent field",
    "pt-br" = "Retornar as marginais do campo latente"
  ),
  "Compute the DIC-value and WAIC" = list(
    "en" = "Compute the DIC-value and WAIC",
    "pt-br" = "Calcular o valor-DIC e WAIC"
  ),
  "Compute the marginal likelihood" = list(
    "en" = "Compute the marginal likelihood",
    "pt-br" = "Calcular a verossemelhança marginal"
  ),
  "Approximation strategy" = list(
    "en" = "Approximation strategy",
    "pt-br" = "Estratégia de aproximação"
  ),
  "Gaussian" = list(
    "en" = "Gaussian",
    "pt-br" = "Gaussiana"
  ),
  "Simplified Laplace" = list(
    "en" = "Simplified Laplace",
    "pt-br" = "Laplace simplifcada"
  ),
  "Integration strategy" = list(
    "en" = "Integration strategy",
    "pt-br" = "Estratégia de integração"
  ),
  "Replace conditional modes in the Laplace approximation with conditional expectation" = list(
     "en" = "Replace conditional modes in the Laplace approximation with conditional expectation",
     "pt-br" = "Substituir Condicional na aproximação de Laplace
              pela esperança da condicional"
   ),
  "Select Variables" = list(
    "en" = "Select Variables",
    "pt-br" = "Selecione as variáveis"
  ),
  "Intercept" = list(
    "en" = "Intercept",
    "pt-br" = "Intercepto"
  ),
  "Edit priors" = list(
    "en" = "Edit priors",
    "pt-br" = "Editar prioris"
  ),
  "Hyperpriors" = list(
    "en" = "Hyperpriors",
    "pt-br" = "Hyperprioris"
  ),
  "Family" = list(
    "en" = "Family",
    "pt-br" = "Família"
  ),
  "Select the response variable" = list(
    "en" = "Select the response variable",
    "pt-br" = "Selecione a variável resposta"
  ),
  "Select the covariates" = list(
    "en" = "Select the covariates",
    "pt-br" = "Selecione as covariáveis"
  ), 
  "Select the distribution of " = list(
    "en" = "Select the distribution of ", 
    "pt-br" = "Selecione a distribuição do "
  ),
  "Parameter " = list(
    "en" = "Parameter ",
    "pt-br" = "Parâmetro "
  ),
  "Model" = list(
    "en" = "Model",
    "pt-br" = "Modelo"
  ),
  "Call" = list(
    "en" = "Call",
    "pt-br" = "Código"
  ),
  "Time Used" = list(
    "en" = "Time Used",
    "pt-br" = "Tempo de Processamento"
  ),
  "Show code" = list(
    "en" = "Show code",
    "pt-br" = "Mostrar código"
  ),
  "Fixed Effects" = list(
    "en" = "Fixed Effects",
    "pt-br" = "Efeitos fixos"
  ),
  "Model Hyperparameters" = list(
    "en" = "Model Hyperparameters",
    "pt-br" = "Hiperparametros do modelo"
  ),
  "Expected Effective Number of Parameters in the Model" = list(
    "en" = "Expected Effective Number of Parameters in the Model",
    "pt-br" = "Número esperado de parâmetros no modelo"
  ),
  "DIC and WAIC" = list(
    "en" = "DIC and WAIC",
    "pt-br" = "DIC e WAIC"
  ),
  "Error" = list(
    "en" = "Error",
    "pt-br" = "Erro"
  ),
  "Error: no covariates selected" = list(
    "en" = "Error: no covariates selected",
    "pt-br" = "Erro: nenhuma covariável selecioanada"
  ),
  "-The response variable must be numeric" = list(
    "en" = "-The response variable must be numeric",
    "pt-br" = "-A variável resposta deve ser numérica"
  ),
  "-The priors of fixed effects must be numeric" = list(
    "en" = "-The priors of fixed effects must be numeric",
    "pt-br" = "-As prioris dos efeitos fixos devem ser numéricas"
  ),
  "-The Hyperprioris must be numeric" = list(
    "en" = "-The Hyperprioris must be numeric",
    "pt-br" = "-As Hiper Prioris devem ser numéricas"
  ),
  "Error in inla" = list(
    "en" = "Error in inla",
    "pt-br" = "Erro no Inla"
  ),
  "Inla has crashed. Try edit the fixed priors and/or the hyperpriors and rerun" = list(
    "en" = "Inla has crashed. Try edit the fixed priors and/or the hyperpriors and rerun",
    "pt-br" = "O Inla travou. Tente editar as prioris dos efeitos fixos e/ou das hiperprioris e rode novamente"
  )
)

