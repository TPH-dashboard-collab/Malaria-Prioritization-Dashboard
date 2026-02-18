# ============================================================
#  setup_project.R
#  Execute este script UMA VEZ para criar a estrutura modular
#  do projecto Malaria-Prioritization-Dashboard
# ============================================================

# ------ 1. Raiz do projecto ---------------------------------
# Altere este caminho para o local desejado no seu computador
project_root <- file.path(getwd(), "MalariaPrioritizationDashboard")

# ------ 2. Directórios a criar ------------------------------
dirs <- c(
  "data",
  "app",
  "data-analysis/outputs",
  "utils",
  "additional"
)

message("\n📁  Criando estrutura de pastas em: ", project_root, "\n")

for (d in dirs) {
  full_path <- file.path(project_root, d)
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    message("  ✅  Criado: ", d)
  } else {
    message("  ⚠️   Já existe: ", d)
  }
}


# ------ 3. Ficheiros esqueleto ------------------------------

## ---- run_app.R (ponto de entrada) -------------------------
writeLines(
'# ============================================================
#  run_app.R  –  Ponto de entrada da aplicação Shiny
# ============================================================
library(shiny)

# Garante que os pacotes necessários estão carregados
source("utils/calculate_quantiles.R")

# Lança a app a partir da pasta app/
shiny::runApp("app", launch.browser = TRUE)
',
  file.path(project_root, "run_app.R")
)

## ---- app/ui.R ---------------------------------------------
writeLines(
'# ============================================================
#  app/ui.R
# ============================================================
library(shiny)
library(leaflet)
library(plotly)

ui <- fluidPage(

  titlePanel("Malaria Prioritization Dashboard"),

  sidebarLayout(

    sidebarPanel(
      selectInput("region",  "Região",   choices = NULL),
      selectInput("interv",  "Intervenção", choices = NULL),
      sliderInput("year",    "Ano",
                  min = 2015, max = 2025, value = 2022, sep = ""),
      actionButton("run",    "Actualizar", class = "btn-primary")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",      leafletOutput("map",    height = 500)),
        tabPanel("Ranking",   plotlyOutput("ranking", height = 450)),
        tabPanel("Quantis",   tableOutput("quantile_table")),
        tabPanel("Sobre",     includeMarkdown("../README.md"))
      )
    )
  )
)
',
  file.path(project_root, "app", "ui.R")
)

## ---- app/server.R -----------------------------------------
writeLines(
'# ============================================================
#  app/server.R
# ============================================================
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)

# Utilitários partilhados
source("../utils/calculate_quantiles.R")

server <- function(input, output, session) {

  # -- Dados reactivos ---------------------------------------
  dados <- reactive({
    req(input$run)          # só corre após clicar no botão
    # Substitua pelo caminho real do seu ficheiro de dados
    readRDS("../data/arquivo_compacto.rds")
  })

  # Preenche os dropdowns dinamicamente
  observe({
    d <- dados()
    updateSelectInput(session, "region", choices = unique(d$region))
    updateSelectInput(session, "interv", choices = unique(d$intervention))
  })

  dados_filtrados <- reactive({
    dados() |>
      filter(region      == input$region,
             intervention == input$interv,
             year         == input$year)
  })

  # -- Outputs -----------------------------------------------
  output$map <- renderLeaflet({
    # Substituir pela lógica de mapa real
    leaflet() |> addTiles()
  })

  output$ranking <- renderPlotly({
    df <- dados_filtrados()
    plot_ly(df, x = ~district, y = ~impact_score,
            type = "bar", name = "Impact Score")
  })

  output$quantile_table <- renderTable({
    calcular_quantis(dados_filtrados()$impact_score)
  })
}
',
  file.path(project_root, "app", "server.R")
)

## ---- utils/calculate_quantiles.R --------------------------
writeLines(
'# ============================================================
#  utils/calculate_quantiles.R
#  Funções reutilizáveis para cálculo de quantis
# ============================================================

#\' Calcula quantis de um vector numérico
#\'
#\' @param x       Vector numérico
#\' @param probs   Probabilidades (padrão: quintis)
#\' @return        data.frame com label e valor
calcular_quantis <- function(x,
                              probs = c(0, 0.20, 0.40, 0.60, 0.80, 1)) {
  stopifnot(is.numeric(x))
  q <- quantile(x, probs = probs, na.rm = TRUE)
  data.frame(
    quantil = names(q),
    valor   = round(unname(q), 3)
  )
}


#\' Classifica cada observação no seu quintil
#\'
#\' @param x  Vector numérico
#\' @return   Factor ordenado com o quintil de cada observação
classificar_quintil <- function(x) {
  cut(x,
      breaks         = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels         = paste0("Q", 1:5),
      include.lowest = TRUE)
}
',
  file.path(project_root, "utils", "calculate_quantiles.R")
)

## ---- data-analysis/Christian_code.R -----------------------
writeLines(
'# ============================================================
#  data-analysis/Christian_code.R
#  Método de priorização (Christian Method)
# ============================================================
library(dplyr)
library(readr)
library(openxlsx)

source("../utils/calculate_quantiles.R")

# -- 1. Carregar dados ---------------------------------------
dados <- readRDS("../data/arquivo_compacto.rds")

# -- 2. Calcular impacto por intervenção ---------------------
impacto <- dados |>
  group_by(region, district, intervention) |>
  summarise(
    cases_averted    = sum(cases_averted,    na.rm = TRUE),
    deaths_averted   = sum(deaths_averted,   na.rm = TRUE),
    impact_score     = sum(impact_score,     na.rm = TRUE),
    .groups = "drop"
  )

# -- 3. Calcular quantis e rankings --------------------------
impacto <- impacto |>
  group_by(intervention) |>
  mutate(
    quintil = classificar_quintil(impact_score),
    ranking = rank(-impact_score, ties.method = "min")
  ) |>
  ungroup()

quantis <- calcular_quantis(impacto$impact_score)

# -- 4. Guardar outputs --------------------------------------
write.xlsx(impacto,  "outputs/impacts_christian_method.xlsx",  overwrite = TRUE)
write.xlsx(quantis,  "outputs/quantiles_christian_method.xlsx", overwrite = TRUE)

rankings <- impacto |> arrange(intervention, ranking)
write.xlsx(rankings, "outputs/rankings_christian_method.xlsx",  overwrite = TRUE)

message("✅  Análise concluída. Ficheiros guardados em data-analysis/outputs/")
',
  file.path(project_root, "data-analysis", "Christian_code.R")
)

## ---- additional/check_data_10_regions.R -------------------
writeLines(
'# ============================================================
#  additional/check_data_10_regions.R
#  Validação e diagnóstico dos dados das 10 regiões
# ============================================================
library(dplyr)
library(readr)

dados <- readRDS("../data/arquivo_compacto.rds")

cat("=== Sumário geral ===\n")
cat("Linhas:", nrow(dados), " | Colunas:", ncol(dados), "\n\n")

cat("=== Regiões presentes ===\n")
print(sort(unique(dados$region)))
cat("\nTotal de regiões:", length(unique(dados$region)), "\n\n")

cat("=== Valores em falta por coluna ===\n")
na_summary <- sapply(dados, function(x) sum(is.na(x)))
print(na_summary[na_summary > 0])

cat("\n=== Verificação: exactamente 10 regiões? ===\n")
n_regioes <- length(unique(dados$region))
if (n_regioes == 10) {
  cat("✅  OK –", n_regioes, "regiões encontradas.\n")
} else {
  cat("⚠️   ATENÇÃO –", n_regioes, "regiões encontradas (esperado: 10).\n")
}
',
  file.path(project_root, "additional", "check_data_10_regions.R")
)

## ---- README.md --------------------------------------------
writeLines(
'# Malaria Prioritization Dashboard

Shiny dashboard para priorização de intervenções antimalárica.

## Estrutura do projecto

```
project/
├─ data/                         # Dados brutos (shp, rds, xlsx…)
├─ app/
│   ├─ ui.R                      # Interface Shiny
│   └─ server.R                  # Lógica do servidor
├─ data-analysis/
│   ├─ Christian_code.R          # Método de priorização principal
│   └─ outputs/                  # Resultados gerados automaticamente
├─ utils/
│   └─ calculate_quantiles.R     # Funções partilhadas de quantis
├─ additional/
│   └─ check_data_10_regions.R   # Validação de dados
└─ run_app.R                     # Ponto de entrada – executa a app
```

## Como usar

1. Coloque os seus dados na pasta `data/`
2. Abra `run_app.R` no RStudio
3. Clique em **Source** ou execute `source("run_app.R")`

## Dependências

```r
install.packages(c("shiny", "leaflet", "plotly",
                   "dplyr", "readr", "openxlsx"))
```
',
  file.path(project_root, "README.md")
)


# ------ 4. Sumário final ------------------------------------
message("\n", strrep("=", 55))
message("  🎉  Estrutura criada com sucesso!")
message(strrep("=", 55))
message("\nFicheiros gerados:")

all_files <- list.files(project_root, recursive = TRUE, full.names = FALSE)
for (f in all_files) message("   📄  ", f)

message("\nPróximo passo:")
message("  ➡️   Abra '", file.path(project_root, "run_app.R"),
        "' no RStudio e faça Source.\n")
