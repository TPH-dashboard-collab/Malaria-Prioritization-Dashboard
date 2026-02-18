library(shiny)
library(leaflet)
library(plotly)
library(dplyr)

source("../utils/calculate_quantiles.R")

server <- function(input, output, session) {

  dados <- reactive({
    req(input$run)
    readRDS("../data/arquivo_compacto.rds")
  })

  observe({
    d <- dados()
    updateSelectInput(session, "region", choices = unique(d$region))
    updateSelectInput(session, "interv", choices = unique(d$intervention))
  })

  dados_filtrados <- reactive({
    dados() |>
      filter(region       == input$region,
             intervention == input$interv,
             year         == input$year)
  })

  output$map <- renderLeaflet({
    leaflet() |> addTiles()
  })

  output$ranking <- renderPlotly({
    df <- dados_filtrados()
    plot_ly(df, x = ~district, y = ~impact_score, type = "bar")
  })

  output$quantile_table <- renderTable({
    calcular_quantis(dados_filtrados()$impact_score)
  })
}

