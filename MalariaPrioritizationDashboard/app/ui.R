library(shiny)
library(leaflet)
library(plotly)

ui <- fluidPage(
  titlePanel("Malaria Prioritization Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Regiao",       choices = NULL),
      selectInput("interv", "Intervencao",  choices = NULL),
      sliderInput("year",   "Ano", min = 2015, max = 2025, value = 2022, sep = ""),
      actionButton("run",   "Actualizar", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",    leafletOutput("map",           height = 500)),
        tabPanel("Ranking", plotlyOutput("ranking",        height = 450)),
        tabPanel("Quantis", tableOutput("quantile_table"))
      )
    )
  )
)

