# ==============================================================================
# UI - PRIORITIZATION DASHBOARD
# ==============================================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "Malaria Prioritization Dashboard"),
  
  # Sidebar com filtros
  dashboardSidebar(
    
    selectInput("region", "Region:", 
                choices = c("All" = "all"), 
                selected = "all"),
    
    sliderInput("year_range", "Year:", 
                min = 2026, max = 2030, 
                value = c(2026, 2030), 
                step = 1, sep = "")
  ),
  
  # Body - 1 ABA SÓ!
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("deaths_averted", width = 6),
      valueBoxOutput("cases_averted", width = 6)
    ),
    
    fluidRow(
      box(title = "Top 15 Regions by Deaths Averted",
          width = 12,
          plotlyOutput("ranking_chart", height = 500))
    ),
    
    fluidRow(
      box(title = "Geographic Distribution",
          width = 12,
          leafletOutput("priority_map", height = 500))
    )
  )
)