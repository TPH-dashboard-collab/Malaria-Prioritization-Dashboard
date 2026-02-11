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

  # Sidebar with filters
  dashboardSidebar(
    
    # Region filter with multiple selection
    selectInput("region", "Region:", 
                choices = c("All" = "all"), 
                selected = "all",
                multiple = TRUE),
    
    # Year range slider
    sliderInput("year_range", "Year:", 
                min = 2026, max = 2030, 
                value = c(2026, 2030), 
                step = 1, sep = "")
  ),
  
  # Body - Single tab layout
  dashboardBody(
    
    # Row 1: Value boxes (2 instead of 3)
    fluidRow(
      valueBoxOutput("cases_averted", width = 6),
      valueBoxOutput("severe_averted", width = 6)
    ),
    
    # Row 2: Ranking chart
    fluidRow(
      box(title = "Top 15 Regions by Severe Cases Averted",
          width = 12,
          plotlyOutput("ranking_chart", height = 500))
    ),
    
    # Row 3: Geographic map
    fluidRow(
      box(title = "Geographic Distribution",
          width = 12,
          leafletOutput("priority_map", height = 500))
    )
  )
)