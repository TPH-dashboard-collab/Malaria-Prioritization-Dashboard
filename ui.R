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
                multiple = TRUE),  # Allow multiple region selection
    
    # Year range slider
    sliderInput("year_range", "Year:", 
                min = 2026, max = 2030, 
                value = c(2026, 2030), 
                step = 1, sep = "")
  ),
  
  # Body - Single tab layout
  dashboardBody(
    
    # Row 1: Value boxes
    fluidRow(
      valueBoxOutput("deaths_averted", width = 4),
      valueBoxOutput("cases_averted", width = 4),
      valueBoxOutput("severe_averted", width = 4)
    ),
    
    # Row 2: Ranking chart
    fluidRow(
      box(title = "Top 15 Regions by Deaths Averted",
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