# ==============================================================================
# SERVER - PRIORITIZATION DASHBOARD
# ==============================================================================

library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(readr)

# Carregar dados
data <- read_csv("data/tza_sample_data.csv", show_col_types = FALSE)

# Calcular impacto (BAU - NSP)
bau <- data %>% filter(plan == "BAU")
nsp <- data %>% filter(plan == "NSP")

impact_data <- bau %>%
  left_join(nsp, 
            by = c("admin_1", "admin_2", "year", "age_group"),
            suffix = c("_bau", "_nsp")) %>%
  mutate(
    deaths_averted = expectedDirectDeaths_bau - expectedDirectDeaths_nsp,
    cases_averted = nUncomp_bau - nUncomp_nsp
  )

server <- function(input, output, session) {
  
  # Atualizar filtro de região
  observe({
    regions <- unique(impact_data$admin_1)
    updateSelectInput(session, "region",
                     choices = c("All" = "all", setNames(regions, regions)))
  })
  
  # Dados filtrados
  filtered_data <- reactive({
    data <- impact_data
    
    if (input$region != "all") {
      data <- data %>% filter(admin_1 == input$region)
    }
    
    data <- data %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    return(data)
  })
  
  # Value Box: Deaths Averted
  output$deaths_averted <- renderValueBox({
    total <- sum(filtered_data()$deaths_averted, na.rm = TRUE)
    valueBox(format(round(total), big.mark = ","), 
             "Deaths Averted", 
             icon = icon("heart"), 
             color = "red")
  })
  
  # Value Box: Cases Averted
  output$cases_averted <- renderValueBox({
    total <- sum(filtered_data()$cases_averted, na.rm = TRUE)
    valueBox(format(round(total), big.mark = ","), 
             "Cases Averted", 
             icon = icon("users"), 
             color = "green")
  })
  
  # Ranking Chart
  output$ranking_chart <- renderPlotly({
    
    top15 <- filtered_data() %>%
      group_by(admin_1) %>%
      summarise(total = sum(deaths_averted, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(15) %>%
      mutate(admin_1 = factor(admin_1, levels = rev(admin_1)))
    
    plot_ly(top15, x = ~total, y = ~admin_1, 
            type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(
        xaxis = list(title = "Deaths Averted"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # Map
  output$priority_map <- renderLeaflet({
    
    map_data <- filtered_data() %>%
      group_by(admin_1) %>%
      summarise(total = sum(deaths_averted, na.rm = TRUE), .groups = "drop")
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 35, lat = -6, zoom = 6) %>%
      addControl(
        html = "<div style='background: white; padding: 10px;'>
                Map will show when shapefiles are added</div>",
        position = "topright"
      )
  })
}