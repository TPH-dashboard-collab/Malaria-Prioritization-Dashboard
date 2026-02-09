# ==============================================================================
# SERVER - PRIORITIZATION DASHBOARD (VERSÃO CORRIGIDA)
# ==============================================================================
library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(readr)
library(sf)

# Carregar dados
data <- read_csv("Malaria_Priorization/tza_sample_data.csv", show_col_types = FALSE)

# Carregar shapefiles
shapefile <- st_read("Malaria_Priorization/shapefiles/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp", quiet = TRUE)
shapefile <- st_transform(shapefile, 4326)

# ============================================================================
# CALCULAR IMPACTO CORRETAMENTE
# ============================================================================

# Agregar por região/ano/idade (média dos seeds)
data_agg <- data %>%
  group_by(plan, admin_1, admin_2, year, age_group) %>%
  summarise(
    expectedDirectDeaths = mean(expectedDirectDeaths, na.rm = TRUE),
    nUncomp = mean(nUncomp, na.rm = TRUE),
    nSevere = mean(nSevere, na.rm = TRUE),
    .groups = "drop"
  )

# Separar BAU e NSP (remover coluna 'plan' antes do join)
bau <- data_agg %>% 
  filter(plan == "BAU") %>%
  select(admin_1, admin_2, year, age_group, expectedDirectDeaths, nUncomp, nSevere)

nsp <- data_agg %>% 
  filter(plan == "NSP") %>%
  select(admin_1, admin_2, year, age_group, expectedDirectDeaths, nUncomp, nSevere)

# Calcular impacto
# NOTA: expectedDirectDeaths não varia entre BAU e NSP neste dataset
# Por isso usamos nSevere (casos severos) como proxy para mortalidade
impact_data <- bau %>%
  inner_join(nsp,
    by = c("admin_1", "admin_2", "year", "age_group"),
    suffix = c("_bau", "_nsp")
  ) %>%
  mutate(
    # USAR CASOS SEVEROS EVITADOS como métrica principal
    severe_averted = nSevere_bau - nSevere_nsp,
    cases_averted = nUncomp_bau - nUncomp_nsp,
    
    # Estimar mortes evitadas baseado em taxa de fatalidade de casos severos
    # Assumindo CFR (Case Fatality Rate) de ~3% para casos severos
    deaths_averted_estimated = severe_averted * 0.03
  )

# Verificação (opcional - pode comentar em produção)
cat("=== VERIFICAÇÃO DO CÁLCULO ===\n")
cat("Total de linhas em impact_data:", nrow(impact_data), "\n")
cat("Total de casos severos evitados:", format(sum(impact_data$severe_averted, na.rm = TRUE), big.mark = ","), "\n")
cat("Total de casos evitados:", format(sum(impact_data$cases_averted, na.rm = TRUE), big.mark = ","), "\n")
cat("Total de mortes evitadas (estimado):", format(round(sum(impact_data$deaths_averted_estimated, na.rm = TRUE)), big.mark = ","), "\n")

server <- function(input, output, session) {
  
  observe({
    regions <- unique(impact_data$admin_1)
    updateSelectInput(session, "region",
      choices = c("All" = "all", setNames(regions, regions))
    )
  })
  
  filtered_data <- reactive({
    data_filtered <- impact_data
    
    if (input$region != "all") {
      data_filtered <- data_filtered %>% filter(admin_1 == input$region)
    }
    
    data_filtered <- data_filtered %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    return(data_filtered)
  })
  
  # VALUE BOX 1: Mortes evitadas (estimado)
  output$deaths_averted <- renderValueBox({
    total <- sum(filtered_data()$deaths_averted_estimated, na.rm = TRUE)
    valueBox(
      format(round(total), big.mark = ","),
      "Deaths Averted (estimated)",
      icon = icon("heart"),
      color = "red"
    )
  })
  
  # VALUE BOX 2: Casos evitados
  output$cases_averted <- renderValueBox({
    total <- sum(filtered_data()$cases_averted, na.rm = TRUE)
    valueBox(
      format(round(total), big.mark = ","),
      "Cases Averted",
      icon = icon("users"),
      color = "green"
    )
  })
  
  # VALUE BOX 3 (OPCIONAL): Casos severos evitados
  output$severe_averted <- renderValueBox({
    total <- sum(filtered_data()$severe_averted, na.rm = TRUE)
    valueBox(
      format(round(total), big.mark = ","),
      "Severe Cases Averted",
      icon = icon("ambulance"),
      color = "orange"
    )
  })
  
  # RANKING CHART: Top 15 regiões por mortes evitadas
  output$ranking_chart <- renderPlotly({
    top15 <- filtered_data() %>%
      group_by(admin_1) %>%
      summarise(total = sum(deaths_averted_estimated, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(15) %>%
      mutate(admin_1 = factor(admin_1, levels = rev(admin_1)))
    
    plot_ly(top15,
      x = ~total, y = ~admin_1,
      type = "bar", orientation = "h",
      marker = list(color = "#e74c3c")
    ) %>%
      layout(
        title = "Top 15 Regions by Deaths Averted (Estimated)",
        xaxis = list(title = "Deaths Averted (Estimated)"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # MAPA: Visualização geográfica
  output$priority_map <- renderLeaflet({
    
    # Agregar por região
    map_data <- filtered_data() %>%
      group_by(admin_1) %>%
      summarise(
        total_cases = sum(cases_averted, na.rm = TRUE),
        total_severe = sum(severe_averted, na.rm = TRUE),
        total_deaths = sum(deaths_averted_estimated, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Juntar com shapefile
    map_sf <- shapefile %>%
      left_join(map_data, by = c("Region_Nam" = "admin_1"))
    
    # Substituir NA por 0
    map_sf$total_cases[is.na(map_sf$total_cases)] <- 0
    map_sf$total_severe[is.na(map_sf$total_severe)] <- 0
    map_sf$total_deaths[is.na(map_sf$total_deaths)] <- 0
    
    # Paleta de cores baseada em CASOS SEVEROS (melhor indicador)
    pal <- colorBin(
      palette = "YlOrRd",
      domain = map_sf$total_severe,
      bins = 7,
      na.color = "#808080"
    )
    
    # Criar mapa
    leaflet(map_sf) %>%
      addTiles() %>%
      setView(lng = 35, lat = -6, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(total_severe),
        fillOpacity = 0.7,
        weight = 2,
        color = "#ffffff",
        opacity = 1,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(
          Region_Nam, ": ",
          format(round(total_severe), big.mark = ","), " severe cases averted"
        ),
        popup = ~paste0(
          "<strong>", Region_Nam, "</strong><br/>",
          "Cases Averted: ", format(round(total_cases), big.mark = ","), "<br/>",
          "Severe Cases Averted: ", format(round(total_severe), big.mark = ","), "<br/>",
          "Deaths Averted (est.): ", format(round(total_deaths), big.mark = ",")
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~total_severe,
        title = "Severe Cases Averted",
        opacity = 0.7
      )
  })
}