# ==============================================================================
# SERVER - PRIORITIZATION DASHBOARD 
# ==============================================================================
library(shiny)
library(dplyr)
library(plotly)
library(leaflet)
library(readr)
library(sf)


# Load data
data <- read_csv("../data/tza_sample_data.csv", show_col_types = FALSE)

# Load shapefiles
shapefiles <- read_sf("../data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefile <- st_transform(shapefiles, 4326)

# ============================================================================
# DATA FILTERING
# ============================================================================

# FILTER: Keep only EIR_mean 
data_filtered <- data |>
  filter(EIR_CI == "EIR_mean")

cat("=== DATA FILTERING ===\n")
cat("Original rows:", nrow(data), "\n")
cat("After EIR_mean filter:", nrow(data_filtered), "\n")

# ============================================================================
# IMPACT CALCULATION
# ============================================================================

# Aggregate by region/year/age (average across seeds)
data_agg <- data_filtered |>
  group_by(plan, admin_1, admin_2, year, age_group) |>
  summarise(
    nUncomp = mean(nUncomp, na.rm = TRUE),
    nSevere = mean(nSevere, na.rm = TRUE),
    .groups = "drop"
  )

# Separate BAU and NSP
bau <- data_agg |> 
  filter(plan == "BAU") |>
  select(admin_1, admin_2, year, age_group, nUncomp, nSevere)

nsp <- data_agg |> 
  filter(plan == "NSP") |>
  select(admin_1, admin_2, year, age_group, nUncomp, nSevere)

# Calculate impact (BAU - NSP)
# NOTE: Deaths metric excluded  (2026-02-11, only because before the diference was not very clear)

impact_data <- bau |>
  inner_join(nsp,
    by = c("admin_1", "admin_2", "year", "age_group"),
    suffix = c("_bau", "_nsp")
  ) |>
  mutate(
    cases_averted = nUncomp_bau - nUncomp_nsp,
    severe_averted = nSevere_bau - nSevere_nsp
  )

# Verification
cat("\n=== IMPACT CALCULATION VERIFICATION ===\n")
cat("Total rows in impact_data:", nrow(impact_data), "\n")
cat("Cases averted (total):", format(round(sum(impact_data$cases_averted, na.rm = TRUE)), big.mark = ","), "\n")
cat("Severe cases averted (total):", format(round(sum(impact_data$severe_averted, na.rm = TRUE)), big.mark = ","), "\n")

server <- function(input, output, session) {
  
  # Update region filter choices
  observe({
    regions <- unique(impact_data$admin_1)
    updateSelectInput(session, "region",
      choices = c("All" = "all", setNames(regions, regions))
    )
  })
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    data_filtered <- impact_data
    
    # Support multiple region selection
    if (!is.null(input$region) && !"all" %in% input$region && length(input$region) > 0) {
      data_filtered <- data_filtered |> filter(admin_1 %in% input$region)
    }
    
    # Filter by year range
    data_filtered <- data_filtered |>
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    return(data_filtered)
  })
  
  # VALUE BOX 1: Cases averted
  output$cases_averted <- renderValueBox({
    total <- sum(filtered_data()$cases_averted, na.rm = TRUE)
    valueBox(
      format(round(total), big.mark = ","),
      "Cases Averted",
      icon = icon("users"),
      color = "green"
    )
  })
  
  # VALUE BOX 2: Severe cases averted
  output$severe_averted <- renderValueBox({
    total <- sum(filtered_data()$severe_averted, na.rm = TRUE)
    valueBox(
      format(round(total), big.mark = ","),
      "Severe Cases Averted",
      icon = icon("ambulance"),
      color = "orange"
    )
  })
  
  # RANKING CHART: Top 15 regions by severe cases averted
  output$ranking_chart <- renderPlotly({
    top15 <- filtered_data() |>
      group_by(admin_1) |>
      summarise(total = sum(severe_averted, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total)) |>
      head(15) |>
      mutate(admin_1 = factor(admin_1, levels = rev(admin_1)))
    
    plot_ly(top15,
      x = ~total, y = ~admin_1,
      type = "bar", orientation = "h",
      marker = list(color = "#e74c3c")
    ) |>
      layout(
        title = "Regions by Severe Cases Averted",
        xaxis = list(title = "Severe Cases Averted"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # MAP: Geographic visualization
  output$priority_map <- renderLeaflet({
    
    # Aggregate by region
    map_data <- filtered_data() |>
      group_by(admin_1) |>
      summarise(
        total_cases = sum(cases_averted, na.rm = TRUE),
        total_severe = sum(severe_averted, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with shapefile
    map_sf <- shapefile |>
      left_join(map_data, by = c("Region_Nam" = "admin_1"))
    
    # Color palette based on severe cases averted
    pal <- colorBin(
      palette = "YlOrRd",
      domain = map_sf$total_severe,
      bins = 7,
      na.color = "#808080"
    )
    
    # Create map
    leaflet(map_sf) |>
      addTiles() |>
      setView(lng = 35, lat = -6, zoom = 6) |>
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
          ifelse(is.na(total_severe), "No data", 
                 paste(format(round(total_severe), big.mark = ","), "severe cases averted"))
        ),
        popup = ~paste0(
          "<strong>", Region_Nam, "</strong><br/>",
          ifelse(is.na(total_cases), "No data available", paste0(
            "Cases Averted: ", format(round(total_cases), big.mark = ","), "<br/>",
            "Severe Cases Averted: ", format(round(total_severe), big.mark = ",")
          ))
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~total_severe,
        title = "Severe Cases Averted",
        opacity = 0.7,
        na.label = "No data"
      )
  })
}