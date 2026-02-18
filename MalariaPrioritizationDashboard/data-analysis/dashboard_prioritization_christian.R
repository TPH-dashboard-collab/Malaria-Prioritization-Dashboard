# ==============================================================================
# PRIORITIZATION DASHBOARD - USING CHRISTIAN'S METHOD
# Tanzania Malaria Intervention Prioritization
# ==============================================================================

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(DT)
library(RColorBrewer)
library(htmltools)

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Load impacts, quantiles and rankings from Christian's method
# DEPOIS (correcto)
impacts <- fread("outputs/impacts_christian_method.csv")
quantiles_data <- fread("outputs/quantiles_christian_method.csv")
rankings_data  <- fread("outputs/rankings_christian_method.csv")

# Load shapefile
shapefiles <- read_sf("../data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles <- st_transform(shapefiles, 4326)  # WGS84

cat("✓ Data loaded successfully\n")
cat("  Impacts:", nrow(impacts), "rows\n")
cat("  Quantiles:", nrow(quantiles_data), "rows\n")
cat("  Rankings:", nrow(rankings_data), "rows\n")
cat("  Shapefile districts:", nrow(shapefiles), "\n")

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Tanzania Prioritization - Christian's Method"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Home", tabName = "home", icon = icon("home")),
      menuItem("🗺️ Maps", tabName = "maps", icon = icon("map")),
      menuItem("📊 Rankings", tabName = "rankings", icon = icon("chart-bar")),
      menuItem("📈 Analysis", tabName = "analysis", icon = icon("balance-scale")),
      menuItem("📋 Data", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    
    h4("Filters:", style = "padding: 0 15px;"),
    
    selectInput(
      "age_filter",
      "Age Group:",
      choices = c("All" = "all", sort(unique(quantiles_data$age_group))),
      selected = "all"
    ),
    
    hr(),
    
    tags$div(style = "padding: 0 15px; font-size: 11px; color: #999;",
      HTML("<strong>Method:</strong> Christian's per_interv_impact<br>
            <strong>Metric:</strong> nUncompCum (cumulative to 2030)<br>
            <strong>Districts:</strong> All in dataset<br>
            <strong>Plan:</strong> Customized (full factorial)")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { background: #ffffff; }
        .box { margin-bottom: 20px; }
      "))
    ),
    
    tabItems(
      # ========================================================================
      # TAB 1: HOME
      # ========================================================================
      tabItem(tabName = "home",
        h2("Tanzania Malaria Intervention Prioritization"),
        h4("Using Christian's Sophisticated Counterfactual Method"),
        
        fluidRow(
          box(
            width = 12,
            title = "About This Dashboard",
            status = "primary",
            solidHeader = TRUE,
            
            h3("📋 Objective:"),
            p("Identify which interventions work best in each district using Christian's advanced counterfactual analysis method."),
            
            hr(),
            
            h3("🔬 Methodology (Christian's Method):"),
            tags$ol(
              tags$li(strong("Data Preparation:"), " Filter EIR_mean, calculate cumulative nUncomp by year, filter year 2030"),
              tags$li(strong("Counterfactual Analysis:"), " For each scenario, create counterfactuals by removing ONE intervention at a time"),
              tags$li(strong("Impact Calculation:"), " IMPACT = nUncompCum(with intervention) - nUncompCum(without intervention)"),
              tags$li(strong("Quantiles:"), " Divide districts into 5 priority groups (0-20% to 80-100%)"),
              tags$li(strong("Rankings:"), " Order districts by mean impact with min/max confidence intervals")
            ),
            
            hr(),
            
            h3("📊 Key Differences from Simple Method:"),
            tags$ul(
              tags$li(strong("Metric:"), " Uses nUncompCum (cumulative cases 2026-2030), not just year 2030"),
              tags$li(strong("Sophistication:"), " Proper counterfactual matching via Christian's functions"),
              tags$li(strong("Robustness:"), " Handles complex intervention combinations correctly")
            )
          )
        ),
        
        fluidRow(
          valueBox(
            value = format(nrow(impacts), big.mark = ","),
            subtitle = "Impacts Calculated",
            icon = icon("calculator"),
            color = "blue",
            width = 3
          ),
          
          valueBox(
            value = length(unique(impacts$intervention)),
            subtitle = "Interventions",
            icon = icon("medkit"),
            color = "green",
            width = 3
          ),
          
          valueBox(
            value = length(unique(impacts$admin_2)),
            subtitle = "Districts",
            icon = icon("map-marker"),
            color = "yellow",
            width = 3
          ),
          
          valueBox(
            value = "Christian",
            subtitle = "Method Used",
            icon = icon("check-circle"),
            color = "green",
            width = 3
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Top 5 Interventions by Mean Impact (Christian's Method)",
            status = "success",
            solidHeader = TRUE,
            
            renderPlot({
              top5 <- impacts %>%
                group_by(intervention) %>%
                summarise(mean_impact = mean(value, na.rm = TRUE), .groups = "drop") %>%
                arrange(desc(mean_impact)) %>%
                head(5)
              
              ggplot(top5, aes(x = reorder(intervention, mean_impact), y = mean_impact)) +
                geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
                geom_text(aes(label = format(round(mean_impact), big.mark = ",")), 
                         hjust = -0.1, size = 5) +
                coord_flip() +
                labs(
                  title = "Interventions with Highest Mean Impact (Cumulative Cases Averted)",
                  x = "",
                  y = "Cumulative Cases Averted (2026-2030)"
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(face = "bold", size = 16),
                  panel.grid.major.y = element_blank()
                )
            }, height = 350)
          )
        )
      ),
      
      # ========================================================================
      # TAB 2: MAPS
      # ========================================================================
      tabItem(tabName = "maps",
        h2("Interactive Maps - Intervention Prioritization"),
        
        fluidRow(
          box(
            width = 12,
            title = "Map Configuration",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(9,
                checkboxGroupInput(
                  "interventions_map",
                  "Select interventions:",
                  choices = sort(unique(quantiles_data$intervention)),
                  selected = head(sort(unique(quantiles_data$intervention)), 3),
                  inline = TRUE
                )
              ),
              
              column(3,
                actionButton(
                  "generate_maps",
                  "Generate Maps",
                  icon = icon("map"),
                  class = "btn-success btn-lg btn-block",
                  style = "margin-top: 0px;"
                )
              )
            ),
            
            tags$div(style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
              h5("Color Legend (Quantiles - Christian's Method):"),
              tags$ul(style = "margin: 5px 0;",
                tags$li("🟢 Dark green (80-100%): Top 20% - MAXIMUM PRIORITY"),
                tags$li("🟢 Light green (60-80%): High impact"),
                tags$li("🟡 Yellow (40-60%): Medium impact"),
                tags$li("🟠 Orange (20-40%): Low impact"),
                tags$li("🔴 Red (0-20%): Bottom 20%")
              )
            )
          )
        ),
        
        uiOutput("maps_container")
      ),
      
      # ========================================================================
      # TAB 3: RANKINGS
      # ========================================================================
      tabItem(tabName = "rankings",
        h2("Rankings - Top Districts by Intervention"),
        
        fluidRow(
          box(
            width = 12,
            title = "Configuration",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(8,
                checkboxGroupInput(
                  "interventions_ranking",
                  "Select interventions:",
                  choices = sort(unique(rankings_data$intervention)),
                  selected = head(sort(unique(rankings_data$intervention)), 3),
                  inline = TRUE
                )
              ),
              
              column(4,
                sliderInput(
                  "top_n",
                  "Number of districts:",
                  min = 5,
                  max = 10,
                  value = 10,
                  step = 1
                ),
                
                actionButton(
                  "generate_rankings",
                  "Generate Rankings",
                  icon = icon("chart-bar"),
                  class = "btn-success btn-block"
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Rankings with Confidence Intervals (Min/Max)",
            status = "success",
            solidHeader = TRUE,
            plotOutput("plot_rankings", height = "auto")
          )
        )
      ),
      
      # ========================================================================
      # TAB 4: ANALYSIS
      # ========================================================================
      tabItem(tabName = "analysis",
        h2("Comparative Analysis"),
        
        fluidRow(
          box(
            width = 3,
            title = "Configuration",
            status = "primary",
            solidHeader = TRUE,
            
            checkboxGroupInput(
              "interventions_compare",
              "Select interventions:",
              choices = sort(unique(quantiles_data$intervention)),
              selected = head(sort(unique(quantiles_data$intervention)), 4),
              inline = FALSE
            ),
            
            hr(),
            
            radioButtons(
              "comparison_type",
              "View:",
              choices = c(
                "By District" = "district",
                "By Intervention" = "intervention"
              ),
              selected = "district"
            ),
            
            actionButton(
              "compare_all",
              "Generate",
              icon = icon("chart-line"),
              class = "btn-success btn-block"
            )
          ),
          
          box(
            width = 9,
            title = "Impact Comparison (Christian's Method)",
            status = "success",
            solidHeader = TRUE,
            plotOutput("plot_comparison_all", height = "600px")
          )
        )
      ),
      
      # ========================================================================
      # TAB 5: DATA
      # ========================================================================
      tabItem(tabName = "data",
        h2("Data Tables"),
        
        fluidRow(
          box(
            width = 12,
            title = "Quantiles Data (Christian's Method)",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("table_quantiles")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Rankings Data",
            status = "info",
            solidHeader = TRUE,
            DTOutput("table_rankings")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
 
  # Generate maps
  maps_data <- eventReactive(input$generate_maps, {
    req(input$interventions_map)
    
    filtered_data <- quantiles_data %>%
      filter(intervention %in% input$interventions_map)
    
    if(input$age_filter != "all") {
      filtered_data <- filtered_data %>%
        filter(age_group == input$age_filter) %>%
        group_by(intervention) %>%
        mutate(
          quantile_numeric = as.numeric(cut(
            mean_impact,
            breaks = quantile(mean_impact, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE),
            include.lowest = TRUE
          ))
        ) %>%
        ungroup()
    }
    
    map_data <- shapefiles %>%
      left_join(filtered_data, by = "admin_2")
    
    list(data = map_data, interventions = input$interventions_map)
  })
  
  output$maps_container <- renderUI({
    data <- maps_data()
    req(data)
    
    n_maps <- length(data$interventions)
    n_cols <- if(n_maps <= 2) n_maps else if(n_maps <= 4) 2 else 3
    
    outputs <- lapply(1:n_maps, function(i) {
      box(
        width = 12 / n_cols,
        title = data$interventions[i],
        status = "success",
        solidHeader = TRUE,
        leafletOutput(paste0("map_", i), height = "400px")
      )
    })
    
    n_rows <- ceiling(n_maps / n_cols)
    rows <- lapply(1:n_rows, function(r) {
      start_idx <- (r - 1) * n_cols + 1
      end_idx <- min(r * n_cols, n_maps)
      fluidRow(outputs[start_idx:end_idx])
    })
    
    do.call(tagList, rows)
  })
  
  observe({
    data <- maps_data()
    req(data)
    
    for(i in 1:length(data$interventions)) {
      local({
        idx <- i
        interv <- data$interventions[idx]
        
        output[[paste0("map_", idx)]] <- renderLeaflet({
          data_interv <- data$data %>% filter(intervention == interv)
          
          pal <- colorFactor(
            palette = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"),
            domain = 1:5,
            na.color = "gray"
          )
          
          labels <- sprintf(
            "<strong>%s</strong><br/>Impact: %s<br/>Quantile: %s",
            data_interv$admin_2,
            format(round(data_interv$mean_impact), big.mark = ","),
            data_interv$quantile_value
          ) %>% lapply(HTML)
          
          leaflet(data_interv) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              fillColor = ~pal(quantile_numeric),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            ) %>%
            addLegend(
              pal = pal,
              values = ~quantile_numeric,
              opacity = 0.7,
              title = "Priority",
              position = "bottomright",
              labFormat = labelFormat(
                transform = function(x) c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")[x]
              )
            )
        })
      })
    }
  })
  
  # Rankings
  output$plot_rankings <- renderPlot({
    req(input$interventions_ranking)
    
    data_filtered <- rankings_data %>%
      filter(intervention %in% input$interventions_ranking) %>%
      group_by(intervention) %>%
      arrange(desc(mean_impact)) %>%
      slice_head(n = input$top_n) %>%
      ungroup()
    
    n <- length(unique(data_filtered$intervention))
    n_cols <- if(n <= 2) n else 2
    
    ggplot(data_filtered, aes(x = reorder(admin_2, mean_impact), y = mean_impact)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_errorbar(aes(ymin = min_impact, ymax = max_impact),
                   width = 0.3, color = "gray30") +
      geom_text(aes(label = format(round(mean_impact), big.mark = ",")),
               hjust = -0.1, size = 3) +
      facet_wrap(~ intervention, scales = "free", ncol = n_cols) +
      coord_flip() +
      labs(
        title = paste("Top", input$top_n, "Districts by Intervention (Christian's Method)"),
        subtitle = "Error bars show min-max range",
        x = "",
        y = "Cumulative Cases Averted (mean)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(fill = "gray95", color = NA),
        panel.grid.major.y = element_blank()
      )
  }, height = function() {
    n <- length(input$interventions_ranking)
    n_rows <- ceiling(n / 2)
    max(400, n_rows * 350)
  })
  
  # Comparative analysis
  comparison_data_all <- eventReactive(input$compare_all, {
    req(input$interventions_compare)
    
    quantiles_data %>%
      filter(intervention %in% input$interventions_compare) %>%
      {if(input$age_filter != "all") filter(., age_group == input$age_filter) else .} %>%
      group_by(intervention, admin_2) %>%
      summarise(mean_impact = mean(mean_impact, na.rm = TRUE), .groups = "drop")
  })
  
  output$plot_comparison_all <- renderPlot({
    data <- comparison_data_all()
    req(data)
    
    if(input$comparison_type == "district") {
      ggplot(data, aes(x = reorder(intervention, mean_impact), y = mean_impact, fill = intervention)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ admin_2, scales = "free_y", ncol = 2) +
        coord_flip() +
        scale_fill_brewer(palette = "Set3") +
        labs(
          title = "Impact by Intervention for Each District",
          x = "",
          y = "Cumulative Cases Averted (mean)",
          fill = "Intervention"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          legend.position = "bottom"
        )
    } else {
      ggplot(data, aes(x = reorder(admin_2, mean_impact), y = mean_impact, fill = admin_2)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ intervention, scales = "free", ncol = 3) +
        coord_flip() +
        labs(
          title = "Impact by District for Each Intervention",
          x = "",
          y = "Cumulative Cases Averted (mean)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          legend.position = "none"
        )
    }
  })
  
  # Data tables
  output$table_quantiles <- renderDT({
    datatable(
      quantiles_data %>%
        mutate(mean_impact = round(mean_impact, 1)) %>%
        select(intervention, admin_2, age_group, mean_impact, quantile_value, quantile_numeric),
      options = list(pageLength = 20),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$table_rankings <- renderDT({
    datatable(
      rankings_data %>%
        mutate(across(where(is.numeric), ~round(., 1))),
      options = list(pageLength = 20),
      rownames = FALSE,
      filter = "top"
    )
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui, server)
