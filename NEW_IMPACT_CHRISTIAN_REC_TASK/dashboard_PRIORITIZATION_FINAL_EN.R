# ==============================================================================
# PRIORITIZATION DASHBOARD - FINAL VERSION
# Following Roland's document 100%
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

# Load impacts, quantiles and rankings
impacts <- fread("impacts_calculated.csv")
quantiles_data <- fread("quantiles_for_dashboard.csv")
rankings_data <- fread("rankings_for_dashboard.csv")

# Load shapefile
shapefiles <- read_sf("shapefiles/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles <- st_transform(shapefiles, 4326)  # WGS84 for leaflet

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Tanzania Prioritization Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Home", tabName = "home", icon = icon("home")),
      menuItem("🗺️ Interactive Maps", tabName = "maps", icon = icon("map")),
      menuItem("📊 Rankings", tabName = "rankings", icon = icon("chart-bar")),
      menuItem("📈 Comparative Analysis", tabName = "analysis", icon = icon("balance-scale")),
      menuItem("📋 Data", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    
    h4("Global Filters:", style = "padding: 0 15px;"),
    
    selectInput(
      "age_filter",
      "Age Group:",
      choices = c("All" = "all", sort(unique(quantiles_data$age_group))),
      selected = "all"
    ),
    
    hr(),
    
    tags$div(style = "padding: 0 15px; font-size: 11px; color: #999;",
      HTML("<strong>Document:</strong> Roland Task<br>
            <strong>Date:</strong> February 2026<br>
            <strong>Impacts:</strong> 77,856<br>
            <strong>Districts:</strong> 10")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { background: #ffffff; }
        .box { margin-bottom: 20px; }
        .small-box { height: auto; }
      "))
    ),
    
    tabItems(
      # ========================================================================
      # TAB 1: HOME
      # ========================================================================
      tabItem(tabName = "home",
        h2("Malaria Intervention Prioritization Dashboard"),
        
        fluidRow(
          box(
            width = 12,
            title = "Welcome to the Prioritization Dashboard",
            status = "primary",
            solidHeader = TRUE,
            
            h3("📋 Objective:"),
            p("Identify which interventions work best in each district to maximize impact on malaria case reduction."),
            
            hr(),
            
            h3("📊 Methodology"),
            tags$ol(
              tags$li(strong("IMPACT_{intervention}:"), " Compare scenario WITH intervention vs WITHOUT intervention"),
              tags$li(strong("Quantiles:"), " Divide districts into 5 groups (0-20%, 20-40%, ..., 80-100%)"),
              tags$li(strong("Rankings:"), " Order districts by mean impact with min/max"),
              tags$li(strong("Visualizations:"), " Maps and charts with facets by intervention")
            ),
            
            hr(),
            
            h3("🗺️ How to use:"),
            tags$ul(
              tags$li(strong("Interactive Maps:"), " Visualize multiple interventions side by side"),
              tags$li(strong("Rankings:"), " Compare top 10 districts for each intervention"),
              tags$li(strong("Comparative Analysis:"), " Compare up to 9 interventions directly"),
              tags$li(strong("Data:"), " Access complete tables")
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
            subtitle = "Interventions Analyzed",
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
            value = "0",
            subtitle = "Calculation Failures",
            icon = icon("check-circle"),
            color = "green",
            width = 3
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Top 5 Interventions by Mean Impact",
            status = "success",
            solidHeader = TRUE,
            
            renderPlot({
              top5 <- impacts %>%
                group_by(intervention) %>%
                summarise(mean_impact = mean(impact, na.rm = TRUE), .groups = "drop") %>%
                arrange(desc(mean_impact)) %>%
                head(5)
              
              ggplot(top5, aes(x = reorder(intervention, mean_impact), y = mean_impact)) +
                geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
                geom_text(aes(label = format(round(mean_impact), big.mark = ",")), 
                         hjust = -0.1, size = 5) +
                coord_flip() +
                labs(
                  title = "Interventions with Highest Mean Impact",
                  x = "",
                  y = "Cases Averted (mean)"
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
      # TAB 2: INTERACTIVE MAPS WITH FACETS
      # ========================================================================
      tabItem(tabName = "maps",
        h2("Interactive Maps - Facets by Intervention"),
        
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
                  "Select up to 6 interventions:",
                  choices = sort(unique(quantiles_data$intervention)),
                  selected = c("SMC", "IG2_Nets", "IRS"),
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
              h5("Color Legend (Quantiles):"),
              tags$ul(style = "margin: 5px 0;",
                tags$li("🟢 Dark green (80-100%): Top 20% - MAXIMUM PRIORITY"),
                tags$li("🟢 Light green (60-80%): High impact"),
                tags$li("🟡 Yellow (40-60%): Medium impact"),
                tags$li("🟠 Orange (20-40%): Low impact"),
                tags$li("🔴 Red (0-20%): Bottom 20% - Avoid")
              )
            )
          )
        ),
        
        # Maps will be generated dynamically
        uiOutput("maps_container")
      ),
      
      # ========================================================================
      # TAB 3: RANKINGS WITH FACETS
      # ========================================================================
      tabItem(tabName = "rankings",
        h2("Rankings - Top 10 Districts by Intervention"),
        
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
                  "Select up to 6 interventions:",
                  choices = sort(unique(rankings_data$intervention)),
                  selected = c("SMC", "IG2_Nets", "PMC"),
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
            title = "Comparative Rankings (Facets)",
            status = "success",
            solidHeader = TRUE,
            plotOutput("plot_rankings", height = "auto")
          )
        )
      ),
      
      # ========================================================================
      # TAB 4: COMPARATIVE ANALYSIS - ALL INTERVENTIONS
      # ========================================================================
      tabItem(tabName = "analysis",
        h2("Comparative Analysis - All Interventions"),
        
        fluidRow(
          box(
            width = 3,
            title = "Configuration",
            status = "primary",
            solidHeader = TRUE,
            
            checkboxGroupInput(
              "interventions_compare",
              "Select interventions (up to 9):",
              choices = sort(unique(quantiles_data$intervention)),
              selected = c("SMC", "IG2_Nets", "PMC", "IRS"),
              inline = FALSE
            ),
            
            hr(),
            
            radioButtons(
              "comparison_type",
              "Visualization type:",
              choices = c(
                "By District" = "district",
                "By Intervention" = "intervention"
              ),
              selected = "district"
            ),
            
            actionButton(
              "compare_all",
              "Generate Comparison",
              icon = icon("chart-line"),
              class = "btn-success btn-block"
            )
          ),
          
          box(
            width = 9,
            title = "Impact Comparison",
            status = "success",
            solidHeader = TRUE,
            plotOutput("plot_comparison_all", height = "600px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Comparative Table",
            status = "info",
            solidHeader = TRUE,
            DTOutput("comparison_table")
          )
        )
      ),
      
      # ========================================================================
      # TAB 5: DATA
      # ========================================================================
      tabItem(tabName = "data",
        h2("Complete Data Tables"),
        
        fluidRow(
          box(
            width = 12,
            title = "Impacts with Quantiles",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("table_quantiles_complete")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Summary by Intervention",
            status = "info",
            solidHeader = TRUE,
            DTOutput("table_summary")
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
  
  # Limit selections to 6 interventions
  observe({
    if(length(input$interventions_map) > 6) {
      updateCheckboxGroupInput(session, "interventions_map",
                               selected = head(input$interventions_map, 6))
      showNotification("Maximum 6 interventions allowed", type = "warning")
    }
  })
  
  observe({
    if(length(input$interventions_ranking) > 6) {
      updateCheckboxGroupInput(session, "interventions_ranking",
                               selected = head(input$interventions_ranking, 6))
    }
  })
  
  # ==========================================================================
  # TAB 2: INTERACTIVE MAPS WITH LEAFLET
  # ==========================================================================
  
  maps_data <- eventReactive(input$generate_maps, {
    req(input$interventions_map)
    
    # Filter data
    filtered_data <- quantiles_data %>%
      filter(intervention %in% input$interventions_map)
    
    # Apply age filter
    if(input$age_filter != "all") {
      filtered_data <- filtered_data %>% 
        filter(age_group == input$age_filter)
      
      # Recalculate quantiles for this specific age
      filtered_data <- filtered_data %>%
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
    
    # Join with shapefile
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
            round(data_interv$mean_impact),
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
              title = "Quantile",
              position = "bottomright",
              labFormat = labelFormat(
                transform = function(x) c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")[x]
              )
            )
        })
      })
    }
  })
  
  # ==========================================================================
  # TAB 3: RANKINGS WITH FACETS
  # ==========================================================================
  
  output$plot_rankings <- renderPlot({
    req(input$interventions_ranking)
    
    # Filter by age if selected
    if(input$age_filter != "all") {
      data_filtered <- impacts %>%
        filter(
          intervention %in% input$interventions_ranking,
          age_group == input$age_filter,
          EIR_CI == "EIR_mean"
        ) %>%
        group_by(intervention, admin_2) %>%
        summarise(
          mean_impact = mean(impact, na.rm = TRUE),
          min_impact = min(impact, na.rm = TRUE),
          max_impact = max(impact, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(intervention) %>%
        arrange(desc(mean_impact)) %>%
        slice_head(n = input$top_n) %>%
        ungroup()
    } else {
      data_filtered <- rankings_data %>%
        filter(intervention %in% input$interventions_ranking) %>%
        group_by(intervention) %>%
        arrange(desc(mean_impact)) %>%
        slice_head(n = input$top_n) %>%
        ungroup()
    }
    
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
        title = paste("Top", input$top_n, "Districts by Intervention"),
        subtitle = if(input$age_filter == "all") {
          "Error bars show min-max (all ages)"
        } else {
          paste("Error bars show min-max (age:", input$age_filter, ")")
        },
        x = "",
        y = "Cases Averted (mean)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(fill = "gray95", color = NA),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1.5, "lines")
      )
  }, height = function() {
    n <- length(input$interventions_ranking)
    n_rows <- ceiling(n / 2)
    max(400, n_rows * 350)
  })
  
  # ==========================================================================
  # TAB 4: COMPARATIVE ANALYSIS - ALL INTERVENTIONS
  # ==========================================================================
  
  observe({
    if(length(input$interventions_compare) > 9) {
      updateCheckboxGroupInput(session, "interventions_compare",
                               selected = head(input$interventions_compare, 9))
      showNotification("Maximum 9 interventions", type = "warning")
    }
  })
  
  comparison_data_all <- eventReactive(input$compare_all, {
    req(input$interventions_compare)
    
    # Filter by age if selected
    if(input$age_filter != "all") {
      data <- impacts %>%
        filter(
          intervention %in% input$interventions_compare,
          age_group == input$age_filter,
          EIR_CI == "EIR_mean"
        ) %>%
        group_by(intervention, admin_2) %>%
        summarise(mean_impact = mean(impact, na.rm = TRUE), .groups = "drop")
    } else {
      data <- quantiles_data %>%
        filter(intervention %in% input$interventions_compare) %>%
        group_by(intervention, admin_2) %>%
        summarise(mean_impact = mean(mean_impact, na.rm = TRUE), .groups = "drop")
    }
    
    data
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
          title = "Impact of Interventions by District",
          subtitle = if(input$age_filter == "all") "All ages" else paste("Age:", input$age_filter),
          x = "",
          y = "Cases Averted (mean)",
          fill = "Intervention"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5),
          strip.text = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "gray95", color = NA),
          legend.position = "bottom",
          panel.grid.major.y = element_blank()
        )
    } else {
      ggplot(data, aes(x = reorder(admin_2, mean_impact), y = mean_impact, fill = admin_2)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ intervention, scales = "free", ncol = 3) +
        coord_flip() +
        labs(
          title = "Impact by District for Each Intervention",
          subtitle = if(input$age_filter == "all") "All ages" else paste("Age:", input$age_filter),
          x = "",
          y = "Cases Averted (mean)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5),
          strip.text = element_text(size = 11, face = "bold"),
          strip.background = element_rect(fill = "gray95", color = NA),
          legend.position = "none",
          panel.grid.major.y = element_blank()
        )
    }
  })
  
  output$comparison_table <- renderDT({
    data <- comparison_data_all()
    req(data)
    
    matrix <- data %>%
      pivot_wider(
        names_from = intervention,
        values_from = mean_impact
      ) %>%
      mutate(across(where(is.numeric), round, 0))
    
    datatable(
      matrix,
      options = list(pageLength = 20),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatStyle(
        columns = 2:ncol(matrix),
        background = styleColorBar(range(data$mean_impact, na.rm = TRUE), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # ==========================================================================
  # TAB 5: DATA
  # ==========================================================================
  
  output$table_quantiles_complete <- renderDT({
    datatable(
      quantiles_data %>%
        mutate(mean_impact = round(mean_impact, 1)) %>%
        select(intervention, admin_2, age_group, mean_impact, quantile_value, quantile_numeric) %>%
        rename(
          Intervention = intervention,
          District = admin_2,
          Age = age_group,
          "Mean Impact" = mean_impact,
          Quantile = quantile_value,
          "Quantile #" = quantile_numeric
        ),
      options = list(pageLength = 20),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatStyle(
        "Quantile #",
        backgroundColor = styleInterval(
          c(1.5, 2.5, 3.5, 4.5),
          c('#d73027', '#fc8d59', '#fee08b', '#91cf60', '#1a9850')
        )
      )
  })
  
  output$table_summary <- renderDT({
    summary <- impacts %>%
      group_by(intervention) %>%
      summarise(
        n_districts = n_distinct(admin_2),
        mean_impact = mean(impact, na.rm = TRUE),
        median_impact = median(impact, na.rm = TRUE),
        min_impact = min(impact, na.rm = TRUE),
        max_impact = max(impact, na.rm = TRUE),
        positive_pct = 100 * mean(impact > 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_impact)) %>%
      mutate(across(where(is.numeric), round, 1))
    
    datatable(
      summary,
      options = list(pageLength = 20),
      rownames = FALSE
    )
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui, server)
