# ==============================================================================
# app-christian/ui.R
# ==============================================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Tanzania Prioritization - Christian's Method"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",     tabName = "home",     icon = icon("home")),
      menuItem("Maps",     tabName = "maps",     icon = icon("map")),
      menuItem("Rankings", tabName = "rankings", icon = icon("chart-bar")),
      menuItem("Analysis", tabName = "analysis", icon = icon("balance-scale")),
      menuItem("Data",     tabName = "data",     icon = icon("table"))
    ),
    
    hr(),
    
    h4("Filters:", style = "padding: 0 15px;"),
    
    selectInput(
      "age_filter",
      "Age Group:",
      choices = c("All" = "all"),  # populated dynamically in the server
      selected = "all"
    ),
    
    hr(),
    
    tags$div(
      style = "padding: 0 15px; font-size: 11px; color: #999;",
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
            width = 12, title = "About This Dashboard",
            status = "primary", solidHeader = TRUE,
            
            h3("Objective:"),
            p("Identify which interventions work best in each district using
               Christian's advanced counterfactual analysis method."),
            hr(),
            h3("Methodology (Christian's Method):"),
            tags$ol(
              tags$li(strong("Data Preparation:"),
                      " Filter EIR_mean, calculate cumulative nUncomp by year, filter year 2030"),
              tags$li(strong("Counterfactual Analysis:"),
                      " For each scenario, create counterfactuals by removing ONE intervention at a time"),
              tags$li(strong("Impact Calculation:"),
                      " IMPACT = nUncompCum(with intervention) - nUncompCum(without intervention)"),
              tags$li(strong("Quantiles:"),
                      " Divide districts into 5 priority groups (0-20% to 80-100%)"),
              tags$li(strong("Rankings:"),
                      " Order districts by mean impact with min/max confidence intervals")
            ),
            hr(),
            h3("Key Differences from Simple Method:"),
            tags$ul(
              tags$li(strong("Metric:"),
                      " Uses nUncompCum (cumulative cases 2026-2030), not just year 2030"),
              tags$li(strong("Sophistication:"),
                      " Proper counterfactual matching via Christian's functions"),
              tags$li(strong("Robustness:"),
                      " Handles complex intervention combinations correctly")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("vbox_impacts",       width = 3),
          valueBoxOutput("vbox_interventions", width = 3),
          valueBoxOutput("vbox_districts",     width = 3),
          valueBox("Christian", "Method Used",
                   icon = icon("check-circle"), color = "green", width = 3)
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Top 5 Interventions by Mean Impact (Christian's Method)",
            status = "success", solidHeader = TRUE,
            plotOutput("plot_top5", height = 350)
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
            width = 12, title = "Map Configuration",
            status = "primary", solidHeader = TRUE,
            
            fluidRow(
              column(9,
                checkboxGroupInput(
                  "interventions_map", "Select interventions:",
                  choices = NULL, inline = TRUE
                )
              ),
              column(3,
                actionButton("generate_maps", "Generate Maps",
                             icon = icon("map"),
                             class = "btn-success btn-lg btn-block")
              )
            ),
            
            tags$div(
              style = "margin-top:15px; padding:10px;
                       background-color:#f8f9fa; border-radius:5px;",
              h5("Color Legend (Quantiles):"),
              tags$ul(style = "margin:5px 0;",
                tags$li("Dark green  (80-100%): Top 20% - MAXIMUM PRIORITY"),
                tags$li("Light green (60-80%):  High impact"),
                tags$li("Yellow      (40-60%):  Medium impact"),
                tags$li("Orange      (20-40%):  Low impact"),
                tags$li("Red         (0-20%):   Bottom 20%")
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
            width = 12, title = "Configuration",
            status = "primary", solidHeader = TRUE,
            
            fluidRow(
              column(8,
                checkboxGroupInput(
                  "interventions_ranking", "Select interventions:",
                  choices = NULL, inline = TRUE
                )
              ),
              column(4,
                sliderInput("top_n", "Number of districts:",
                            min = 5, max = 10, value = 10, step = 1),
                actionButton("generate_rankings", "Generate Rankings",
                             icon = icon("chart-bar"),
                             class = "btn-success btn-block")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Rankings with Confidence Intervals (Min/Max)",
            status = "success", solidHeader = TRUE,
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
            width = 3, title = "Configuration",
            status = "primary", solidHeader = TRUE,
            
            checkboxGroupInput(
              "interventions_compare", "Select interventions:",
              choices = NULL, inline = FALSE
            ),
            hr(),
            radioButtons(
              "comparison_type", "View:",
              choices = c("By District"     = "district",
                          "By Intervention" = "intervention"),
              selected = "district"
            ),
            actionButton("compare_all", "Generate",
                         icon = icon("chart-line"),
                         class = "btn-success btn-block")
          ),
          
          box(
            width = 9,
            title = "Impact Comparison (Christian's Method)",
            status = "success", solidHeader = TRUE,
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
          box(width = 12, title = "Quantiles Data",
              status = "primary", solidHeader = TRUE,
              DTOutput("table_quantiles"))
        ),
        
        fluidRow(
          box(width = 12, title = "Rankings Data",
              status = "info", solidHeader = TRUE,
              DTOutput("table_rankings"))
        )
      )
    )
  )
)