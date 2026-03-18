# ==============================================================================
# ui.R — Multi-Plan Dashboard (Restructured)
# One intervention at a time — Map + Ranking on the same page
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Tanzania Prioritization"),
  
  # ============================================================================
  # SIDEBAR
  # ============================================================================
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Home",         tabName = "home",         icon = icon("home")),
      menuItem("🔍 Explore",      tabName = "explore",      icon = icon("map")),
      menuItem("📋 Data",         tabName = "data",         icon = icon("table"))
    ),
    
    hr(),
    h4("Filters:", style = "padding: 0 15px;"),
    
    # Plan — NSP and BAU only (Customized removed, to be added later)
    selectInput("plan_filter", "Plan:",
                choices  = c("NSP", "BAU"),
                selected = "NSP"),
    
    # Age
    selectInput("age_filter", "Age Group:",
                choices  = c("All ages" = "0-100", "Children (0-5)" = "0-5"),
                selected = "0-100"),
    
    # Metric
    selectInput("metric_filter", "Metric:",
                choices  = c("Cases Averted"           = "mean_impact",
                             "Cases Averted per 1,000" = "impact_per_1000"),
                selected = "mean_impact"),
    
    # Intervention — populated dynamically in server
    selectInput("intervention_filter", "Intervention:",
                choices  = NULL,
                selected = NULL),
    
    # Top N
    sliderInput("top_n", "Top N districts:",
                min = 3, max = 10, value = 5, step = 1),
    
    hr(),
    uiOutput("sidebar_info")
  ),
  
  # ============================================================================
  # BODY
  # ============================================================================
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { background: #ffffff; }
        .box { margin-bottom: 15px; }
        .rank-badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 10px;
          font-weight: bold;
          font-size: 11px;
          color: white;
        }
        .content-wrapper { background-color: #f4f6f9; }
      "))
    ),
    
    tabItems(
      
      # ========================================================================
      # TAB 1: HOME
      # ========================================================================
      tabItem(tabName = "home",
              
              fluidRow(
                column(12,
                       h2(uiOutput("home_title")),
                       p("Select a plan, age group and metric in the sidebar. Use the Explore tab to analyse one intervention at a time.")
                )
              ),
              
              # Summary strip — total cases averted for selected plan
              fluidRow(
                column(4,
                       valueBox(
                         value    = uiOutput("vbox_total_cases"),
                         subtitle = uiOutput("vbox_total_label"),
                         icon     = icon("heartbeat"),
                         color    = "blue", width = 12
                       )
                ),
                column(4,
                       valueBox(
                         value    = uiOutput("vbox_n_interventions"),
                         subtitle = "Interventions in this plan",
                         icon     = icon("medkit"),
                         color    = "green", width = 12
                       )
                ),
                column(4,
                       valueBox(
                         value    = uiOutput("vbox_n_districts"),
                         subtitle = "Districts covered",
                         icon     = icon("map-marker"),
                         color    = "yellow", width = 12
                       )
                )
              ),
              
              # Top interventions chart
              fluidRow(
                box(
                  width = 12,
                  title = uiOutput("home_chart_title"),
                  status = "primary", solidHeader = TRUE,
                  plotOutput("plot_home_top", height = "380px")
                )
              )
      ),
      
      # ========================================================================
      # TAB 2: EXPLORE — Map + Ranking side by side per intervention
      # ========================================================================
      tabItem(tabName = "explore",
              
              fluidRow(
                column(12,
                       h2(uiOutput("explore_title")),
                       uiOutput("explore_subtitle")
                )
              ),
              
              # ── Rank colour legend (shared between map and chart) ──────────────
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = FALSE,
                  collapsible = FALSE,
                  div(style = "display: flex; align-items: center; gap: 20px; flex-wrap: wrap; padding: 5px 0;",
                      strong("Priority rank:"),
                      div(style = "display:flex; align-items:center; gap:6px;",
                          div(style = "width:18px;height:18px;background:#08519C;border-radius:3px;"),
                          span("Rank 1-2 (highest)")),
                      div(style = "display:flex; align-items:center; gap:6px;",
                          div(style = "width:18px;height:18px;background:#2171B5;border-radius:3px;"),
                          span("Rank 3-4")),
                      div(style = "display:flex; align-items:center; gap:6px;",
                          div(style = "width:18px;height:18px;background:#4292C6;border-radius:3px;"),
                          span("Rank 5-6")),
                      div(style = "display:flex; align-items:center; gap:6px;",
                          div(style = "width:18px;height:18px;background:#9ECAE1;border-radius:3px;"),
                          span("Rank 7-8")),
                      div(style = "display:flex; align-items:center; gap:6px;",
                          div(style = "width:18px;height:18px;background:#DEEBF7;border-radius:3px;border:1px solid #ccc;"),
                          span("Rank 9-10 (lowest)"))
                  )
                )
              ),
              
              # ── Map (left) + Bar chart (right) ────────────────────────────────
              fluidRow(
                # Map
                box(
                  width = 6,
                  title = uiOutput("map_title"),
                  status = "success", solidHeader = TRUE,
                  leafletOutput("map_explore", height = "480px")
                ),
                # Ranking bar chart
                box(
                  width = 6,
                  title = uiOutput("chart_title"),
                  status = "success", solidHeader = TRUE,
                  plotOutput("plot_ranking", height = "480px")
                )
              ),
              
              # ── Data table (collapsible, hidden by default) ────────────────────
              fluidRow(
                box(
                  width = 12,
                  title = "📋 Data — click to expand",
                  status = "info", solidHeader = FALSE,
                  collapsible = TRUE, collapsed = TRUE,
                  DTOutput("table_explore")
                )
              )
      ),
      
      # ========================================================================
      # TAB 3: DATA
      # ========================================================================
      tabItem(tabName = "data",
              h2("Full Data Tables"),
              p(uiOutput("data_plan_label")),
              
              fluidRow(
                box(width = 12, title = "All Ranks",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("table_ranks_full"))
              ),
              fluidRow(
                box(width = 12, title = "Raw Impacts",
                    status = "warning", solidHeader = TRUE,
                    DTOutput("table_impacts_full"))
              )
      )
    )
  )
)