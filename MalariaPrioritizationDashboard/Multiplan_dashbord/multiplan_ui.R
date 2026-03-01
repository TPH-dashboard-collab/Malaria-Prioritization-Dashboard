# ==============================================================================
# ui.R — Multi-Plan Dashboard (NSP + BAU + Customized)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Tanzania Prioritization Dashboard"),
  
  # ============================================================================
  # SIDEBAR
  # ============================================================================
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Home",     tabName = "home",     icon = icon("home")),
      menuItem("🗺️ Maps",     tabName = "maps",     icon = icon("map")),
      menuItem("📊 Rankings", tabName = "rankings", icon = icon("chart-bar")),
      menuItem("📋 Data",     tabName = "data",     icon = icon("table"))
    ),
    
    hr(),
    h4("Filters:", style = "padding: 0 15px;"),
    
    # ── Plan selector ──────────────────────────────────────────────
    selectInput(
      "plan_filter",
      "Plan:",
      choices = setNames(ALL_PLANS, ALL_PLANS),
      selected = "NSP"
    ),
    
    # ── Age selector ───────────────────────────────────────────────
    selectInput(
      "age_filter",
      "Age Group:",
      choices  = c("All" = "all", sort(unique(avg_impact_all$age_group))),
      selected = "all"
    ),
    
    hr(),
    
    # Info box that updates with selected plan
    uiOutput("sidebar_info")
  ),
  
  # ============================================================================
  # BODY
  # ============================================================================
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { background: #ffffff; }
        .box { margin-bottom: 20px; }
        .small { font-size: 11px; color: #777; }
        .plan-badge {
          display: inline-block;
          padding: 3px 10px;
          border-radius: 12px;
          font-weight: bold;
          font-size: 12px;
          color: white;
          margin-left: 8px;
        }
        .plan-NSP         { background-color: #2980b9; }
        .plan-BAU         { background-color: #7f8c8d; }
        .plan-Customized  { background-color: #8e44ad; }
      "))
    ),
    
    tabItems(
      
      # ========================================================================
      # TAB 1: HOME
      # ========================================================================
      tabItem(tabName = "home",
              
              fluidRow(
                column(12,
                       h2(uiOutput("home_title_inline")),
                       p("Select a plan in the sidebar to explore its results.")
                )
              ),
              
              # ── Value boxes for selected plan ────────────────────────────
              fluidRow(
                valueBox(
                  value    = uiOutput("vbox_impacts"),
                  subtitle = "Impacts Calculated",
                  icon     = icon("calculator"),
                  color    = "blue", width = 4
                ),
                valueBox(
                  value    = uiOutput("vbox_interventions"),
                  subtitle = "Interventions",
                  icon     = icon("medkit"),
                  color    = "green", width = 4
                ),
                valueBox(
                  value    = uiOutput("vbox_districts"),
                  subtitle = "Districts",
                  icon     = icon("map-marker"),
                  color    = "yellow", width = 4
                )
              ),
              
              # ── Top 5 for selected plan ───────────────────────────────────
              fluidRow(
                box(
                  width = 12,
                  title = uiOutput("home_top5_title"),
                  status = "success", solidHeader = TRUE,
                  plotOutput("plot_home_top5", height = "350px")
                )
              )
      ),
      
      # ========================================================================
      # TAB 2: MAPS
      # ========================================================================
      tabItem(tabName = "maps",
              h2(uiOutput("maps_title_inline")),
              p("Districts colored by quantile: Red (low priority) → Green (high priority)"),
              
              fluidRow(
                box(
                  width = 12, title = "Map Configuration",
                  status = "primary", solidHeader = TRUE,
                  
                  fluidRow(
                    column(9,
                           uiOutput("map_interventions_ui")
                    ),
                    column(3,
                           actionButton(
                             "generate_maps", "Generate Maps",
                             icon  = icon("map"),
                             class = "btn-success btn-lg btn-block",
                             style = "margin-top: 0px;"
                           )
                    )
                  ),
                  
                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 5px;",
                    p(class = "small",
                      "ℹ️ Changing the ", strong("Plan"), " or ", strong("Age Group"),
                      " filters updates the maps automatically after clicking 'Generate Maps'.")
                  ),
                  
                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                    h5("📊 Quantile Color Scale:"),
                    tags$ul(style = "margin: 5px 0;",
                            tags$li("🟢 Dark green (80-100%): ", strong("HIGHEST PRIORITY")),
                            tags$li("🟢 Light green (60-80%): High impact"),
                            tags$li("🟡 Yellow (40-60%): Medium impact"),
                            tags$li("🟠 Orange (20-40%): Low impact"),
                            tags$li("🔴 Red (0-20%): Lowest priority")
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
              h2(uiOutput("rankings_page_title")),
              p("Bars show mean impact; error bars show min/max range across age groups"),
              
              fluidRow(
                box(
                  width = 12, title = "Configuration",
                  status = "primary", solidHeader = TRUE,
                  
                  fluidRow(
                    column(8,
                           uiOutput("ranking_interventions_ui")
                    ),
                    column(4,
                           sliderInput(
                             "top_n", "Top N districts:",
                             min = 5, max = 10, value = 10, step = 1
                           )
                    )
                  ),
                  
                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background-color: #d4edda; border-radius: 5px;",
                    p(class = "small",
                      "✅ Both ", strong("Plan"), " and ", strong("Age Group"),
                      " filters update rankings automatically.")
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = uiOutput("rankings_box_title"),
                  status = "success", solidHeader = TRUE,
                  plotOutput("plot_rankings", height = "auto")
                )
              )
      ),
      
      # ========================================================================
      # TAB 4: DATA
      # ========================================================================
      tabItem(tabName = "data",
              h2("Data Tables"),
              p(uiOutput("data_plan_label")),
              
              fluidRow(
                box(width = 12, title = "Quantiles Data",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("table_quantiles"))
              ),
              fluidRow(
                box(width = 12, title = "Rankings Data",
                    status = "info", solidHeader = TRUE,
                    DTOutput("table_rankings"))
              ),
              fluidRow(
                box(width = 12, title = "Raw Impacts Data",
                    status = "warning", solidHeader = TRUE,
                    DTOutput("table_impacts"))
              )
      )
    )
  )
)