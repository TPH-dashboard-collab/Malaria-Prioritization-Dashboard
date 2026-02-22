# DASHBOARD: NSP Prioritization — Christian's Original Functions (per_interv_impact)


ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Tanzania Prioritization Dashboard - NSP"),

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

    # Age filter — automatically affects Maps and Rankings

    selectInput(
      "age_filter",
      "Age Group:",
      choices  = c("All" = "all", sort(unique(avg_impact$age_group))),
      selected = "all"
    ),

    hr(),

    tags$div(
      style = "padding: 0 15px; font-size: 11px; color: #999;",
      HTML("<strong>Plan:</strong> NSP (National Strategic Plan)<br>
            <strong>Method:</strong> Christian's per_interv_impact()<br>
            <strong>Metric:</strong> nUncompCum<br>
            <strong>Districts:</strong> 10<br>
            <strong>Validated:</strong> Myroslava Volosko ✓")
    )
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
      "))
    ),

    tabItems(

      # ========================================================================
      # TAB 1: HOME
      # ========================================================================
      tabItem(tabName = "home",
              h2("Tanzania Malaria Intervention Prioritization"),
              h4("NSP Plan - Validated by Supervisors"),

              fluidRow(
                box(
                  width = 12, title = "Dashboard Overview",
                  status = "primary", solidHeader = TRUE,

                  h3("📋 Analytics Calculated:"),
                  tags$ol(
                    tags$li(strong("IMPACT_{intervention}:"), " Per intervention impact calculated using counterfactual analysis"),
                    tags$li(strong("Quantiles:"), " Districts divided into 5 priority groups (0-20% to 80-100%)"),
                    tags$li(strong("Rankings:"), " Districts ranked by mean impact with min/max confidence intervals")
                  ),
                  hr(),
                  h3("📊 Visualizations:"),
                  tags$ol(
                    tags$li(strong("Maps:"), " Choropleth maps with districts colored by quantile (intervention as facet)"),
                    tags$li(strong("Bar Graphs:"), " Top 10 districts per intervention with confidence intervals")
                  ),
                  hr(),
                  tags$div(
                    style = "padding: 10px; background-color: #d4edda; border-radius: 5px;",
                    tags$b("ℹ️ Age Group Filter:"),
                    " The sidebar filter affects both the Maps and Rankings tabs simultaneously.
                     Selecting '0-5' shows results only for children under 5;
                     selecting '0-100' shows the full population;
                     'All' averages both groups together."
                  )
                )
              ),

              fluidRow(
                valueBox(
                  value    = format(nrow(impacts_nsp), big.mark = ","),
                  subtitle = "Impacts Calculated (NSP)",
                  icon     = icon("calculator"),
                  color    = "blue", width = 4
                ),
                valueBox(
                  value    = length(unique(impacts_nsp$intervention)),
                  subtitle = "Interventions",
                  icon     = icon("medkit"),
                  color    = "green", width = 4
                ),
                valueBox(
                  value    = length(unique(impacts_nsp$admin_2)),
                  subtitle = "Districts",
                  icon     = icon("map-marker"),
                  color    = "yellow", width = 4
                )
              ),

              fluidRow(
                box(
                  width = 12, title = "Top 5 Interventions by Mean Impact (NSP Plan)",
                  status = "success", solidHeader = TRUE,
                  plotOutput("plot_home_top5", height = "350px")
                )
              )
      ),

      # ========================================================================
      # TAB 2: MAPS
      # ========================================================================
      tabItem(tabName = "maps",
              h2("Maps - Intervention Priority by District"),
              p("Districts colored by quantile: Red (low priority) → Green (high priority)"),

              fluidRow(
                box(
                  width = 12, title = "Map Configuration",
                  status = "primary", solidHeader = TRUE,

                  fluidRow(
                    column(9,
                           checkboxGroupInput(
                             "interventions_map",
                             "Select interventions (up to 6 for best layout):",
                             choices  = sort(unique(quantiles_data$intervention)),
                             selected = head(sort(unique(quantiles_data$intervention)), 3),
                             inline   = TRUE
                           )
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
                      "ℹ️ The ", strong("Age Group"), " filter in the sidebar updates the maps automatically.
                       Click 'Generate Maps' first, then change the age group to see it update.")
                  ),

                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                    h5("📊 Quantile Color Scale:"),
                    tags$ul(style = "margin: 5px 0;",
                            tags$li("🟢 Dark green (80-100%): ", strong("HIGHEST PRIORITY"), " - Top 20% impact"),
                            tags$li("🟢 Light green (60-80%): High impact"),
                            tags$li("🟡 Yellow (40-60%): Medium impact"),
                            tags$li("🟠 Orange (20-40%): Low impact"),
                            tags$li("🔴 Red (0-20%): Lowest priority - Bottom 20% impact")
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
              p("Bars show mean impact; error bars show min/max range"),

              fluidRow(
                box(
                  width = 12, title = "Configuration",
                  status = "primary", solidHeader = TRUE,

                  fluidRow(
                    column(8,
                           checkboxGroupInput(
                             "interventions_ranking",
                             "Select interventions (up to 6):",
                             choices  = sort(unique(rankings_data$intervention)),
                             selected = head(sort(unique(rankings_data$intervention)), 3),
                             inline   = TRUE
                           )
                    ),
                    column(4,
                           sliderInput(
                             "top_n", "Top N districts:",
                             min = 5, max = 10, value = 10, step = 1
                           ),
                           actionButton(
                             "generate_rankings", "Generate Rankings",
                             icon  = icon("chart-bar"),
                             class = "btn-success btn-block"
                           )
                    )
                  ),

                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background-color: #d4edda; border-radius: 5px;",
                    p(class = "small",
                      "✅ The ", strong("Age Group"), " filter in the sidebar also updates these rankings automatically.
                       When '0-5' is selected, rankings reflect impact on children under 5 only.
                       When '0-100' is selected, rankings reflect impact on the full population.")
                  ),

                  tags$div(
                    style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 5px;",
                    p(class = "small",
                      "📌 ", strong("Roland's Specification:"), " Bar graphs in decreasing order of IMPACT,
                       showing top 10 districts by default. Error bars represent min/max confidence intervals.")
                  )
                )
              ),

              fluidRow(
                box(
                  width = 12,
                  title = uiOutput("rankings_title"),
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
                box(width = 12, title = "Raw Impacts Data (NSP)",
                    status = "warning", solidHeader = TRUE,
                    DTOutput("table_impacts"))
              )
      )
    )
  )
)