# ==============================================================================
# server.R — Multi-Plan Dashboard 
# ==============================================================================

server <- function(input, output, session) {
  
  # Colour palette — rank group → blue shades (Myro's suggestion)
  # create_blue_palette(5): rank 1-2 = darkest blue (highest priority)
  #                         rank 9-10 = lightest blue (lowest priority)
  blue_palette <- create_blue_palette(5)
  rank_colors <- c(
    "1-2"  = "#08519C",   # darkest blue  — highest priority
    "3-4"  = "#2171B5",
    "5-6"  = "#4292C6",
    "7-8"  = "#9ECAE1",
    "9-10" = "#DEEBF7"    # lightest blue — lowest priority
  )
  
  plan_colors <- c(NSP = "#2980b9", BAU = "#7f8c8d", Customized = "#8e44ad")
  
  # ============================================================================
  # HELPERS
  # ============================================================================
  
  # Active metric column name
  metric_col <- reactive({ input$metric_filter })
  
  # Active rank column name (matches metric)
  rank_col <- reactive({
    if (input$metric_filter == "impact_per_1000") "rank_group_per_1000" else "rank_group_cases"
  })
  
  rank_num_col <- reactive({
    if (input$metric_filter == "impact_per_1000") "rank_num_per_1000" else "rank_num_cases"
  })
  
  rank_abs_col <- reactive({
    if (input$metric_filter == "impact_per_1000") "rank_per_1000" else "rank_cases"
  })
  
  metric_label <- reactive({
    if (input$metric_filter == "impact_per_1000") "Cases Averted per 1,000 pop." else "Cases Averted"
  })
  
  age_label <- reactive({
    if (input$age_filter == "0-100") "All ages" else "Children (0-5)"
  })
  
  # ============================================================================
  # REACTIVE IMPACT CALCULATION — runs live when plan changes (point 5)
  # Triggered only for the selected plan — not pre-computed at startup
  # Progress bar shown while calculating
  # ============================================================================
  
  impacts_reactive <- reactiveVal(NULL)
  
  observeEvent(input$plan_filter, {
    plan_selected <- input$plan_filter
    scenario_selected <- tolower(plan_selected)  # "NSP" -> "nsp", "BAU" -> "bau"
    
    strata_plan <- strata_all[scenario_name == scenario_selected]
    
    withProgress(message = paste("Calculating impacts for", plan_selected, "..."),
                 value = 0, {
                   n <- nrow(strata_plan)
                   
                   # lapply + rbindlist — faster than for loop, native data.table approach
                   impacts_dt <- rbindlist(
                     lapply(seq_len(n), function(i) {
                       incProgress(1 / n, detail = paste(i, "/", n))
                       x <- strata_plan[i]
                       tryCatch(
                         per_interv_impact(df1,
                                           indicator = "cum_nUncomp",
                                           strata    = c(admin_2       = x$admin_2,
                                                         age_group     = x$age_group,
                                                         scenario_name = x$scenario_name,
                                                         plan          = x$plan)),
                         error = function(e) NULL
                       )
                     }),
                     fill = TRUE
                   )
                   
                   # Join plan back and rename value column
                   impacts_dt <- merge(impacts_dt,
                                       unique(strata_plan[, .(admin_2, age_group, scenario_name, plan)]),
                                       by = c("admin_2", "age_group", "scenario_name"))
                   setnames(impacts_dt, "value", "mean_impact")
                   
                   # Population weighting — data.table join
                   impacts_dt <- merge(impacts_dt, population_data,
                                       by = c("admin_2", "age_group"), all.x = TRUE)
                   impacts_dt[, impact_per_1000 := (mean_impact / nHost) * 1000]
                   
                   # Ranks — data.table grouped operation
                   impacts_dt[, rank_cases    := frank(-mean_impact,    ties.method = "min"),
                              by = .(plan, intervention, age_group)]
                   impacts_dt[, rank_per_1000 := frank(-impact_per_1000, ties.method = "min"),
                              by = .(plan, intervention, age_group)]
                   impacts_dt[, n_districts := .N,
                              by = .(plan, intervention, age_group)]
                   
                   impacts_dt[, rank_group_cases    := rank_groups(rank_cases)]
                   impacts_dt[, rank_group_per_1000 := rank_groups(rank_per_1000)]
                   impacts_dt[, rank_num_cases      := rank_numeric(rank_cases)]
                   impacts_dt[, rank_num_per_1000   := rank_numeric(rank_per_1000)]
                   
                   impacts_reactive(impacts_dt)
                 })
  }, ignoreNULL = FALSE)
  
  # ============================================================================
  # BASE REACTIVE — filtered ranks for selected plan + age
  # ============================================================================
  
  ranks_filtered <- reactive({
    req(impacts_reactive())
    impacts_reactive()[plan == input$plan_filter & age_group == input$age_filter]
  })
  
  # For selected intervention only
  ranks_intervention <- reactive({
    req(input$intervention_filter)
    ranks_filtered()[intervention == input$intervention_filter]
  })
  
  # ============================================================================
  # UPDATE INTERVENTION CHOICES when plan or age changes
  # ============================================================================
  
  observe({
    req(impacts_reactive())
    choices <- sort(unique(ranks_filtered()$intervention))
    updateSelectInput(session, "intervention_filter",
                      choices  = choices,
                      selected = choices[1])
  })
  
  # ============================================================================
  # SIDEBAR INFO
  # ============================================================================
  
  output$sidebar_info <- renderUI({
    col <- plan_colors[input$plan_filter]
    tags$div(
      style = "padding: 0 15px; font-size: 11px; color: #999;",
      HTML(paste0(
        "<strong>Plan:</strong> <span style='color:", col, ";font-weight:bold;'>",
        input$plan_filter, "</span><br>",
        "<strong>Age:</strong> ", age_label(), "<br>",
        "<strong>Metric:</strong> ", metric_label()
      ))
    )
  })
  
  # ============================================================================
  # HOME TAB
  # ============================================================================
  
  output$home_title <- renderUI({
    col <- plan_colors[input$plan_filter]
    tagList(
      "Tanzania Malaria Prioritization — ",
      tags$span(input$plan_filter,
                style = paste0("color:", col, "; font-weight:bold;"))
    )
  })
  
  output$vbox_total_cases <- renderUI({
    req(impacts_reactive())
    val <- impacts_reactive()[plan == input$plan_filter & age_group == input$age_filter,
                              sum(mean_impact, na.rm = TRUE)]
    format(round(val), big.mark = ",")
  })
  
  output$vbox_total_label <- renderUI({
    paste("Total Cases Averted —", input$plan_filter, "|", age_label())
  })
  
  output$vbox_n_interventions <- renderUI({
    req(impacts_reactive())
    uniqueN(ranks_filtered()$intervention)
  })
  
  output$vbox_n_districts <- renderUI({
    req(impacts_reactive())
    uniqueN(ranks_filtered()$admin_2)
  })
  
  output$home_chart_title <- renderUI({
    paste("Cases Averted by Intervention —", input$plan_filter, "|", age_label())
  })
  
  output$plot_home_top <- renderPlot({
    
    req(impacts_reactive())
    # Filter by selected plan + age — data.table aggregation
    data <- impacts_reactive()[plan == input$plan_filter & age_group == input$age_filter,
                               .(total_cases = sum(mean_impact, na.rm = TRUE),
                                 total_pop   = sum(nHost,       na.rm = TRUE)),
                               by = intervention]
    data[, weighted_per_1000 := (total_cases / total_pop) * 1000]
    data[, active := if (input$metric_filter == "impact_per_1000") weighted_per_1000 else total_cases]
    setorder(data, -active)
    
    y_label <- if (input$metric_filter == "impact_per_1000") {
      "Cases Averted per 1,000 people (across all districts)"
    } else {
      "Total Cases Averted (all districts)"
    }
    
    ggplot(data, aes(x = reorder(intervention, active), y = active, fill = intervention)) +
      geom_bar(stat = "identity", alpha = 0.85, width = 0.65) +
      geom_text(
        aes(label = if (input$metric_filter == "impact_per_1000")
          round(active, 2)
          else
            format(round(active), big.mark = ",")),
        hjust = -0.1, size = 4.5) +
      scale_fill_brewer(palette = "Set2") +
      coord_flip() +
      expand_limits(y = max(data$active) * 1.18) +
      labs(
        x        = "",
        y        = y_label,
        subtitle = paste(input$plan_filter, "·", age_label())
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position    = "none",
        plot.subtitle      = element_text(color = "gray50")
      )
  })
  
  # ============================================================================
  # EXPLORE TAB — titles
  # ============================================================================
  
  output$explore_title <- renderUI({
    req(input$intervention_filter)
    tagList(input$intervention_filter, " — ", input$plan_filter, " Plan")
  })
  
  output$explore_subtitle <- renderUI({
    req(input$intervention_filter)
    tags$p(style = "color:#666;",
           "Showing top", input$top_n, "districts  ·",
           age_label(), " ·", metric_label())
  })
  
  output$map_title <- renderUI({
    req(input$intervention_filter)
    paste("Priority Map —", input$intervention_filter)
  })
  
  output$chart_title <- renderUI({
    req(input$intervention_filter)
    paste("Ranking —", input$intervention_filter)
  })
  
  # ============================================================================
  # EXPLORE TAB — MAP
  # ============================================================================
  
  output$map_explore <- renderLeaflet({
    req(impacts_reactive(), input$intervention_filter)
    
    rc  <- rank_col()
    rnc <- rank_num_col()
    rac <- rank_abs_col()
    mc  <- metric_col()
    
    data_interv <- ranks_intervention()
    
    # Join to shapefile — merge sf object with data.table
    # Using base merge to avoid dplyr dependency
    map_data <- merge(shapefiles, data_interv,
                      by = "admin_2", all.x = TRUE)
    
    # Colour palette — rank group
    pal <- colorFactor(
      palette  = unname(rank_colors),
      levels   = names(rank_colors),
      na.color = "lightgray"
    )
    
    # Tooltip
    metric_vals <- if (mc == "impact_per_1000") {
      ifelse(is.na(map_data[[mc]]),
             "No data",
             as.character(round(map_data[[mc]], 2)))
    } else {
      ifelse(is.na(map_data[[mc]]),
             "No data",
             format(round(map_data[[mc]]), big.mark = ","))
    }
    
    rank_vals  <- ifelse(is.na(map_data[[rac]]), "—", paste0("#", map_data[[rac]]))
    group_vals <- ifelse(is.na(map_data[[rc]]),  "—", map_data[[rc]])
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
       <strong>Rank:</strong> %s<br/>
       <strong>Rank group:</strong> %s<br/>
       <strong>%s:</strong> %s<br/>
       <strong>Plan:</strong> %s  ·  <strong>Age:</strong> %s",
      map_data$admin_2,
      rank_vals,
      group_vals,
      metric_label(),
      metric_vals,
      input$plan_filter,
      age_label()
    ) |> lapply(HTML)
    
    leaflet(map_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor        = ~pal(map_data[[rc]]),
        weight           = 2, opacity = 1,
        color            = "white", fillOpacity = 0.75,
        highlightOptions = highlightOptions(
          weight = 3, color = "#444",
          fillOpacity = 0.95, bringToFront = TRUE
        ),
        label        = labels,
        labelOptions = labelOptions(
          style    = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px", direction = "auto"
        )
      ) |>
      addLegend(
        colors   = unname(rank_colors),
        labels   = names(rank_colors),
        opacity  = 0.8,
        title    = "Priority Rank",
        position = "bottomright"
      )
  })
  
  # ============================================================================
  # EXPLORE TAB — RANKING BAR CHART
  # ============================================================================
  
  output$plot_ranking <- renderPlot({
    req(impacts_reactive(), input$intervention_filter)
    
    rc  <- rank_col()
    rac <- rank_abs_col()
    mc  <- metric_col()
    
    data_plot <- copy(ranks_intervention())
    setorderv(data_plot, rac)
    data_plot <- head(data_plot, input$top_n)
    data_plot[, active_value   := .SD[[1]], .SDcols = mc]
    data_plot[, active_rank    := .SD[[1]], .SDcols = rac]
    data_plot[, active_group   := .SD[[1]], .SDcols = rc]
    data_plot[, district_label := paste0("#", active_rank, " ", admin_2)]
    
    y_max <- max(data_plot$active_value, na.rm = TRUE)
    
    ggplot(data_plot,
           aes(x = reorder(district_label, -active_rank),
               y = active_value,
               fill = active_group)) +
      geom_bar(stat = "identity", alpha = 0.88, width = 0.65) +
      geom_text(
        aes(label = if (mc == "impact_per_1000")
          round(active_value, 1)
          else
            format(round(active_value), big.mark = ",")),
        hjust = -0.1, size = 4, color = "gray20"
      ) +
      scale_fill_manual(
        values = rank_colors,
        breaks = names(rank_colors),
        name   = "Priority Rank"
      ) +
      coord_flip() +
      expand_limits(y = y_max * 1.18) +
      labs(
        x        = "",
        y        = metric_label(),
        subtitle = paste("Top", input$top_n, "districts ·",
                         input$plan_filter, "·", age_label())
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.subtitle      = element_text(color = "gray50", size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        legend.position    = "right",
        axis.text.y        = element_text(size = 11)
      )
  })
  
  # ============================================================================
  # EXPLORE TAB — DATA TABLE (collapsed by default)
  # ============================================================================
  
  output$table_explore <- renderDT({
    req(input$intervention_filter)
    
    cols <- c("plan", "intervention", "admin_2", "age_group",
              "mean_impact", "impact_per_1000",
              "rank_cases", "rank_per_1000",
              "rank_group_cases", "rank_group_per_1000")
    data <- copy(ranks_intervention()[, ..cols])
    setorder(data, rank_cases)
    num_cols <- names(data)[sapply(data, is.numeric)]
    data[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]
    
    datatable(data,
              rownames = FALSE,
              filter   = "top",
              options  = list(pageLength = 10, scrollX = TRUE),
              caption  = paste("Data for:", input$intervention_filter,
                               "·", input$plan_filter, "·", age_label()))
  })
  
  # ============================================================================
  # DATA TAB
  # ============================================================================
  
  output$data_plan_label <- renderUI({
    tags$p(style = "color:#555;",
           "Filtered by plan: ",
           tags$strong(style = paste0("color:", plan_colors[input$plan_filter], ";"),
                       input$plan_filter),
           " · Age: ", age_label())
  })
  
  output$table_ranks_full <- renderDT({
    req(impacts_reactive())
    cols <- c("plan", "intervention", "admin_2", "age_group",
              "mean_impact", "impact_per_1000",
              "rank_cases", "rank_group_cases",
              "rank_per_1000", "rank_group_per_1000")
    dt <- copy(impacts_reactive()[plan == input$plan_filter &
                                    age_group == input$age_filter,
                                  ..cols])
    num_cols <- names(dt)[sapply(dt, is.numeric)]
    dt[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]
    setorder(dt, intervention, rank_cases)
    datatable(dt, rownames = FALSE, filter = "top",
              options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$table_impacts_full <- renderDT({
    req(impacts_reactive())
    dt <- copy(impacts_reactive()[plan == input$plan_filter])
    dt[, mean_impact := round(mean_impact, 1)]
    setorder(dt, intervention, admin_2, age_group)
    datatable(dt, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE))
  })
}