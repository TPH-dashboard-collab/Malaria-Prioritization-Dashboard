# ==============================================================================
# server.R — Multi-Plan Dashboard (Restructured)
# ==============================================================================

server <- function(input, output, session) {
  
  blue_palette <- create_blue_palette(5)
  rank_colors <- c(
    "1-2"  = blue_palette[5],   
    "3-4"  = blue_palette[4],
    "5-6"  = blue_palette[3],
    "7-8"  = blue_palette[2],
    "9-10" = blue_palette[1]  
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
  # BASE REACTIVE — filtered ranks for selected plan + age
  # ============================================================================
  
  ranks_filtered <- reactive({
    ranks_all |>
      filter(plan == input$plan_filter, age_group == input$age_filter)
  })
  
  # For selected intervention only
  ranks_intervention <- reactive({
    req(input$intervention_filter)
    ranks_filtered() |>
      filter(intervention == input$intervention_filter)
  })
  
  # ============================================================================
  # UPDATE INTERVENTION CHOICES when plan or age changes
  # ============================================================================
  
  observe({
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
    data <- avg_impact_all |>
      filter(plan == input$plan_filter, age_group == input$age_filter) |>
      summarise(
        total_cases = sum(mean_impact, na.rm = TRUE),
        total_pop   = sum(nHost,       na.rm = TRUE)
      )
    
    if (input$metric_filter == "impact_per_1000") {
      val <- (data$total_cases / data$total_pop) * 1000
      as.character(round(val, 2))
    } else {
      format(round(data$total_cases), big.mark = ",")
    }
  })
  
  output$vbox_total_label <- renderUI({
    metric <- if (input$metric_filter == "impact_per_1000")
      "Cases Averted per 1,000 people"
    else
      "Total Cases Averted"
    paste(metric, "—", input$plan_filter, "|", age_label())
  })
  
  output$vbox_n_interventions <- renderUI({
    n_distinct(ranks_filtered()$intervention)
  })
  
  output$vbox_n_districts <- renderUI({
    n_distinct(ranks_filtered()$admin_2)
  })
  
  output$home_chart_title <- renderUI({
    paste("Cases Averted by Intervention —", input$plan_filter, "|", age_label())
  })
  
  output$plot_home_top <- renderPlot({
    
    # Filter by selected age group to avoid double counting (0-5 + 0-100)
    data <- avg_impact_all |>
      filter(plan == input$plan_filter, age_group == input$age_filter) |>
      group_by(intervention) |>
      summarise(
        total_cases = sum(mean_impact, na.rm = TRUE),
        total_pop   = sum(nHost,       na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        # Correct weighted rate: total cases averted / total population × 1000
        # This is interpretable: "per 1,000 people across all districts"
        weighted_per_1000 = (total_cases / total_pop) * 1000,
        active = if (input$metric_filter == "impact_per_1000") weighted_per_1000 else total_cases
      ) |>
      arrange(desc(active))
    
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
    req(input$intervention_filter)
    
    rc  <- rank_col()
    rnc <- rank_num_col()
    rac <- rank_abs_col()
    mc  <- metric_col()
    
    data_interv <- ranks_intervention()
    
    # Join to shapefile
    map_data <- shapefiles |>
      left_join(data_interv, by = "admin_2")
    
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
    req(input$intervention_filter)
    
    rc  <- rank_col()
    rac <- rank_abs_col()
    mc  <- metric_col()
    
    data_plot <- ranks_intervention() |>
      arrange(.data[[rac]]) |>
      slice_head(n = input$top_n) |>
      mutate(
        active_value  = .data[[mc]],
        active_rank   = .data[[rac]],
        active_group  = .data[[rc]],
        district_label = paste0("#", active_rank, " ", admin_2)
      )
    
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
    
    data <- ranks_intervention() |>
      select(plan, intervention, admin_2, age_group,
             mean_impact, impact_per_1000,
             rank_cases, rank_per_1000,
             rank_group_cases, rank_group_per_1000) |>
      arrange(rank_cases) |>
      mutate(across(where(is.numeric), ~ round(., 2)))
    
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
    datatable(
      ranks_all |>
        filter(plan == input$plan_filter, age_group == input$age_filter) |>
        select(plan, intervention, admin_2, age_group,
               mean_impact, impact_per_1000,
               rank_cases, rank_group_cases,
               rank_per_1000, rank_group_per_1000) |>
        mutate(across(where(is.numeric), ~ round(., 2))) |>
        arrange(intervention, rank_cases),
      rownames = FALSE, filter = "top",
      options  = list(pageLength = 20, scrollX = TRUE)
    )
  })
  
  output$table_impacts_full <- renderDT({
    datatable(
      impacts_all |>
        filter(plan == input$plan_filter) |>
        mutate(value = round(value, 1)) |>
        arrange(intervention, admin_2, age_group),
      rownames = FALSE, filter = "top",
      options  = list(pageLength = 25, scrollX = TRUE)
    )
  })
}