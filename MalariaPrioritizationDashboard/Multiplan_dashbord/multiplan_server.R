# ==============================================================================
# server.R — Multi-Plan Dashboard (NSP + BAU + Customized)
# ==============================================================================

server <- function(input, output, session) {
  
  # Plan colours
  plan_colors <- c(NSP = "#2980b9", BAU = "#7f8c8d", Customized = "#8e44ad")
  
  # Intervention colours — cor fixa por intervenção em todos os gráficos
  interv_colors <- c(
    CM       = "#2980b9",
    ICCM     = "#1abc9c",
    IRS      = "#e74c3c",
    LSM      = "#27ae60",
    PMC      = "#8e44ad",
    SMC      = "#f39c12",
    PBO_Nets = "#d35400",
    IG2_Nets = "#2c3e50",
    IPTSc    = "#16a085",
    Vaccine  = "#c0392b",
    STD_Nets = "#7f8c8d"
  )
  
  # ============================================================================
  # SIDEBAR INFO — updates with selected plan
  # ============================================================================
  
  output$sidebar_info <- renderUI({
    plan <- input$plan_filter
    n_impacts <- impacts_all |> filter(plan == !!plan) |> nrow()
    n_interv  <- impacts_all |> filter(plan == !!plan) |> pull(intervention) |> n_distinct()
    n_dist    <- impacts_all |> filter(plan == !!plan) |> pull(admin_2) |> n_distinct()
    
    col <- plan_colors[plan]
    
    tags$div(
      style = "padding: 0 15px; font-size: 11px; color: #999;",
      HTML(paste0(
        "<strong>Plan:</strong> <span style='color:", col, ";font-weight:bold;'>", plan, "</span><br>",
        "<strong>Metric:</strong> nUncompCum<br>",
        "<strong>Impacts:</strong> ", format(n_impacts, big.mark=","), "<br>",
        "<strong>Interventions:</strong> ", n_interv, "<br>",
        "<strong>Districts:</strong> ", n_dist
      ))
    )
  })
  
  # ============================================================================
  # DYNAMIC TITLES (show current plan with colour badge)
  # ============================================================================
  
  plan_badge <- reactive({
    plan <- input$plan_filter
    col  <- plan_colors[plan]
    tags$span(
      tags$span(plan, class = paste0("plan-badge plan-", plan),
                style = paste0("background-color:", col, ";"))
    )
  })
  
  output$home_title_inline     <- renderUI({ tagList("Tanzania Malaria Prioritization —", plan_badge()) })
  output$maps_title_inline     <- renderUI({ tagList("Maps — Intervention Priority by District —", plan_badge()) })
  output$rankings_page_title   <- renderUI({ tagList("Rankings — Top Districts by Intervention —", plan_badge()) })
  output$home_top5_title       <- renderUI({ paste("Top 5 Interventions by Mean Impact —", input$plan_filter, "Plan") })
  output$rankings_box_title    <- renderUI({
    age_label <- switch(input$age_filter,
                        "all"   = "All age groups",
                        "0-5"   = "Age 0-5 (children under 5)",
                        "0-100" = "Age 0-100 (full population)"
    )
    paste("Rankings —", input$plan_filter, "Plan |", age_label)
  })
  output$data_plan_label <- renderUI({
    tags$p(style = "color:#555;",
           "Showing data for plan: ",
           tags$strong(style = paste0("color:", plan_colors[input$plan_filter], ";"), input$plan_filter))
  })
  
  # ============================================================================
  # REACTIVE: filter impacts by selected plan
  # ============================================================================
  
  impacts_plan <- reactive({
    impacts_all |> filter(plan == input$plan_filter)
  })
  
  # ============================================================================
  # REACTIVE: avg_impact filtered by plan + age
  # ============================================================================
  
  avg_impact_filtered <- reactive({
    base <- avg_impact_all |> filter(plan == input$plan_filter)
    
    if (input$age_filter != "all") {
      base <- base |> filter(age_group == input$age_filter)
    }
    
    base |>
      group_by(intervention, admin_2) |>
      summarise(
        mean_impact = mean(mean_impact, na.rm = TRUE),
        min_impact  = min(mean_impact,  na.rm = TRUE),
        max_impact  = max(mean_impact,  na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # ============================================================================
  # REACTIVE: quantiles filtered by plan + age
  # ============================================================================
  
  quantiles_filtered <- reactive({
    base <- quantiles_all |> filter(plan == input$plan_filter)
    
    if (input$age_filter != "all") {
      base <- base |> filter(age_group == input$age_filter)
    }
    
    # Recalculate quantiles for the filtered subset
    base |>
      group_by(intervention) |>
      mutate(
        n_districts = n(),
        quantile_value = if (first(n_districts) < 5) {
          case_when(
            rank(mean_impact) == max(rank(mean_impact))        ~ "80-100%",
            rank(mean_impact) >= max(rank(mean_impact)) * 0.6 ~ "60-80%",
            rank(mean_impact) >= max(rank(mean_impact)) * 0.4 ~ "40-60%",
            rank(mean_impact) >= max(rank(mean_impact)) * 0.2 ~ "20-40%",
            TRUE                                               ~ "0-20%"
          )
        } else {
          as.character(
            cut(
              mean_impact,
              breaks = quantile(mean_impact, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE),
              labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
              include.lowest = TRUE
            )
          )
        },
        quantile_numeric = as.numeric(factor(
          quantile_value,
          levels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")
        ))
      ) |>
      ungroup() |>
      select(-n_districts)
  })
  
  # ============================================================================
  # VALUE BOXES
  # ============================================================================
  
  output$vbox_impacts       <- renderUI({ format(nrow(impacts_plan()), big.mark = ",") })
  output$vbox_interventions <- renderUI({ n_distinct(impacts_plan()$intervention) })
  output$vbox_districts     <- renderUI({ n_distinct(impacts_plan()$admin_2) })
  
  # ============================================================================
  # HOME — Top 5 interventions for selected plan
  # ============================================================================
  
  output$plot_home_top5 <- renderPlot({
    top5 <- impacts_plan() |>
      group_by(intervention) |>
      summarise(mean_impact = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(mean_impact)) |>
      head(5)
    
    # Usa cor por intervenção — fallback para cinzento se intervenção não estiver no mapa
    top5 <- top5 |>
      mutate(color = interv_colors[intervention] |> replace_na("#95a5a6"))
    
    ggplot(top5, aes(x = reorder(intervention, mean_impact), y = mean_impact, fill = intervention)) +
      geom_bar(stat = "identity", alpha = 0.85) +
      geom_text(aes(label = format(round(mean_impact), big.mark = ",")),
                hjust = -0.1, size = 5) +
      scale_fill_manual(values = interv_colors) +
      coord_flip() +
      labs(
        title    = paste("Top 5 Interventions —", input$plan_filter, "Plan"),
        subtitle = "Cumulative Cases Averted (2025-2030)",
        x = "", y = "Cases Averted"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title         = element_text(face = "bold", size = 16),
        panel.grid.major.y = element_blank(),
        legend.position    = "none"
      )
  })
  
  # ============================================================================
  # DYNAMIC CHECKBOXES — update choices based on selected plan
  # ============================================================================
  
  output$map_interventions_ui <- renderUI({
    choices <- sort(unique(quantiles_filtered()$intervention))
    checkboxGroupInput(
      "interventions_map",
      "Select interventions:",
      choices  = choices,
      selected = head(choices, 3),
      inline   = TRUE
    )
  })
  
  output$ranking_interventions_ui <- renderUI({
    choices <- sort(unique(avg_impact_filtered()$intervention))
    checkboxGroupInput(
      "interventions_ranking",
      "Select interventions:",
      choices  = choices,
      selected = head(choices, 3),
      inline   = TRUE
    )
  })
  
  # Limit to 6
  # observe({
  #   req(input$interventions_map)
  #   if(length(input$interventions_map) > 6) {
  #     updateCheckboxGroupInput(session, "interventions_map",
  #                              selected = head(input$interventions_map, 6))
  #     showNotification("Maximum 6 interventions for optimal display", type = "warning")
  #   }
  # })
  
  # ============================================================================
  # MAPS
  # ============================================================================
  
  selected_interventions_map <- eventReactive(input$generate_maps, {
    req(input$interventions_map)
    input$interventions_map
  })
  
  maps_data <- reactive({
    intervs <- selected_interventions_map()
    req(intervs)
    
    map_data <- shapefiles |>
      left_join(
        quantiles_filtered() |> filter(intervention %in% intervs),
        by = "admin_2"
      )
    
    list(data = map_data, interventions = intervs)
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
        status = "success", solidHeader = TRUE,
        leafletOutput(paste0("map_", i), height = "400px")
      )
    })
    
    n_rows <- ceiling(n_maps / n_cols)
    rows   <- lapply(1:n_rows, function(r) {
      start_idx <- (r - 1) * n_cols + 1
      end_idx   <- min(r * n_cols, n_maps)
      fluidRow(outputs[start_idx:end_idx])
    })
    
    do.call(tagList, rows)
  })
  
  observe({
    data <- maps_data()
    req(data)
    
    for(i in 1:length(data$interventions)) {
      local({
        idx    <- i
        interv <- data$interventions[idx]
        
        output[[paste0("map_", idx)]] <- renderLeaflet({
          data_interv <- data$data |> filter(intervention == interv)
          
          pal <- colorFactor(
            palette  = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"),
            domain   = 1:5,
            na.color = "lightgray"
          )
          
          labels <- sprintf(
            "<strong>%s</strong><br/>
             <strong>Plan:</strong> %s<br/>
             <strong>Intervention:</strong> %s<br/>
             <strong>Cases Averted:</strong> %s cases<br/>
             <strong>Quantile:</strong> %s<br/>
             <strong>Priority:</strong> %s",
            data_interv$admin_2,
            input$plan_filter,
            interv,
            format(round(data_interv$mean_impact), big.mark = ","),
            data_interv$quantile_value,
            case_when(
              data_interv$quantile_numeric == 5 ~ "HIGHEST",
              data_interv$quantile_numeric == 4 ~ "High",
              data_interv$quantile_numeric == 3 ~ "Medium",
              data_interv$quantile_numeric == 2 ~ "Low",
              data_interv$quantile_numeric == 1 ~ "Lowest",
              TRUE ~ "Unknown"
            )
          ) |> lapply(HTML)
          
          leaflet(data_interv) |>
            addProviderTiles(providers$CartoDB.Positron) |>
            addPolygons(
              fillColor        = ~pal(quantile_numeric),
              weight           = 2, opacity = 1,
              color            = "white", fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 3, color = "#666",
                fillOpacity = 0.9, bringToFront = TRUE
              ),
              label        = labels,
              labelOptions = labelOptions(
                style     = list("font-weight" = "normal", padding = "3px 8px"),
                textsize  = "13px", direction = "auto"
              )
            ) |>
            addLegend(
              pal = pal, values = ~quantile_numeric,
              opacity = 0.7, title = "Priority<br/>Quantile",
              position = "bottomright",
              labFormat = labelFormat(
                transform = function(x) {
                  lbls <- c("0-20%<br/>(Lowest)", "20-40%<br/>(Low)",
                            "40-60%<br/>(Medium)", "60-80%<br/>(High)",
                            "80-100%<br/>(HIGHEST)")
                  lbls[x]
                }
              )
            )
        })
      })
    }
  })
  
  # ============================================================================
  # RANKINGS
  # ============================================================================
  
  output$plot_rankings <- renderPlot({
    req(input$interventions_ranking)
    
    data_filtered <- avg_impact_filtered() |>
      filter(intervention %in% input$interventions_ranking) |>
      group_by(intervention) |>
      arrange(desc(mean_impact)) |>
      slice_head(n = input$top_n) |>
      ungroup()
    
    n      <- length(unique(data_filtered$intervention))
    n_cols <- if(n <= 2) n else 2
    
    age_subtitle <- switch(input$age_filter,
                           "all"   = "All ages",
                           "0-5"   = "Age 0-5 only",
                           "0-100" = "Age 0-100 (full population)"
    )
    
    ggplot(data_filtered, aes(x = reorder(admin_2, mean_impact), y = mean_impact, fill = intervention)) +
      geom_bar(stat = "identity", alpha = 0.85, width = 0.7) +
      geom_errorbar(
        aes(ymin = min_impact, ymax = max_impact),
        width = 0.25, color = "gray30", linewidth = 0.8
      ) +
      geom_text(
        aes(label = format(round(mean_impact), big.mark = ",")),
        hjust = -0.1, size = 3.5, color = "gray20"
      ) +
      scale_fill_manual(values = interv_colors) +
      facet_wrap(~ intervention, scales = "free", ncol = n_cols) +
      coord_flip() +
      labs(
        title    = paste("Top", input$top_n, "Districts —", input$plan_filter, "Plan"),
        subtitle = paste("Bars = mean impact | Error bars = min-max range |", age_subtitle),
        x = "", y = "Cumulative Cases Averted (mean)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title         = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle      = element_text(size = 11, hjust = 0.5, color = "gray40"),
        strip.text         = element_text(size = 13, face = "bold"),
        strip.background   = element_rect(fill = "gray95", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        axis.text.y        = element_text(size = 10),
        legend.position    = "none"
      )
  }, height = function() {
    req(input$interventions_ranking)
    n      <- length(input$interventions_ranking)
    n_rows <- ceiling(n / 2)
    max(400, n_rows * 350)
  })
  
  # ============================================================================
  # DATA TABLES — filtered by selected plan
  # ============================================================================
  
  output$table_quantiles <- renderDT({
    datatable(
      quantiles_all |>
        filter(plan == input$plan_filter) |>
        mutate(mean_impact = round(mean_impact, 1)) |>
        select(plan, intervention, admin_2, age_group, mean_impact, quantile_value, quantile_numeric) |>
        arrange(intervention, desc(mean_impact)),
      options  = list(pageLength = 20),
      rownames = FALSE, filter = "top",
      caption  = paste("Quantiles for plan:", input$plan_filter)
    )
  })
  
  output$table_rankings <- renderDT({
    datatable(
      rankings_all |>
        filter(plan == input$plan_filter) |>
        mutate(across(where(is.numeric), ~round(., 1))) |>
        arrange(intervention, rank),
      options  = list(pageLength = 20),
      rownames = FALSE, filter = "top",
      caption  = paste("Rankings for plan:", input$plan_filter)
    )
  })
  
  output$table_impacts <- renderDT({
    datatable(
      impacts_all |>
        filter(plan == input$plan_filter) |>
        mutate(value = round(value, 1)) |>
        arrange(intervention, admin_2, age_group),
      options  = list(pageLength = 25),
      rownames = FALSE, filter = "top",
      caption  = paste("Raw impacts for plan:", input$plan_filter)
    )
  })
}