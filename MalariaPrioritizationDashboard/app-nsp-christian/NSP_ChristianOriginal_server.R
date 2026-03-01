# DASHBOARD: NSP Prioritization — Christian's Original Functions (per_interv_impact)

server <- function(input, output, session) {


  # ============================================================================
  #  HOME — Top 5 Chart
  # ============================================================================

  output$plot_home_top5 <- renderPlot({
    top5 <- impacts_nsp |>
      group_by(intervention) |>
      summarise(mean_impact = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(mean_impact)) |>
      head(5)

    ggplot(top5, aes(x = reorder(intervention, mean_impact), y = mean_impact)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = format(round(mean_impact), big.mark = ",")),
                hjust = -0.1, size = 5) +
      coord_flip() +
      labs(
        title    = "Interventions with Highest Mean Impact (NSP)",
        subtitle = "Cumulative Cases Averted (2026-2030)",
        x = "", y = "Cases Averted"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title         = element_text(face = "bold", size = 16),
        panel.grid.major.y = element_blank()
      )
  })

  # ============================================================================
  # REACTIVE: avg_impact filtered by age
  #Shared by Maps and Rankings — automatically reacts to age_filter
  # ============================================================================

  avg_impact_filtered <- reactive({
    if (input$age_filter == "all") {
      avg_impact |>
        group_by(intervention, admin_2) |>
        summarise(
          mean_impact = mean(mean_impact, na.rm = TRUE),
          min_impact  = min(mean_impact,  na.rm = TRUE),
          max_impact  = max(mean_impact,  na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      avg_impact |>
        filter(age_group == input$age_filter) |>
        group_by(intervention, admin_2) |>
        summarise(
          mean_impact = mean(mean_impact, na.rm = TRUE),
          min_impact  = min(mean_impact,  na.rm = TRUE),
          max_impact  = max(mean_impact,  na.rm = TRUE),
          .groups = "drop"
        )
    }
  })

  # ============================================================================
  # REACTIVE: Recalculated quantiles after applying the age filter
  # ============================================================================

  quantiles_filtered <- reactive({
    if (input$age_filter == "all") {
      quantiles_data
    } else {
      avg_impact |>
        filter(age_group == input$age_filter) |>
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
    }
  })

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
             <strong>Intervention:</strong> %s<br/>
             <strong>Mean Impact:</strong> %s cases<br/>
             <strong>Quantile:</strong> %s<br/>
             <strong>Priority:</strong> %s",
            data_interv$admin_2,
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
              weight           = 2,
              opacity          = 1,
              color            = "white",
              fillOpacity      = 0.7,
              highlightOptions = highlightOptions(
                weight = 3, color = "#666",
                fillOpacity = 0.9, bringToFront = TRUE
              ),
              label        = labels,
              labelOptions = labelOptions(
                style     = list("font-weight" = "normal", padding = "3px 8px"),
                textsize  = "13px",
                direction = "auto"
              )
            ) |>
            addLegend(
              pal      = pal,
              values   = ~quantile_numeric,
              opacity  = 0.7,
              title    = "Priority<br/>Quantile",
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
  # RANKINGS — automatically react to age
  # ============================================================================

  output$rankings_title <- renderUI({
    age_label <- switch(input$age_filter,
      "all"   = "All age groups (mean of 0-5 and 0-100)",
      "0-5"   = "Age group: 0-5 years (children under 5)",
      "0-100" = "Age group: 0-100 years (full population)"
    )
    paste("Rankings with Confidence Intervals —", age_label)
  })

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

    ggplot(data_filtered, aes(x = reorder(admin_2, mean_impact), y = mean_impact)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8, width = 0.7) +
      geom_errorbar(
        aes(ymin = min_impact, ymax = max_impact),
        width     = 0.25,
        color     = "gray30",
        linewidth = 0.8
      ) +
      geom_text(
        aes(label = format(round(mean_impact), big.mark = ",")),
        hjust = -0.1, size = 3.5, color = "gray20"
      ) +
      facet_wrap(~ intervention, scales = "free", ncol = n_cols) +
      coord_flip() +
      labs(
        title    = paste("Top", input$top_n, "Districts by Intervention (NSP Plan)"),
        subtitle = paste("Bars = mean impact | Error bars = min-max range |", age_subtitle),
        x = "",
        y = "Cumulative Cases Averted (mean)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title         = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle      = element_text(size = 11, hjust = 0.5, color = "gray40"),
        strip.text         = element_text(size = 13, face = "bold", color = "#2E75B5"),
        strip.background   = element_rect(fill = "gray95", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        axis.text.y        = element_text(size = 10)
      )
  }, height = function() {
    n      <- length(input$interventions_ranking)
    n_rows <- ceiling(n / 2)
    max(400, n_rows * 350)
  })

  # ============================================================================
  # DATA TABLES
  # ============================================================================

  output$table_quantiles <- renderDT({
    datatable(
      quantiles_data |>
        mutate(mean_impact = round(mean_impact, 1)) |>
        select(intervention, admin_2, age_group, mean_impact, quantile_value, quantile_numeric) |>
        arrange(intervention, desc(mean_impact)),
      options  = list(pageLength = 20, order = list(list(0, 'asc'), list(3, 'desc'))),
      rownames = FALSE, filter = "top",
      caption  = "Quantiles: Districts grouped into 5 priority levels per intervention and age group"
    )
  })

  output$table_rankings <- renderDT({
    datatable(
      rankings_data |>
        mutate(across(where(is.numeric), ~round(., 1))) |>
        arrange(intervention, rank),
      options  = list(pageLength = 20, order = list(list(0, 'asc'), list(5, 'asc'))),
      rownames = FALSE, filter = "top",
      caption  = "Rankings: Districts ranked by mean impact with confidence intervals"
    )
  })

  output$table_impacts <- renderDT({
    datatable(
      impacts_nsp |>
        mutate(value = round(value, 1)) |>
        arrange(intervention, admin_2, age_group),
      options  = list(pageLength = 25),
      rownames = FALSE, filter = "top",
      caption  = "Raw Impacts: All calculated intervention impacts (NSP plan)"
    )
  })
}