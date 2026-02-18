# ==============================================================================
# app-christian/server.R
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

server <- function(input, output, session) {

  # ----------------------------------------------------------------------------
  # Populate dropdowns dynamically from the data loaded in run_app
  # ----------------------------------------------------------------------------
  observe({
    updateSelectInput(session, "age_filter",
      choices = c("All" = "all", sort(unique(quantiles_data$age_group))))

    updateCheckboxGroupInput(session, "interventions_map",
      choices  = sort(unique(quantiles_data$intervention)),
      selected = head(sort(unique(quantiles_data$intervention)), 3))

    updateCheckboxGroupInput(session, "interventions_ranking",
      choices  = sort(unique(rankings_data$intervention)),
      selected = head(sort(unique(rankings_data$intervention)), 3))

    updateCheckboxGroupInput(session, "interventions_compare",
      choices  = sort(unique(quantiles_data$intervention)),
      selected = head(sort(unique(quantiles_data$intervention)), 4))
  })

  # ----------------------------------------------------------------------------
  # HOME TAB — value boxes and top 5 chart
  # ----------------------------------------------------------------------------
  output$vbox_impacts <- renderValueBox({
    valueBox(format(nrow(impacts), big.mark = ","),
             "Impacts Calculated", icon = icon("calculator"), color = "blue")
  })

  output$vbox_interventions <- renderValueBox({
    valueBox(length(unique(impacts$intervention)),
             "Interventions", icon = icon("medkit"), color = "green")
  })

  output$vbox_districts <- renderValueBox({
    valueBox(length(unique(impacts$admin_2)),
             "Districts", icon = icon("map-marker"), color = "yellow")
  })

  output$plot_top5 <- renderPlot({
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
      labs(title = "Interventions with Highest Mean Impact (Cumulative Cases Averted)",
           x = "", y = "Cumulative Cases Averted (2026-2030)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            panel.grid.major.y = element_blank())
  })

  # ----------------------------------------------------------------------------
  # TAB MAPS
  # ----------------------------------------------------------------------------
  maps_data <- eventReactive(input$generate_maps, {
    req(input$interventions_map)

    filtered_data <- quantiles_data %>%
      filter(intervention %in% input$interventions_map)

    if (input$age_filter != "all") {
      filtered_data <- filtered_data %>%
        filter(age_group == input$age_filter) %>%
        group_by(intervention) %>%
        mutate(
          quantile_numeric = as.numeric(cut(
            mean_impact,
            breaks = quantile(mean_impact,
                              probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                              na.rm = TRUE),
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
    n_cols <- if (n_maps <= 2) n_maps else if (n_maps <= 4) 2 else 3

    outputs <- lapply(1:n_maps, function(i) {
      box(width = 12 / n_cols, title = data$interventions[i],
          status = "success", solidHeader = TRUE,
          leafletOutput(paste0("map_", i), height = "400px"))
    })

    n_rows <- ceiling(n_maps / n_cols)
    rows <- lapply(1:n_rows, function(r) {
      start_idx <- (r - 1) * n_cols + 1
      end_idx   <- min(r * n_cols, n_maps)
      fluidRow(outputs[start_idx:end_idx])
    })

    do.call(tagList, rows)
  })

  observe({
    data <- maps_data()
    req(data)

    for (i in seq_along(data$interventions)) {
      local({
        idx    <- i
        interv <- data$interventions[idx]

        output[[paste0("map_", idx)]] <- renderLeaflet({
          data_interv <- data$data %>% filter(intervention == interv)

          pal <- colorFactor(
            palette  = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"),
            domain   = 1:5,
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
              fillColor   = ~pal(quantile_numeric),
              weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
              label        = labels,
              labelOptions = labelOptions(
                style    = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px", direction = "auto")
            ) %>%
            addLegend(
              pal = pal, values = ~quantile_numeric, opacity = 0.7,
              title = "Priority", position = "bottomright",
              labFormat = labelFormat(
                transform = function(x)
                  c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")[x])
            )
        })
      })
    }
  })

  # ----------------------------------------------------------------------------
  # TAB RANKINGS
  # ----------------------------------------------------------------------------
  output$plot_rankings <- renderPlot({
    req(input$interventions_ranking)

    data_filtered <- rankings_data %>%
      filter(intervention %in% input$interventions_ranking) %>%
      group_by(intervention) %>%
      arrange(desc(mean_impact)) %>%
      slice_head(n = input$top_n) %>%
      ungroup()

    n      <- length(unique(data_filtered$intervention))
    n_cols <- if (n <= 2) n else 2

    ggplot(data_filtered,
           aes(x = reorder(admin_2, mean_impact), y = mean_impact)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_errorbar(aes(ymin = min_impact, ymax = max_impact),
                    width = 0.3, color = "gray30") +
      geom_text(aes(label = format(round(mean_impact), big.mark = ",")),
                hjust = -0.1, size = 3) +
      facet_wrap(~intervention, scales = "free", ncol = n_cols) +
      coord_flip() +
      labs(title    = paste("Top", input$top_n,
                            "Districts by Intervention (Christian's Method)"),
           subtitle = "Error bars show min-max range",
           x = "", y = "Cumulative Cases Averted (mean)") +
      theme_minimal(base_size = 12) +
      theme(plot.title  = element_text(face = "bold", size = 16, hjust = 0.5),
            strip.text  = element_text(size = 13, face = "bold"),
            strip.background = element_rect(fill = "gray95", color = NA),
            panel.grid.major.y = element_blank())
  }, height = function() {
    n      <- length(input$interventions_ranking)
    n_rows <- ceiling(n / 2)
    max(400, n_rows * 350)
  })

  # ----------------------------------------------------------------------------
  # TAB ANALYSIS
  # ----------------------------------------------------------------------------
  comparison_data_all <- eventReactive(input$compare_all, {
    req(input$interventions_compare)

    quantiles_data %>%
      filter(intervention %in% input$interventions_compare) %>%
      { if (input$age_filter != "all") filter(., age_group == input$age_filter) else . } %>%
      group_by(intervention, admin_2) %>%
      summarise(mean_impact = mean(mean_impact, na.rm = TRUE), .groups = "drop")
  })

  output$plot_comparison_all <- renderPlot({
    data <- comparison_data_all()
    req(data)

    if (input$comparison_type == "district") {
      ggplot(data, aes(x = reorder(intervention, mean_impact),
                       y = mean_impact, fill = intervention)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~admin_2, scales = "free_y", ncol = 2) +
        coord_flip() +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Impact by Intervention for Each District",
             x = "", y = "Cumulative Cases Averted (mean)", fill = "Intervention") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
              legend.position = "bottom")
    } else {
      ggplot(data, aes(x = reorder(admin_2, mean_impact),
                       y = mean_impact, fill = admin_2)) +
        geom_bar(stat = "identity") +
        facet_wrap(~intervention, scales = "free", ncol = 3) +
        coord_flip() +
        labs(title = "Impact by District for Each Intervention",
             x = "", y = "Cumulative Cases Averted (mean)") +
        theme_minimal(base_size = 11) +
        theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
              legend.position = "none")
    }
  })

  # ----------------------------------------------------------------------------
  # TAB DATA
  # ----------------------------------------------------------------------------
  output$table_quantiles <- renderDT({
    datatable(
      quantiles_data %>%
        mutate(mean_impact = round(mean_impact, 1)) %>%
        select(intervention, admin_2, age_group,
               mean_impact, quantile_value, quantile_numeric),
      options  = list(pageLength = 20),
      rownames = FALSE,
      filter   = "top"
    )
  })

  output$table_rankings <- renderDT({
    datatable(
      rankings_data %>%
        mutate(across(where(is.numeric), ~round(., 1))),
      options  = list(pageLength = 20),
      rownames = FALSE,
      filter   = "top"
    )
  })
}