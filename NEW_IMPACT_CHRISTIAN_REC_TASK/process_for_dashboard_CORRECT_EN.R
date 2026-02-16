# ==============================================================================
# DASHBOARD DATA PROCESSING - QUANTILES AND RANKINGS
# Following Roland's document exactly
# ==============================================================================

library(data.table)
library(dplyr)

# ==============================================================================
# STEP 2: CALCULATE QUANTILES
# ==============================================================================

calculate_quantiles_correct <- function(impacts) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  CALCULATING QUANTILES                                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("STEP 1: Calculate mean IMPACT per district, EIR_mean, age_group\n")
  
  # Filter only EIR_mean
  impacts_mean <- impacts %>%
    filter(EIR_CI == "EIR_mean")
  
  # Calculate mean per district, age_group, intervention
  avg_impact <- impacts_mean %>%
    group_by(intervention, admin_2, age_group) %>%
    summarise(
      mean_impact = mean(impact, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    )
  
  cat("✓ Means calculated:", nrow(avg_impact), "rows\n\n")
  
  cat("STEP 2: Calculate quantiles per age_group\n")
  
  # Calculate quantiles within each age_group and intervention
  quantiles_data <- avg_impact %>%
    group_by(intervention, age_group) %>%
    mutate(
      # Calculate quantiles
      quantile_value = cut(
        mean_impact,
        breaks = quantile(mean_impact, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE),
        labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
        include.lowest = TRUE
      ),
      quantile_numeric = as.numeric(cut(
        mean_impact,
        breaks = quantile(mean_impact, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE),
        include.lowest = TRUE
      ))
    ) %>%
    ungroup()
  
  cat("✓ Quantiles assigned\n\n")
  
  # Summary
  cat("Summary by quantile:\n")
  summary_table <- quantiles_data %>%
    group_by(quantile_value) %>%
    summarise(n = n(), .groups = "drop")
  print(summary_table)
  cat("\n")
  
  return(quantiles_data)
}

# ==============================================================================
# STEP 3: CREATE RANKINGS
# ==============================================================================

create_rankings_correct <- function(impacts) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  CREATING RANKINGS                                         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Filter EIR_mean
  impacts_mean <- impacts %>%
    filter(EIR_CI == "EIR_mean")
  
  # Calculate statistics per district, age_group, intervention
  stats_by_group <- impacts_mean %>%
    group_by(intervention, admin_2, age_group) %>%
    summarise(
      mean_impact = mean(impact, na.rm = TRUE),
      min_impact = min(impact, na.rm = TRUE),
      max_impact = max(impact, na.rm = TRUE),
      n_seeds = n(),
      .groups = "drop"
    )
  
  # Calculate min/max across age_groups and seeds
  # For each intervention + district
  rankings <- stats_by_group %>%
    group_by(intervention, admin_2) %>%
    summarise(
      mean_impact = mean(mean_impact, na.rm = TRUE),
      min_impact = min(min_impact, na.rm = TRUE),
      max_impact = max(max_impact, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Rank within each intervention
    group_by(intervention) %>%
    arrange(desc(mean_impact)) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  cat("✓ Rankings created\n")
  cat("  Interventions:", length(unique(rankings$intervention)), "\n")
  cat("  Districts per intervention:", nrow(rankings) / length(unique(rankings$intervention)), "\n\n")
  
  # Show top 5 per intervention
  cat("Top 5 per intervention:\n\n")
  for(interv in unique(rankings$intervention)) {
    cat(interv, ":\n")
    top5 <- rankings %>%
      filter(intervention == interv) %>%
      head(5) %>%
      select(rank, admin_2, mean_impact)
    print(top5)
    cat("\n")
  }
  
  return(rankings)
}

# ==============================================================================
# MAIN FUNCTION: PROCESS EVERYTHING
# ==============================================================================

process_for_dashboard <- function(impacts_file = "impacts_calculated.csv") {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  COMPLETE DASHBOARD PROCESSING                             ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Load impacts
  cat("Loading impacts...\n")
  impacts <- fread(impacts_file)
  cat("✓ Loaded:", nrow(impacts), "rows\n")
  
  # Calculate quantiles
  quantiles_data <- calculate_quantiles_correct(impacts)
  fwrite(quantiles_data, "quantiles_for_dashboard.csv")
  cat("✓ Quantiles saved: quantiles_for_dashboard.csv\n")
  
  # Create rankings
  rankings_data <- create_rankings_correct(impacts)
  fwrite(rankings_data, "rankings_for_dashboard.csv")
  cat("✓ Rankings saved: rankings_for_dashboard.csv\n")
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PROCESSING COMPLETE!                                      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("Files generated:\n")
  cat("  1. quantiles_for_dashboard.csv\n")
  cat("  2. rankings_for_dashboard.csv\n\n")
  
  return(list(
    quantiles = quantiles_data,
    rankings = rankings_data
  ))
}

# ==============================================================================
# EXECUTION SCRIPT
# ==============================================================================

if(FALSE) {  # Change to TRUE to run
  
  # Process everything
  results <- process_for_dashboard("impacts_calculated.csv")
  
  # View results
  print(head(results$quantiles, 20))
  print(head(results$rankings, 20))
}
