# ==============================================================================
# INTERVENTION VERIFICATION IN 10 DISTRICTS
# ==============================================================================

library(data.table)
library(dplyr)

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  INTERVENTION VERIFICATION BY DISTRICT                       ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("1. Loading data...\n")
data_new <- fread("TZ_subset_10regions_1seed.csv")
impacts <- fread("impacts_calculated.csv")
quantiles_data <- fread("quantiles_for_dashboard.csv")
rankings_data <- fread("rankings_for_dashboard.csv")

cat("✓ Data loaded\n\n")

# ==============================================================================
# DISTRICTS IN DATASET
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  1. DISTRICTS IN DATASET                                     ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

districts <- sort(unique(data_new$admin_2))
cat("Total districts:", length(districts), "\n\n")

for(i in 1:length(districts)) {
  cat(sprintf("%2d. %s\n", i, districts[i]))
}
cat("\n")

# ==============================================================================
# AVAILABLE INTERVENTIONS
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  2. ANALYZED INTERVENTIONS                                   ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

interventions <- sort(unique(impacts$intervention))
cat("Total interventions:", length(interventions), "\n\n")

for(i in 1:length(interventions)) {
  # Count impacts calculated
  n_impacts <- nrow(impacts %>% filter(intervention == interventions[i]))
  
  # Mean impact
  mean_impact <- mean(impacts$impact[impacts$intervention == interventions[i]], na.rm = TRUE)
  
  cat(sprintf("%2d. %-12s | Impacts: %5d | Mean: %8.0f cases\n", 
             i, interventions[i], n_impacts, mean_impact))
}
cat("\n")

# ==============================================================================
# RANKING BY INTERVENTION
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  3. DISTRICT RANKINGS BY INTERVENTION                        ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

for(interv in interventions) {
  cat("──────────────────────────────────────────────────────────────\n")
  cat("INTERVENTION:", interv, "\n")
  cat("──────────────────────────────────────────────────────────────\n")
  
  ranking <- rankings_data %>%
    filter(intervention == interv) %>%
    arrange(rank) %>%
    select(rank, admin_2, mean_impact, min_impact, max_impact)
  
  for(i in 1:nrow(ranking)) {
    cat(sprintf("%2d. %-30s | Mean: %8.0f | Min: %8.0f | Max: %8.0f\n",
               ranking$rank[i],
               ranking$admin_2[i],
               ranking$mean_impact[i],
               ranking$min_impact[i],
               ranking$max_impact[i]))
  }
  cat("\n")
}

# ==============================================================================
# IMPACT MATRIX: DISTRICT x INTERVENTION
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  4. MATRIX: MEAN IMPACT (DISTRICT x INTERVENTION)           ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# Create matrix
matrix <- rankings_data %>%
  select(intervention, admin_2, mean_impact) %>%
  pivot_wider(
    names_from = intervention,
    values_from = mean_impact
  ) %>%
  arrange(admin_2)

cat("Values = Cases averted (mean impact)\n\n")
print(matrix, n = 100)
cat("\n")

# Save matrix
write.csv(matrix, "impact_matrix_district_intervention.csv", row.names = FALSE)
cat("✓ Matrix saved to: impact_matrix_district_intervention.csv\n\n")

# ==============================================================================
# QUANTILES BY DISTRICT
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  5. QUANTILES BY DISTRICT (ALL AGES)                         ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

for(district in districts) {
  cat("──────────────────────────────────────────────────────────────\n")
  cat("DISTRICT:", district, "\n")
  cat("──────────────────────────────────────────────────────────────\n")
  
  dist_data <- quantiles_data %>%
    filter(admin_2 == district) %>%
    group_by(intervention) %>%
    summarise(
      mean_impact = mean(mean_impact, na.rm = TRUE),
      quantile = first(quantile_value),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_impact))
  
  for(i in 1:nrow(dist_data)) {
    cat(sprintf("  %-12s | Impact: %8.0f | Quantile: %s\n",
               dist_data$intervention[i],
               dist_data$mean_impact[i],
               dist_data$quantile[i]))
  }
  cat("\n")
}

# ==============================================================================
# BEST INTERVENTIONS BY DISTRICT
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  6. TOP 3 INTERVENTIONS BY DISTRICT                          ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

recommendations <- data.frame()

for(district in districts) {
  cat(district, ":\n")
  
  top3 <- rankings_data %>%
    filter(admin_2 == district) %>%
    arrange(desc(mean_impact)) %>%
    head(3)
  
  for(i in 1:nrow(top3)) {
    cat(sprintf("  %d. %-12s → %8.0f cases averted\n",
               i, top3$intervention[i], top3$mean_impact[i]))
    
    # Save for report
    recommendations <- rbind(recommendations, data.frame(
      district = district,
      priority = i,
      intervention = top3$intervention[i],
      impact = top3$mean_impact[i]
    ))
  }
  cat("\n")
}

# Save recommendations
write.csv(recommendations, "recommendations_by_district.csv", row.names = FALSE)
cat("✓ Recommendations saved to: recommendations_by_district.csv\n\n")

# ==============================================================================
# LOW IMPACT INTERVENTIONS
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  7. LOW IMPACT INTERVENTIONS (< 500 cases)                   ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

low_impact <- rankings_data %>%
  filter(mean_impact < 500) %>%
  arrange(mean_impact)

if(nrow(low_impact) > 0) {
  cat("⚠️  WARNING: These combinations have low impact!\n\n")
  
  for(i in 1:nrow(low_impact)) {
    cat(sprintf("%-12s in %-30s: %8.0f cases\n",
               low_impact$intervention[i],
               low_impact$admin_2[i],
               low_impact$mean_impact[i]))
  }
} else {
  cat("✓ All interventions have impact > 500 cases\n")
}
cat("\n")

# ==============================================================================
# COMPARISON BETWEEN DISTRICTS
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  8. COMPARISON: BEST vs WORST DISTRICT BY INTERVENTION      ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

for(interv in interventions) {
  data_interv <- rankings_data %>%
    filter(intervention == interv)
  
  best <- data_interv %>% filter(rank == 1)
  worst <- data_interv %>% filter(rank == max(rank))
  
  difference <- best$mean_impact - worst$mean_impact
  ratio <- best$mean_impact / worst$mean_impact
  
  cat(sprintf("%-12s:\n", interv))
  cat(sprintf("  🟢 Best:  %-30s (%8.0f cases)\n", best$admin_2, best$mean_impact))
  cat(sprintf("  🔴 Worst: %-30s (%8.0f cases)\n", worst$admin_2, worst$mean_impact))
  cat(sprintf("  📊 Difference: %8.0f cases (%.1fx better)\n", difference, ratio))
  cat("\n")
}

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  FINAL SUMMARY                                               ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("Files generated:\n")
cat("  1. impact_matrix_district_intervention.csv\n")
cat("  2. recommendations_by_district.csv\n\n")

cat("Statistics:\n")
cat("  Districts analyzed:", length(districts), "\n")
cat("  Interventions analyzed:", length(interventions), "\n")
cat("  Total impacts calculated:", nrow(impacts), "\n\n")

cat("BEST intervention (overall mean):\n")
best_overall <- impacts %>%
  group_by(intervention) %>%
  summarise(mean_impact = mean(impact, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_impact)) %>%
  head(1)
cat("  🏆", best_overall$intervention, "→", format(round(best_overall$mean_impact), big.mark = ","), "cases\n\n")

cat("District with HIGHEST mean impact overall:\n")
best_district <- rankings_data %>%
  group_by(admin_2) %>%
  summarise(mean_impact = mean(mean_impact, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_impact)) %>%
  head(1)
cat("  🏆", best_district$admin_2, "→", format(round(best_district$mean_impact), big.mark = ","), "cases (mean)\n\n")

cat("✓ VERIFICATION COMPLETE!\n\n")
