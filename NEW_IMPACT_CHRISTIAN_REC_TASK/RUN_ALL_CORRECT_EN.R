# ==============================================================================
# MAIN EXECUTION SCRIPT - RUN EVERYTHING
# ==============================================================================

library(data.table)
library(dplyr)

# Load functions
source("calculate_impacts_CORRECT_EN.R")
source("process_for_dashboard_CORRECT_EN.R")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLETE PRIORITIZATION ANALYSIS                            ║\n")
cat("║  Following the document from Chris                                ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# STEP 1: LOAD DATA
# ==============================================================================

cat("STEP 1/3: Loading data...\n\n")

data_new <- fread("TZ_subset_10regions_1seed.csv")

cat("✓ Data loaded\n")
cat("  Rows:", nrow(data_new), "\n")
cat("  Columns:", ncol(data_new), "\n")
cat("  Districts:", length(unique(data_new$admin_2)), "\n")
cat("  Plans:", paste(unique(data_new$plan), collapse = ", "), "\n\n")

# ==============================================================================
# STEP 2: CALCULATE IMPACTS
# ==============================================================================

cat("STEP 2/3: Calculating intervention impacts...\n")
cat("(This may take 10-20 minutes)\n\n")

impacts <- calculate_intervention_impacts_correct(
  data = data_new,
  indicator = "nUncomp",
  plan_filter = "Customized"
)

# Save
save_impacts(impacts, "impacts_calculated.csv")

# ==============================================================================
# STEP 3: PROCESS FOR DASHBOARD
# ==============================================================================

cat("STEP 3/3: Processing quantiles and rankings...\n\n")

dashboard_data <- process_for_dashboard("impacts_calculated.csv")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  ANALYSIS COMPLETE!                                          ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("Files generated:\n")
cat("  1. impacts_calculated.csv          - All impacts\n")
cat("  2. quantiles_for_dashboard.csv     - Quantiles per district\n")
cat("  3. rankings_for_dashboard.csv      - Top 10 rankings\n\n")

cat("Statistics:\n")
cat("  Impacts calculated:", nrow(impacts), "\n")
cat("  Interventions:", length(unique(impacts$intervention)), "\n")
cat("  Districts:", length(unique(impacts$admin_2)), "\n\n")

cat("Next step:\n")
cat("  Run: shiny::runApp('dashboard_PRIORITIZATION_FINAL_EN.R')\n\n")

cat("Top 3 Interventions by Mean Impact:\n")
top3 <- impacts %>%
  group_by(intervention) %>%
  summarise(mean_impact = mean(impact, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_impact)) %>%
  head(3)

for(i in 1:nrow(top3)) {
  cat(sprintf("  %d. %s: %.0f cases averted\n", 
             i, top3$intervention[i], top3$mean_impact[i]))
}

cat("\n✓ READY FOR DASHBOARD!\n\n")
