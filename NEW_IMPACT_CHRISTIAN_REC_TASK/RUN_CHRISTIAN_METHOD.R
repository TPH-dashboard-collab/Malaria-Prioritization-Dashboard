# ==============================================================================
# COMPLETE PRIORITIZATION PIPELINE - CHRISTIAN'S METHOD
# Tanzania Malaria Intervention Prioritization
# ==============================================================================

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

# ==============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 1: LOADING DATA                                      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Load data (works with both CSV and RDS)
if(file.exists("TZ_subset_10regions_1seed.csv")) {
  df <- fread("TZ_subset_10regions_1seed.csv")
  cat("✓ Loaded CSV file\n")
} else if(file.exists("arquivo_compacto.rds")) {
  df <- readRDS("arquivo_compacto.rds")
  df <- as.data.table(df)
  cat("✓ Loaded RDS file\n")
} else {
  stop("❌ Data file not found! Please provide TZ_subset_10regions_1seed.csv or arquivo_compacto.rds")
}

cat("  Rows:", nrow(df), "\n")
cat("  Columns:", ncol(df), "\n")
cat("  Districts:", length(unique(df$admin_2)), "\n\n")

# Prepare data with CORRECTED cumsum
cat("Processing data (filtering EIR_mean, calculating cumulative sum)...\n")

df %>%
  filter(EIR_CI == "EIR_mean") %>%
  # ✓ CRITICAL: Group by all identifying variables before cumsum
  group_by(admin_2, age_group, scenario_name, seed) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(nUncompCum = cumsum(nUncomp)) %>%
  ungroup() %>%
  filter(year == 2030) %>%
  rename_with(~ str_remove(., "active_int_"), starts_with("active_int_")) -> df1

df1 <- as.data.table(df1)

cat("✓ Data prepared\n")
cat("  Filtered rows:", nrow(df1), "\n\n")

# ==============================================================================
# STEP 2: HELPER FUNCTIONS (Christian's method)
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 2: LOADING HELPER FUNCTIONS                          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Function to create counterfactual scenarios
counterfactual_lgc <- function(x){
  res <- lapply(which(x), function(i) {
    y <- x
    y[i] <- FALSE
    names(y) <- names(x)
    y
  })
  return(res)
}

# Function to filter by intervention pattern
filter_lgc <- function(df,
                       target,
                       interv = c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                  "STD_Nets","PBO_Nets","IG2_Nets",
                                  "SMC","PMC")){
  subdf <- df[, .SD, .SDcols = interv]
  out <- df[rowSums(subdf == matrix(target, nrow(df), length(target), byrow = TRUE)) == length(target)]
  return(out)
}

# Main function: calculate per-intervention impact
per_interv_impact <- function(df,
                               strata = c(admin_2 = "Bunda District Council",
                                          age_group = "0-100",
                                          scenario_name = "CM_IG2_Nets_IRS_PMC",
                                          plan = "NSP"),
                               indicator = c("nUncompCum"),
                               interv = c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                          "STD_Nets","PBO_Nets","IG2_Nets",
                                          "SMC","PMC"),
                               keep_col = NULL){
  
  strat_join <- names(strata)[1:3]
  
  strata <- as.data.table(t(strata))
  setkeyv(df, strat_join)
  setkeyv(strata, strat_join)
  target <- as.logical(df[strata][, ..interv])
  names(target) <- interv
  
  counterfactual_cols <- c(strat_join, indicator, interv)
  if(!is.null(keep_col)){counterfactual_cols <- c(counterfactual_cols, keep_col)}
  
  lapply(counterfactual_lgc(target),
         function(x) {
           setkeyv(df, strat_join[1:2])
           setkeyv(strata, strat_join[1:2])
           filter_lgc(df[strata][, ..counterfactual_cols], x)
         }
  ) -> counterfactual_df
  counterfactual_df <- counterfactual_df[lapply(counterfactual_df, nrow) > 0]
  
  setkeyv(df, strat_join)
  setkeyv(strata, strat_join)
  strata_df <- df[strata][, ..counterfactual_cols]
  
  lapply(counterfactual_df,
         function(x) {
           setkeyv(x, strat_join[1:2])
           setkeyv(strata, strat_join[1:2])
           foo <- x[strata_df]
           for (k in indicator){
             foo[, paste0("IMPACT_", k) := get(k) - get(paste0("i.", k))]
           }
           foo_cols <- c(strat_join[strat_join != "scenario_name"], "i.scenario_name", paste0("IMPACT_", indicator))
           if(!is.null(keep_col)){foo_cols <- c(foo_cols, keep_col)}
           melt(
             foo[, ..foo_cols],
             id.vars = setdiff(foo_cols, indicator),
             measure.vars = paste0("IMPACT_", indicator),
             variable.name = "metric",
             value.name = "value"
           ) -> foo
           return(foo)
         }
  ) -> counterfactual_df
  
  lapply(names(counterfactual_df),
         function(x) {
           foo <- counterfactual_df[[x]]
           foo <- foo[, intervention := x]
           setnames(foo, 
                    old = c("i.scenario_name"), 
                    new = c("scenario_name"))
           return(foo)
         }
  ) -> result
  
  result <- rbindlist(result)
  result <- result[, (paste0("IMPACT_", indicator)) := NULL]
  
  return(result)
}

cat("✓ Helper functions loaded\n\n")

# ==============================================================================
# STEP 3: CALCULATE IMPACTS FOR ALL SCENARIOS
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 3: CALCULATING IMPACTS FOR ALL SCENARIOS             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Get all Customized plan scenarios (full factorial design)
cat("Identifying scenarios from Customized plan...\n")

df1 %>%
  filter(plan == "Customized") %>%
  select(admin_2, age_group, scenario_name, plan) -> strata_all

cat("✓ Found", nrow(strata_all), "scenarios to process\n")
cat("  Districts:", length(unique(strata_all$admin_2)), "\n")
cat("  Age groups:", length(unique(strata_all$age_group)), "\n\n")

cat("Calculating per-intervention impacts...\n")
cat("(This may take 15-30 minutes depending on your computer)\n\n")

start_time <- Sys.time()

# Calculate impacts for all scenarios
impacts_all <- rbindlist(
  lapply(1:nrow(strata_all), function(i) {
    if(i %% 100 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- i / elapsed
      remaining <- (nrow(strata_all) - i) / rate / 60
      cat(sprintf("Progress: %d/%d (%.1f%%) | Remaining: ~%.0f min\r", 
                  i, nrow(strata_all), 100*i/nrow(strata_all), remaining))
    }
    
    x <- strata_all[i, ]
    per_interv_impact(df1,
                      indicator = c("nUncompCum"),
                      strata = c(admin_2 = x$admin_2,
                                 age_group = x$age_group,
                                 scenario_name = x$scenario_name,
                                 plan = x$plan),
                      keep_col = NULL)
  })
)

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "mins")

cat("\n\n✓ Impact calculation complete!\n")
cat("  Time elapsed:", round(elapsed_time, 1), "minutes\n")
cat("  Impacts calculated:", nrow(impacts_all), "\n")
cat("  Interventions:", length(unique(impacts_all$intervention)), "\n")
cat("  Districts:", length(unique(impacts_all$admin_2)), "\n\n")

# Save impacts
fwrite(impacts_all, "impacts_christian_method.csv")
cat("✓ Saved to: impacts_christian_method.csv\n\n")

# ==============================================================================
# STEP 4: CALCULATE QUANTILES (Roland's specification)
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 4: CALCULATING QUANTILES                             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Calculate mean impact per district, intervention, age_group
cat("Step 1: Calculating mean impact per district...\n")

avg_impact <- impacts_all %>%
  group_by(intervention, admin_2, age_group) %>%
  summarise(
    mean_impact = mean(value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

cat("✓ Means calculated:", nrow(avg_impact), "rows\n\n")

# Calculate quantiles per age_group and intervention
cat("Step 2: Assigning quantiles...\n")

quantiles_data <- avg_impact %>%
  group_by(intervention, age_group) %>%
  mutate(
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

# Save quantiles
fwrite(quantiles_data, "quantiles_christian_method.csv")
cat("✓ Saved to: quantiles_christian_method.csv\n\n")

# ==============================================================================
# STEP 5: CALCULATE RANKINGS (Roland's specification)
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 5: CALCULATING RANKINGS                              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Calculate min/max across seeds and ages
rankings_data <- avg_impact %>%
  group_by(intervention, admin_2) %>%
  summarise(
    mean_impact = mean(mean_impact, na.rm = TRUE),
    min_impact = min(mean_impact, na.rm = TRUE),
    max_impact = max(mean_impact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(intervention) %>%
  arrange(desc(mean_impact)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

cat("✓ Rankings created\n")
cat("  Interventions:", length(unique(rankings_data$intervention)), "\n")
cat("  Districts per intervention:", nrow(rankings_data) / length(unique(rankings_data$intervention)), "\n\n")

# Save rankings
fwrite(rankings_data, "rankings_christian_method.csv")
cat("✓ Saved to: rankings_christian_method.csv\n\n")

# ==============================================================================
# STEP 6: SUMMARY STATISTICS
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  FINAL SUMMARY                                             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Files generated:\n")
cat("  1. impacts_christian_method.csv\n")
cat("  2. quantiles_christian_method.csv\n")
cat("  3. rankings_christian_method.csv\n\n")

cat("Top 5 interventions by mean impact:\n")
top5 <- impacts_all %>%
  group_by(intervention) %>%
  summarise(mean_impact = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_impact)) %>%
  head(5)

for(i in 1:nrow(top5)) {
  cat(sprintf("  %d. %s: %s cases\n", 
              i, top5$intervention[i], format(round(top5$mean_impact[i]), big.mark = ",")))
}

cat("\n✓ PIPELINE COMPLETE!\n")
cat("\nNext step: Run dashboard_prioritization_christian.R\n\n")

