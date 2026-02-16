# ==============================================================================
# CALCULATE INTERVENTION IMPACTS - CORRECT METHOD
# Following Roland's document exactly
# ==============================================================================
# 
# PURPOSE:
# Calculate IMPACT_{intervention} for each scenario
# 
# METHOD:
# For each scenario with interventions, find scenario WITHOUT each intervention
# and calculate difference in indicator (nUncomp)
#
# GRANULARITY:
# Per intervention, seed, district (admin_2), EIR, age_group
# ==============================================================================

library(data.table)
library(dplyr)

# ==============================================================================
# MAIN FUNCTION: CALCULATE INTERVENTION IMPACTS
# ==============================================================================

calculate_intervention_impacts_correct <- function(data, 
                                                   indicator = "nUncomp",
                                                   plan_filter = "Customized") {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  CALCULATING INTERVENTION IMPACTS                         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Convert to data.table
  if(!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # Define intervention columns
  interv_cols <- c("active_int_LSM", "active_int_IRS", "active_int_IPTSc", 
                   "active_int_Vaccine", "active_int_CM", "active_int_ICCM",
                   "active_int_STD_Nets", "active_int_PBO_Nets", 
                   "active_int_IG2_Nets", "active_int_SMC", "active_int_PMC")
  
  # Convert to logical if necessary
  for(col in interv_cols) {
    if(col %in% names(data)) {
      data[[col]] <- as.logical(data[[col]])
    }
  }
  
  # Filter by plan
  data_filtered <- data[plan == plan_filter]
  
  cat("Filtered data:\n")
  cat("  Plan:", plan_filter, "\n")
  cat("  Rows:", nrow(data_filtered), "\n")
  cat("  Districts:", length(unique(data_filtered$admin_2)), "\n")
  cat("  Unique scenarios:", length(unique(data_filtered$scenario_name)), "\n\n")
  
  # Identify unique scenarios
  scenarios <- data_filtered %>%
    distinct(admin_2, age_group, EIR_CI, seed, scenario_name)
  
  cat("Total scenarios to process:", nrow(scenarios), "\n\n")
  
  # Calculate impact for each scenario
  results_list <- list()
  n_success <- 0
  n_fail <- 0
  
  for(i in 1:nrow(scenarios)) {
    
    if(i %% 100 == 0) {
      cat(sprintf("Processing %d/%d | Success: %d | Failures: %d\r", 
                  i, nrow(scenarios), n_success, n_fail))
    }
    
    # Extract scenario information
    district <- scenarios$admin_2[i]
    age <- scenarios$age_group[i]
    eir <- scenarios$EIR_CI[i]
    sd <- scenarios$seed[i]
    scenario <- scenarios$scenario_name[i]
    
    # Get data for this specific scenario
    scenario_data <- data_filtered[
      admin_2 == district & 
      age_group == age & 
      EIR_CI == eir & 
      seed == sd & 
      scenario_name == scenario
    ]
    
    if(nrow(scenario_data) == 0) next
    
    # Identify active interventions
    active_interventions <- character(0)
    for(col in interv_cols) {
      if(scenario_data[[col]][1] == TRUE) {
        active_interventions <- c(active_interventions, col)
      }
    }
    
    # Need at least 1 active intervention
    if(length(active_interventions) == 0) {
      n_fail <- n_fail + 1
      next
    }
    
    # Calculate impact of each active intervention
    for(intervention in active_interventions) {
      
      # Create counterfactual scenario pattern (without this intervention)
      # All other interventions remain the same
      counterfactual_pattern <- scenario_data[1, ..interv_cols]
      counterfactual_pattern[[intervention]] <- FALSE
      
      # Search for counterfactual scenario
      counterfactual <- data_filtered[
        admin_2 == district & 
        age_group == age & 
        EIR_CI == eir & 
        seed == sd
      ]
      
      # Filter to have exactly the intervention pattern
      for(col in interv_cols) {
        counterfactual <- counterfactual[
          get(col) == counterfactual_pattern[[col]]
        ]
      }
      
      # If counterfactual found, calculate impact
      if(nrow(counterfactual) > 0) {
        
        # Take first match
        cf <- counterfactual[1]
        
        # Calculate impact
        # IMPACT = cases WITHOUT intervention - cases WITH intervention
        # Positive value = intervention reduced cases (good!)
        impact_value <- cf[[indicator]] - scenario_data[[indicator]][1]
        
        # Store result
        result_row <- data.frame(
          admin_2 = district,
          age_group = age,
          EIR_CI = eir,
          seed = sd,
          scenario_name = scenario,
          intervention = gsub("active_int_", "", intervention),
          indicator = indicator,
          value_with = scenario_data[[indicator]][1],
          value_without = cf[[indicator]],
          impact = impact_value,
          stringsAsFactors = FALSE
        )
        
        results_list[[length(results_list) + 1]] <- result_row
        n_success <- n_success + 1
      }
    }
  }
  
  cat("\n\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  CALCULATION COMPLETE                                      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  cat("Scenarios processed:", nrow(scenarios), "\n")
  cat("Impacts calculated:", n_success, "\n")
  cat("Failures:", n_fail, "\n\n")
  
  # Combine results
  if(length(results_list) == 0) {
    stop("No impacts calculated! Check your data.")
  }
  
  impacts <- rbindlist(results_list)
  
  return(impacts)
}

# ==============================================================================
# FUNCTION: SAVE RESULTS
# ==============================================================================

save_impacts <- function(impacts, filename = "impacts_calculated.csv") {
  fwrite(impacts, filename)
  cat("✓ Impacts saved to:", filename, "\n")
  cat("  Rows:", nrow(impacts), "\n")
  cat("  Interventions:", length(unique(impacts$intervention)), "\n")
  cat("  Districts:", length(unique(impacts$admin_2)), "\n\n")
}

# ==============================================================================
# EXECUTION SCRIPT
# ==============================================================================

if(FALSE) {  # Change to TRUE to run
  
  # Load data
  data_new <- fread("TZ_subset_10regions_1seed.csv")
  
  # Calculate impacts
  impacts <- calculate_intervention_impacts_correct(
    data = data_new,
    indicator = "nUncomp",
    plan_filter = "Customized"
  )
  
  # Save
  save_impacts(impacts, "impacts_calculated.csv")
  
  # View summary
  print(head(impacts, 20))
}
