# DASHBOARD: NSP Prioritization — Christian's Original Functions (per_interv_impact) without any change of the original code.
# ——————————————————————————————————————————————————————

options(scipen = 999)

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
library(stringr)
library(tidyr)

# ==============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ==============================================================================

cat("Loading data...\n")

df <- fread("../data/TZ_subset_10regions_1seed.csv")

cat("✓ Raw data loaded:", nrow(df), "rows\n")

df |>
  filter(EIR_CI == "EIR_mean") |>
  group_by(admin_2, age_group, scenario_name, seed) |>
  arrange(year, .by_group = TRUE) |>
  mutate(nUncompCum = cumsum(nUncomp)) |>
  ungroup() |>
  filter(year == 2030) |>
  rename_with(~ str_remove(., "active_int_"), starts_with("active_int_")) -> df1

df1 <- as.data.table(df1)

cat("✓ Data prepared:", nrow(df1), "rows\n")

# ==============================================================================
# STEP 2: CHRISTIAN'S FUNCTIONS
# ==============================================================================

counterfactual_lgc <- function(x){
  res <- lapply(which(x), function(i) {
    y <- x
    y[i] <- FALSE
    names(y) <- names(x)
    y
  })
  return(res)
}

filter_lgc <- function(df,
                       target,
                       interv = c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                  "STD_Nets","PBO_Nets","IG2_Nets",
                                  "SMC","PMC")){
  subdf <- df[, .SD, .SDcols = interv]
  out <- df[rowSums(subdf == matrix(target, nrow(df), length(target), byrow = TRUE)) == length(target)]
  return(out)
}

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

cat("✓ Functions loaded\n")

# ==============================================================================
# STEP 3: CALCULATE IMPACTS (NSP PLAN)
# ==============================================================================

cat("\nCalculating impacts for NSP plan...\n")

df1 |>
  filter(plan == "NSP") |>
  select(admin_2, age_group, scenario_name, plan) -> strata_nsp

cat("✓ Found", nrow(strata_nsp), "NSP scenarios\n")

impacts_nsp <- rbindlist(
  lapply(1:nrow(strata_nsp), function(i) {
    if(i %% 5 == 0) cat("Progress:", i, "/", nrow(strata_nsp), "\r")
    x <- strata_nsp[i, ]
    per_interv_impact(df1,
                      indicator = c("nUncompCum"),
                      strata = c(admin_2 = x$admin_2,
                                 age_group = x$age_group,
                                 scenario_name = x$scenario_name,
                                 plan = x$plan),
                      keep_col = NULL)
  })
)

cat("\n✓ Impacts calculated:", nrow(impacts_nsp), "rows\n")

# ==============================================================================
# STEP 4: CALCULATE QUANTILES
# ==============================================================================

cat("\nCalculating quantiles...\n")

avg_impact <- impacts_nsp |>
  group_by(intervention, admin_2, age_group) |>
  summarise(
    mean_impact = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

quantiles_data <- avg_impact |>
  group_by(intervention, age_group) |>
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

cat("✓ Quantiles calculated:", nrow(quantiles_data), "rows\n")

# ==============================================================================
# STEP 5: CALCULATE RANKINGS
# ==============================================================================

cat("Calculating rankings...\n")

rankings_data <- avg_impact |>
  group_by(intervention, admin_2) |>
  summarise(
    mean_impact = mean(mean_impact, na.rm = TRUE),
    min_impact  = min(mean_impact,  na.rm = TRUE),
    max_impact  = max(mean_impact,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(intervention) |>
  arrange(desc(mean_impact)) |>
  mutate(rank = row_number()) |>
  ungroup()

cat("✓ Rankings calculated:", nrow(rankings_data), "rows\n")

# ==============================================================================
# STEP 6: LOAD SHAPEFILE
# ==============================================================================

cat("Loading shapefile...\n")
shapefiles <- read_sf("../data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles <- st_transform(shapefiles, 4326)
cat("✓ Shapefile loaded:", nrow(shapefiles), "districts\n\n")

cat("═══════════════════════════════════════════════════════\n")
cat("✓ global.R complete — all objects ready\n")
cat("═══════════════════════════════════════════════════════\n\n")