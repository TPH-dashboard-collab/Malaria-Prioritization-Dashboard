# ==============================================================================
# global.R — Multi-Plan Dashboard (NSP + BAU + Customized)
# ==============================================================================

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

# ==============================================================================
# COLOUR PALETTE — Myro's blue palette function
# Rank 1-2 = darkest blue (highest priority)
# Rank 9-10 = lightest blue (lowest priority)
# ==============================================================================

create_blue_palette <- function(n) {
  stopifnot("n must be an integer > 0" = n > 0 && !is.na(n) && n == as.integer(n))
  blues <- brewer.pal(9, "Blues")
  blues <- blues[seq(3, length(blues))]
  blue_ramp <- colorRampPalette(blues)
  return(blue_ramp(n))
}


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

# Population data — nHost at year 2030
population_data <- df |>
  filter(EIR_CI == "EIR_mean", year == 2030) |>
  group_by(admin_2, age_group) |>
  summarise(nHost = mean(nHost, na.rm = TRUE), .groups = "drop")

cat("✓ Population data extracted:", nrow(population_data), "rows\n")

ALL_PLANS <- sort(unique(df1$plan))
cat("✓ Plans found:", paste(ALL_PLANS, collapse = ", "), "\n")

# ==============================================================================
# STEP 2: CHRISTIAN'S FUNCTIONS (original, unmodified)
# ==============================================================================

counterfactual_lgc <- function(x){
  res <- lapply(which(x), function(i) {
    y <- x; y[i] <- FALSE; names(y) <- names(x); y
  })
  return(res)
}

filter_lgc <- function(df, target,
                       interv = c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                  "STD_Nets","PBO_Nets","IG2_Nets","SMC","PMC")){
  subdf <- df[, .SD, .SDcols = interv]
  df[rowSums(subdf == matrix(target, nrow(df), length(target), byrow = TRUE)) == length(target)]
}

per_interv_impact <- function(df,
                              strata = c(admin_2="Bunda District Council",
                                         age_group="0-100",
                                         scenario_name="CM_IG2_Nets_IRS_PMC",
                                         plan="NSP"),
                              indicator = c("nUncompCum"),
                              interv = c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                         "STD_Nets","PBO_Nets","IG2_Nets","SMC","PMC"),
                              keep_col = NULL){
  strat_join <- names(strata)[1:3]
  strata <- as.data.table(t(strata))
  setkeyv(df, strat_join); setkeyv(strata, strat_join)
  target <- as.logical(df[strata][, ..interv]); names(target) <- interv
  
  counterfactual_cols <- c(strat_join, indicator, interv)
  if(!is.null(keep_col)) counterfactual_cols <- c(counterfactual_cols, keep_col)
  
  lapply(counterfactual_lgc(target), function(x) {
    setkeyv(df, strat_join[1:2]); setkeyv(strata, strat_join[1:2])
    filter_lgc(df[strata][, ..counterfactual_cols], x)
  }) -> counterfactual_df
  counterfactual_df <- counterfactual_df[lapply(counterfactual_df, nrow) > 0]
  
  setkeyv(df, strat_join); setkeyv(strata, strat_join)
  strata_df <- df[strata][, ..counterfactual_cols]
  
  lapply(counterfactual_df, function(x) {
    setkeyv(x, strat_join[1:2]); setkeyv(strata, strat_join[1:2])
    foo <- x[strata_df]
    for(k in indicator) foo[, paste0("IMPACT_",k) := get(k) - get(paste0("i.",k))]
    foo_cols <- c(strat_join[strat_join != "scenario_name"], "i.scenario_name", paste0("IMPACT_",indicator))
    if(!is.null(keep_col)) foo_cols <- c(foo_cols, keep_col)
    melt(foo[, ..foo_cols],
         id.vars      = setdiff(foo_cols, indicator),
         measure.vars = paste0("IMPACT_", indicator),
         variable.name = "metric", value.name = "value")
  }) -> counterfactual_df
  
  lapply(names(counterfactual_df), function(x) {
    foo <- counterfactual_df[[x]]
    foo <- foo[, intervention := x]
    setnames(foo, old = "i.scenario_name", new = "scenario_name")
    foo
  }) -> result
  
  result <- rbindlist(result)
  cols_to_remove <- intersect(paste0("IMPACT_", indicator), names(result))
  if(length(cols_to_remove) > 0) result <- result[, (cols_to_remove) := NULL]
  return(result)
}

cat("✓ Functions loaded\n")

# ==============================================================================
# STEP 3: CALCULATE IMPACTS FOR ALL PLANS
# ==============================================================================

cat("\nCalculating impacts for ALL plans...\n")

df1 |> select(admin_2, age_group, scenario_name, plan) |> distinct() -> strata_all
cat("✓ Found", nrow(strata_all), "scenarios across all plans\n")

impacts_all <- rbindlist(
  lapply(1:nrow(strata_all), function(i) {
    if(i %% 10 == 0) cat("Progress:", i, "/", nrow(strata_all), "\r")
    x <- strata_all[i,]
    tryCatch(
      per_interv_impact(df1, indicator = "nUncompCum",
                        strata = c(admin_2=x$admin_2, age_group=x$age_group,
                                   scenario_name=x$scenario_name, plan=x$plan)),
      error = function(e) NULL
    )
  })
)

impacts_all <- impacts_all |>
  left_join(strata_all |> select(admin_2, age_group, scenario_name, plan) |> distinct(),
            by = c("admin_2","age_group","scenario_name"))

cat("\n✓ Impacts calculated:", nrow(impacts_all), "rows\n")

# ==============================================================================
# STEP 4: avg_impact + population weighting
# ==============================================================================

cat("\nCalculating avg_impact...\n")

avg_impact_all <- impacts_all |>
  group_by(plan, intervention, admin_2, age_group) |>
  summarise(mean_impact = mean(value, na.rm = TRUE), .groups = "drop") |>
  left_join(population_data, by = c("admin_2","age_group")) |>
  mutate(impact_per_1000 = (mean_impact / nHost) * 1000)

cat("✓ avg_impact_all:", nrow(avg_impact_all), "rows\n")

# ==============================================================================
# STEP 5: RANKS (1 = best district, consistent across interventions)
# Groups: 1-2, 3-4, 5-6, 7-8, 9-10
# Colour: 1=dark green (best) → 5=red (worst)
# ==============================================================================

cat("Calculating ranks...\n")

rank_groups <- function(r) {
  case_when(r <= 2 ~ "1-2", r <= 4 ~ "3-4", r <= 6 ~ "5-6", r <= 8 ~ "7-8", TRUE ~ "9-10")
}
rank_numeric <- function(r) {
  case_when(r <= 2 ~ 1L, r <= 4 ~ 2L, r <= 6 ~ 3L, r <= 8 ~ 4L, TRUE ~ 5L)
}

ranks_all <- avg_impact_all |>
  group_by(plan, intervention, age_group) |>
  mutate(
    rank_cases       = rank(-mean_impact,    ties.method = "min"),
    rank_per_1000    = rank(-impact_per_1000, ties.method = "min"),
    n_districts      = n()
  ) |>
  mutate(
    rank_group_cases    = rank_groups(rank_cases),
    rank_group_per_1000 = rank_groups(rank_per_1000),
    rank_num_cases      = rank_numeric(rank_cases),
    rank_num_per_1000   = rank_numeric(rank_per_1000)
  ) |>
  ungroup()

cat("✓ Ranks calculated:", nrow(ranks_all), "rows\n")

# ==============================================================================
# STEP 6: LOAD SHAPEFILE
# ==============================================================================

cat("Loading shapefile...\n")
shapefiles <- read_sf("../data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles <- st_transform(shapefiles, 4326)
cat("✓ Shapefile loaded:", nrow(shapefiles), "districts\n\n")

cat("═══════════════════════════════════════════════════════\n")
cat("✓ global.R complete\n")
cat("  Plans:", paste(ALL_PLANS, collapse = " | "), "\n")
cat("═══════════════════════════════════════════════════════\n\n")