# ==============================================================================
# global.R — Multi-Plan Dashboard (NSP + BAU)
# Changes vs previous version:
#   1. All data manipulation uses data.table (no dplyr)
#   2. Roxygen documentation added to all three functions
#   3. cum_nUncomp used directly from CSV — no cumsum() recalculation
#   4. District names read from CSV — not hardcoded
#   5. per_interv_impact() moved to server.R as a reactive (point 5)
# ==============================================================================

options(scipen = 999)

library(shiny)
library(shinydashboard)
library(data.table)
library(sf)
library(leaflet)
library(ggplot2)
library(DT)
library(RColorBrewer)
library(htmltools)
library(stringr)

# ==============================================================================
# COLOUR PALETTE — Myro's blue palette function
# Rank 1-2 = darkest blue (highest priority)
# Rank 9-10 = lightest blue (lowest priority)
# ==============================================================================

#' Create a blue colour palette with n shades
#'
#' @description
#' Generates a sequential palette of blue shades derived from RColorBrewer's
#' "Blues" palette. The lightest two shades are removed to ensure all colours
#' are visible on a white background. Used for rank group colouring in the
#' priority map and ranking bar chart.
#'
#' @param n Integer > 0. Number of colours to generate.
#'
#' @returns A character vector of length \code{n} containing hex colour codes,
#'   ordered from lightest to darkest blue.
#'
#' @examples
#' create_blue_palette(5)
#' create_blue_palette(3)
create_blue_palette <- function(n) {
  stopifnot("n must be an integer > 0" = n > 0 && !is.na(n) && n == as.integer(n))
  blues     <- brewer.pal(9, "Blues")
  blues     <- blues[seq(3, length(blues))]
  blue_ramp <- colorRampPalette(blues)
  return(blue_ramp(n))
}

# ==============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ==============================================================================

cat("Loading data...\n")
df <- fread("../data/TZ_subset_10regions_1seed.csv")
cat("✓ Raw data loaded:", nrow(df), "rows\n")

# Filter to EIR_mean and year 2030
# Use cum_nUncomp directly from CSV — no cumsum() recalculation needed
# Rename active_int_ columns by stripping the prefix
df1 <- df[EIR_CI == "EIR_mean" & year == 2030]
old_names <- grep("^active_int_", names(df1), value = TRUE)
new_names <- sub("^active_int_", "", old_names)
setnames(df1, old_names, new_names)
cat("✓ Data prepared:", nrow(df1), "rows\n")

# District names read from CSV — not hardcoded (point 4)
DISTRICTS <- unique(df1$admin_2)
cat("✓ Districts found:", length(DISTRICTS), "—", paste(sort(DISTRICTS), collapse = ", "), "\n")

# Population data — mean nHost over 2026-2030 per district x age_group
# Matches the cumulative period of cum_nUncomp (2026-2030)
population_data <- df[EIR_CI == "EIR_mean" & year %between% c(2026, 2030),
                      .(nHost = mean(nHost, na.rm = TRUE)),
                      by = .(admin_2, age_group)]
cat("✓ Population data extracted:", nrow(population_data), "rows\n")

ALL_PLANS <- sort(unique(df1$plan))
cat("✓ Plans found:", paste(ALL_PLANS, collapse = ", "), "\n")

# ==============================================================================
# STEP 2: CHRISTIAN'S FUNCTIONS — with roxygen documentation added
# Original logic unmodified; only indicator default updated to cum_nUncomp
# ==============================================================================

#' Generate counterfactual logical vectors
#'
#' @description
#' For a named logical vector of active interventions, generates one modified
#' copy per active intervention where that intervention is set to FALSE.
#' Used internally by \code{per_interv_impact()} to build the set of
#' counterfactual scenarios needed for marginal impact calculation.
#'
#' @param x Named logical vector indicating which interventions are active
#'   (\code{TRUE} = present, \code{FALSE} = absent). Names must correspond
#'   to intervention column names in the data.
#'
#' @returns A named list of logical vectors. Each element is a copy of
#'   \code{x} with one active intervention set to \code{FALSE}. The list
#'   is named by intervention.
#'
#' @keywords internal
#'
#' @examples
#' x <- c(CM = TRUE, IRS = FALSE, SMC = TRUE)
#' counterfactual_lgc(x)
#' # Returns a list of length 2:
#' #   $CM:  CM = FALSE, IRS = FALSE, SMC = TRUE
#' #   $SMC: CM = TRUE,  IRS = FALSE, SMC = FALSE
#'
#' @noRd
counterfactual_lgc <- function(x) {
  res <- lapply(which(x), function(i) {
    y <- x; y[i] <- FALSE; names(y) <- names(x); y
  })
  return(res)
}

#' Filter a data.table to rows matching a logical intervention pattern
#'
#' @description
#' Subsets a data.table to rows where the intervention columns exactly match
#' the named logical vector \code{target}. Used internally by
#' \code{per_interv_impact()} to retrieve the counterfactual scenario row
#' for a specific intervention combination.
#'
#' @param df A data.table containing one logical column per intervention
#'   and additional outcome columns.
#' @param target Named logical vector specifying the exact intervention
#'   combination to match. Names must correspond to column names in \code{df}.
#' @param interv Character vector of intervention column names to use for
#'   matching. Defaults to all 11 interventions in the Tanzania dataset.
#'
#' @returns A data.table containing only the rows of \code{df} where all
#'   columns named in \code{interv} match \code{target} exactly.
#'
#' @keywords internal
#'
#' @examples
#' # Internal use only — called inside per_interv_impact()
#'
#' @noRd
filter_lgc <- function(df, target,
                       interv = c("LSM", "IRS", "IPTSc", "Vaccine", "CM", "ICCM",
                                  "STD_Nets", "PBO_Nets", "IG2_Nets", "SMC", "PMC")) {
  subdf <- df[, .SD, .SDcols = interv]
  df[rowSums(subdf == matrix(target, nrow(df), length(target), byrow = TRUE)) == length(target)]
}

#' Calculate per-intervention counterfactual impact
#'
#' @description
#' For a given district x age group x scenario combination, calculates the
#' marginal impact of each active intervention by comparing the full scenario
#' to a counterfactual where that intervention is removed.
#'
#' Impact is defined as:
#'
#' \code{IMPACT = indicator(without intervention X) - indicator(with intervention X)}
#'
#' A positive value means cases averted by that intervention.
#' This function is called reactively in \code{server.R} — it runs live
#' after the user selects a plan, not at dashboard startup.
#'
#' @param df A data.table prepared from the simulation output. Must contain
#'   columns for \code{admin_2}, \code{age_group}, \code{scenario_name},
#'   \code{cum_nUncomp}, and one logical column per intervention.
#' @param strata Named character vector with four elements: \code{admin_2},
#'   \code{age_group}, \code{scenario_name}, and \code{plan}. Identifies the
#'   specific row combination to analyse.
#' @param indicator Character vector of outcome column names to use as the
#'   impact metric. Defaults to \code{"cum_nUncomp"} (cumulative uncomplicated
#'   malaria cases, read directly from CSV).
#' @param interv Character vector of intervention column names. Defaults to
#'   all 11 interventions in the Tanzania dataset.
#' @param keep_col Optional character vector of additional columns to retain
#'   in the output. Default is \code{NULL}.
#'
#' @returns A data.table in long format with one row per active intervention,
#'   containing columns: \code{admin_2}, \code{age_group}, \code{scenario_name},
#'   \code{intervention}, \code{metric}, and \code{value} (the impact).
#'
#' @examples
#' # Called reactively in server.R for the selected plan
#' # per_interv_impact(df1, strata = c(admin_2 = "Bunda District Council",
#' #                                   age_group = "0-100",
#' #                                   scenario_name = "nsp",
#' #                                   plan = "NSP"))
per_interv_impact <- function(df,
                              strata    = c(admin_2       = "Bunda District Council",
                                            age_group     = "0-100",
                                            scenario_name = "nsp",
                                            plan          = "NSP"),
                              indicator = c("cum_nUncomp"),
                              interv    = c("LSM", "IRS", "IPTSc", "Vaccine", "CM", "ICCM",
                                            "STD_Nets", "PBO_Nets", "IG2_Nets", "SMC", "PMC"),
                              keep_col  = NULL) {
  
  strat_join <- names(strata)[1:3]
  strata     <- as.data.table(t(strata))
  setkeyv(df, strat_join); setkeyv(strata, strat_join)
  target <- as.logical(df[strata][, ..interv]); names(target) <- interv
  
  counterfactual_cols <- c(strat_join, indicator, interv)
  if (!is.null(keep_col)) counterfactual_cols <- c(counterfactual_cols, keep_col)
  
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
    for (k in indicator) foo[, paste0("IMPACT_", k) := get(k) - get(paste0("i.", k))]
    foo_cols <- c(strat_join[strat_join != "scenario_name"], "i.scenario_name",
                  paste0("IMPACT_", indicator))
    if (!is.null(keep_col)) foo_cols <- c(foo_cols, keep_col)
    melt(foo[, ..foo_cols],
         id.vars       = setdiff(foo_cols, indicator),
         measure.vars  = paste0("IMPACT_", indicator),
         variable.name = "metric", value.name = "value")
  }) -> counterfactual_df
  
  lapply(names(counterfactual_df), function(x) {
    foo <- counterfactual_df[[x]]
    foo <- foo[, intervention := x]
    setnames(foo, old = "i.scenario_name", new = "scenario_name")
    foo
  }) -> result
  
  result           <- rbindlist(result)
  cols_to_remove   <- intersect(paste0("IMPACT_", indicator), names(result))
  if (length(cols_to_remove) > 0) result <- result[, (cols_to_remove) := NULL]
  return(result)
}

cat("✓ Functions loaded\n")

# ==============================================================================
# STEP 3: STATIC DATA ONLY — impacts calculated reactively in server.R
# (point 5: per_interv_impact runs live after plan selection, not at startup)
# ==============================================================================

# strata_all used by server.R to know which rows to iterate over per plan
strata_all <- unique(
  df1[scenario_name %in% c("nsp", "bau"),
      .(admin_2, age_group, scenario_name, plan)]
)
cat("✓ strata_all prepared:", nrow(strata_all), "rows (NSP + BAU)\n")

# ==============================================================================
# STEP 4: RANK HELPER FUNCTIONS — pure data.table
# ==============================================================================

#' Assign rank group label from numeric rank
#'
#' @description
#' Converts a numeric rank into a paired rank group label.
#' Used to colour districts consistently across map and bar chart.
#'
#' @param r Integer vector of ranks where 1 = highest impact district.
#'
#' @returns Character vector of rank group labels:
#'   \code{"1-2"}, \code{"3-4"}, \code{"5-6"}, \code{"7-8"}, or \code{"9-10"}.
#'
#' @examples
#' rank_groups(c(1, 2, 3, 7, 10))
#' # Returns: "1-2" "1-2" "3-4" "7-8" "9-10"
#'
#' @noRd
rank_groups <- function(r) {
  fcase(r <= 2L, "1-2",
        r <= 4L, "3-4",
        r <= 6L, "5-6",
        r <= 8L, "7-8",
        default = "9-10")
}

#' Assign numeric rank group index from numeric rank
#'
#' @description
#' Converts a numeric rank into an integer group index (1 to 5).
#' Used internally for ordering and colour mapping.
#'
#' @param r Integer vector of ranks where 1 = highest impact district.
#'
#' @returns Integer vector from \code{1L} (rank group 1-2) to
#'   \code{5L} (rank group 9-10).
#'
#' @examples
#' rank_numeric(c(1, 3, 5, 8, 10))
#' # Returns: 1 2 3 4 5
#'
#' @noRd
rank_numeric <- function(r) {
  fcase(r <= 2L, 1L,
        r <= 4L, 2L,
        r <= 6L, 3L,
        r <= 8L, 4L,
        default = 5L)
}

# ==============================================================================
# STEP 5: LOAD SHAPEFILE
# ==============================================================================

cat("Loading shapefile...\n")
shapefiles <- read_sf("../data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles <- st_transform(shapefiles, 4326)
cat("✓ Shapefile loaded:", nrow(shapefiles), "districts\n\n")

cat("═══════════════════════════════════════════════════════\n")
cat("✓ global.R complete\n")
cat("  Plans available:", paste(ALL_PLANS, collapse = " | "), "\n")
cat("  Districts:", length(DISTRICTS), "\n")
cat("  Note: impacts calculated reactively in server.R\n")
cat("═══════════════════════════════════════════════════════\n\n")