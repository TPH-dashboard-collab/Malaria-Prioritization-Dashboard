# ==============================================================================
# run_christian_app.R  -  Entry point for the Christian dashboard
# Run this file to launch the dashboard
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

# Load data (available globally for ui.R and server.R)
impacts        <- fread("data-analysis/outputs/impacts_christian_method.csv")
quantiles_data <- fread("data-analysis/outputs/quantiles_christian_method.csv")
rankings_data  <- fread("data-analysis/outputs/rankings_christian_method.csv")
shapefiles     <- read_sf("data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles     <- st_transform(shapefiles, 4326)

cat("Data loaded successfully!\n")

# Launch the app
shiny::runApp("app-christian", launch.browser = TRUE)