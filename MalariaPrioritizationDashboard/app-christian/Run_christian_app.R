# ==============================================================================
# run_christian_app.R  -  Ponto de entrada do dashboard Christian
# Execute este ficheiro para lançar o dashboard
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

# Carregar dados (disponíveis globalmente para ui.R e server.R)
impacts        <- fread("data-analysis/outputs/impacts_christian_method.csv")
quantiles_data <- fread("data-analysis/outputs/quantiles_christian_method.csv")
rankings_data  <- fread("data-analysis/outputs/rankings_christian_method.csv")
shapefiles     <- read_sf("data/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")
shapefiles     <- st_transform(shapefiles, 4326)

cat("Dados carregados com sucesso!\n")

# Lançar a app
shiny::runApp("app-christian", launch.browser = TRUE)