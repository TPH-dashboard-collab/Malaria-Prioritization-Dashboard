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

# Load data
#data <- read_csv("data/tza_sample_data.csv", show_col_types = FALSE)

# Load shapefiles
#shapefiles <- read_sf("")
#shapefiles <- st_transform(shapefiles, 4326)


#cat("Data loaded successfully!\n")

# Launch the app
shiny::runApp("app", launch.browser = TRUE)

shinyApp(ui = ui, server = server)
