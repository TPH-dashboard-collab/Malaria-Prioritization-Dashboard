# Ponto de entrada – execute este ficheiro para lançar a app
library(shiny)
source("utils/calculate_quantiles.R")
shiny::runApp("app", launch.browser = TRUE)

