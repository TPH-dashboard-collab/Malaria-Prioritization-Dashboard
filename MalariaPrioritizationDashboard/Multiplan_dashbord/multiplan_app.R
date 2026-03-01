# ==============================================================================
# app.R — Multi-Plan Dashboard (NSP + BAU + Customized)
# Coloca este ficheiro dentro da pasta app-multiplan/
# ==============================================================================

source("multiplan_global.R")
source("multiplan_ui.R")
source("multiplan_server.R")

shinyApp(ui = ui, server = server)
