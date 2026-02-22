# ==============================================================================
# app.R — Lança o dashboard NSP (Christian's Original Functions)
# Coloca este ficheiro dentro da pasta app-nsp-christian/
# ==============================================================================

source("NSP_ChristianOriginal_global.R")
source("NSP_ChristianOriginal_ui.R")
source("NSP_ChristianOriginal_server.R")

shinyApp(ui = ui, server = server)
