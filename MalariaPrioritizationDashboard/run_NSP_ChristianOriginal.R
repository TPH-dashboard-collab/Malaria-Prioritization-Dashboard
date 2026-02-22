# app.R — coloca este ficheiro dentro de app-nsp-christian/
source("NSP_ChristianOriginal_global.R")
source("NSP_ChristianOriginal_ui.R")
source("NSP_ChristianOriginal_server.R")

shinyApp(ui = ui, server = server)