# ==============================================================================
# app.R — Multi-Plan Dashboard (NSP + BAU + Customized)
# ==============================================================================

# Load the directory containing the files
old_wd <- getwd()
setwd(file.path(getwd(), "Multiplan_dashbord"))

source("multiplan_global.R")
source("multiplan_ui.R")
source("multiplan_server.R")

# Restore the working directory
setwd(old_wd)

shinyApp(ui = ui, server = server)