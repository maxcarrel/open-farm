library(shiny)
library(leaflet)

source("datasets.R")

source("gui.R")

source("backend.R")

# Run the application 
shinyApp(ui = ui, server = server)

