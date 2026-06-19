library(shiny)
library(tidyverse)
library(httr2)
library(ggrepel)
library(officer)
library(flextable)
library(config)
library(ggiraph)
library(bslib)
library(shinycssloaders) # needed for the spinners
library(RColorBrewer) # needed for the regional analysis
# library(leaflet)
# library(sf)

source("functions.R")
source("text.R")
source("comments.R")
source("ui/panel_ic_monitoring.R")
source("ui/panel_regional.R")
source("ui/panel_about.R")
# source("ui/panel_mpi.R")


# UI
ui <- page_navbar(
      title = "International Cooperation (IC) Context Monitoring",
      theme = bs_theme(bootswatch = "flatly", 
                       heading_font = font_collection("Arial"),
                       base_font = font_collection("Arial"), 
                       primary = "#519796") |> 
        bs_add_rules(paste(readLines("www/styles.css"), collapse = "\n")),
      nav_spacer(),
      panel_ic_monitoring,
      panel_regional,
      # panel_mpi,
      panel_about

)

# Server
server <- function(input, output, session) {
  
  source("server/fetching.R",      local = TRUE)
  source("server/fetching_regional.R",      local = TRUE)
  source("server/ic_monitoring_plots.R",      local = TRUE)
  source("server/regional_plots.R",      local = TRUE)
  source("server/ic_monitoring_word.R",      local = TRUE)
  # source("server/server_mpi.R",      local = TRUE)
  
}

# Run the app
shinyApp(ui = ui, server = server)
