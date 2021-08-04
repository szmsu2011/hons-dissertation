library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(htmlwidgets)
library(shinyWidgets)
library(rlang)
library(echarts4r)
library(reactable)
library(leaflet)
library(tsibble)
library(lubridate)
library(colorspace)
library(janitor)

# Load Shiny Modules
invisible(map(paste0("shiny/app/module/", list.files("shiny/app/module/")), source))

source("shiny/app/function.R")
source("shiny/app/data.R")
source("shiny/app/ui.R")
source("shiny/app/server.R")

initial_app_state <- list(
  data = append_data(NULL, "queen_street"),
  map_onclick = "Queen Street"
)

shinyApp(app_ui, app_server)
