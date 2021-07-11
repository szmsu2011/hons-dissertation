library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(rlang)
library(echarts4r)
library(mapboxer)
library(tsibble)
library(lubridate)
library(janitor)

invisible(map(paste0("shiny/app/module/", list.files("shiny/app/module/")), source))

source("shiny/app/function.R")
source("shiny/app/data.R")
source("shiny/app/ui.R")
source("shiny/app/server.R")

initial_app_state <- list(
  map_onclick = "Queen Street",
  year = year(max(aqi_data[["datetime"]])),
  aqi_date_selected = NULL
)

shinyApp(app_ui, app_server)
