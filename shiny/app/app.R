library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(echarts4r)
library(mapboxer)

invisible(map(paste0("shiny/app/module/", list.files("shiny/app/module/")), source))
source("shiny/app/ui.R")
source("shiny/app/server.R")

shinyApp(app_ui, app_server)
