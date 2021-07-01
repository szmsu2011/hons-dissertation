library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
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

initial_location <- "Queen Street"

shinyApp(app_ui, app_server)
