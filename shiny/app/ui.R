app_ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    fluidPage(
      fluidRow(selectInput("year", "Year", "")),
      fluidRow(column(aqi_heatmap_ui("aqi_heatmap"), width = 12)),
      fluidRow(column(aqi_details_ui("aqi_details"), width = 12)),
      fluidRow(column(map_ui("map"), width = 12))
    )
  )
)
