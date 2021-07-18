app_ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    fluidPage(
      fluidRow(selectInput("year", "Year", "")),
      fluidRow(
        column(map_ui("map"), width = 4),
        column(aqi_heatmap_ui("aqi_heatmap"), width = 4),
        column(aqi_details_ui("aqi_details"), width = 4)
      )
    )
  )
)
