app_ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    fluidPage(
      fluidRow(
        column(aqi_heatmap_ui("aqi_heatmap"), width = 12)
      ),
      fluidRow(
        column(map_ui("map"), width = 6),
        column(callback_ui("callback"), width = 6)
      )
    )
  )
)
