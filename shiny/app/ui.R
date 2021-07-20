app_ui <- dashboardPage(
  dashboardHeader(title = "AKL Environment"),
  dashboardSidebar(sidebarMenu(
    menuItem("Air Quality", tabName = "aqi", icon = icon("dashboard")),
    menuItem("Weather", tabName = "weather", icon = icon("cloud-sun")),
    menuItem("Wind", tabName = "wind", icon = icon("wind"))
  )),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      tabItem("aqi", fluidPage(
        fluidRow(selectInput("year", "Year", "")),
        fluidRow(
          column(map_ui("map"), width = 4),
          column(aqi_heatmap_ui("aqi_heatmap"), width = 4),
          column(aqi_details_ui("aqi_details"), width = 4)
        )
      )),
      tabItem("weather"),
      tabItem("wind", fluidPage(
        fluidRow(
          column(wind_rose_ui("wind_rose"), width = 4)
        )
      ))
    )
  )
)
