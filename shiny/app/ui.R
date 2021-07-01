app_ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    fluidRow(
      column(map_ui("map"), width = 6)
    ),
    fluidRow(
      column(callback_ui("callback"), width = 12)
    )
  )
)
