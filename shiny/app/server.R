app_server <- function(input, output, session) {
  app_state <- reactiveValues(map_onclick = initial_location)

  map_server("map", app_state)
  callback_server("callback", app_state)
  aqi_heatmap_server("aqi_heatmap", app_state)
}
