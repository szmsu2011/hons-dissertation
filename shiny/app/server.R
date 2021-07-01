app_server <- function(input, output, session) {
  app_state <- reactiveValues(map_onclick = initial_location)

  map_mod("map", app_state)
  callback_mod("callback", app_state)
  aqi_heatmap_mod("aqi_heatmap", app_state)
}
