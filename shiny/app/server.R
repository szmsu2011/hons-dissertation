app_server <- function(input, output, session) {
  app_state <- eval_tidy(new_quosure(expr(reactiveValues(!!!initial_app_state))))

  map_mod("map", app_state)
  callback_mod("callback", app_state)
  aqi_heatmap_mod("aqi_heatmap", app_state)
  aqi_details_mod("aqi_lineplot", app_state)
}
