app_server <- function(input, output, session) {
  app_state <- eval_tidy(new_quosure(expr(reactiveValues(!!!initial_app_state))))

  aqi_heatmap_mod("aqi_heatmap", app_state)
  aqi_details_mod("aqi_details", app_state)
  map_mod("map", app_state)
  wind_rose_mod("wind_rose", app_state)
  # callback_mod("test", app_state)

  observeEvent(input[["year"]], {
    app_state[["year"]] <- input[["year"]]
  })

  observeEvent(app_state[["map_onclick"]], {
    loc <- make_clean_names(app_state[["map_onclick"]])
    yr <- unique(year(filter(aqi_data, location == loc)[["datetime"]]))
    if (app_state[["year"]] %in% yr) {
      updateSelectInput(session, "year", "Year", sort(yr), app_state[["year"]])
    } else {
      updateSelectInput(session, "year", "Year", sort(yr), max(yr))
    }
  })
}
