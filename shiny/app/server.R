app_server <- function(input, output, session) {
  app_state <- reactiveValues(
    map_onclick = "Queen Street"
  )

  map_server("map", app_state)
  callback_server("callback", app_state)
}
