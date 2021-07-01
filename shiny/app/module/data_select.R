data_select_server <- function(id, state) {
  module <- function(input, output, session) {
    current_loc_aqi_data <- reactive({
      filter(aqi_data, location == make_clean_names(state[["map_onclick"]]))
    })

    return(current_loc_aqi_data)
  }

  moduleServer(id, module)
}
