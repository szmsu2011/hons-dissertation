map_ui <- function(id) {
  ns <- NS(id)

  tagList(mapboxerOutput(ns("map"), height = "1150px"))
}

map_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["map"]] <- renderMapboxer({
      station %>%
        as_mapbox_source() %>%
        mapboxer(
          center = c(174.7945, -37.2109),
          zoom = 9.2
        ) %>%
        add_navigation_control() %>%
        add_circle_layer(
          circle_color = "white",
          circle_blur = .5
        )
    })

    observeEvent(input[["map_onclick"]], {
      state[["map_onclick"]] <- input[["map_onclick"]][["props"]][["site"]]
      state[["aqi_heatmap_datazoom"]] <- NULL
    })
  }

  moduleServer(id, module)
}
