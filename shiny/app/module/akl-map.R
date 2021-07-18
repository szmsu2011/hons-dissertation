map_ui <- function(id) {
  ns <- NS(id)

  tagList(leafletOutput(ns("map"), height = "1150px"))
}

map_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["map"]] <- renderLeaflet({
      leaflet(data = station) %>%
        addTiles() %>%
        setView(174.7945, -37.2109, zoom = 9.5) %>%
        addMarkers(~lng, ~lat, ~site, label = ~site)
    })

    observeEvent(input[["map_marker_click"]], {
      state[["map_onclick"]] <- input[["map_marker_click"]][["id"]]
      state[["aqi_heatmap_datazoom"]] <- NULL
    })
  }

  moduleServer(id, module)
}
