map_ui <- function(id) {
  ns <- NS(id)

  tagList(mapboxerOutput(ns("map")))
}

map_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["map"]] <- renderMapboxer({
      mapbox_api_token <- "pk.eyJ1Ijoic3ptc3UyMDExIiwiYSI6ImNrcXV0" %>%
        paste0("aTNkOTA3dnIydnQ5eTYwZGd4NmEifQ.K98w3pims54xe7RgTWKZhg")

      station %>%
        as_mapbox_source() %>%
        mapboxer(
          center = c(174.7645, -36.8509),
          zoom = 8,
          style = "mapbox://styles/mapbox/light-v10",
          token = mapbox_api_token
        ) %>%
        add_navigation_control() %>%
        add_circle_layer(
          circle_color = "steelblue",
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
