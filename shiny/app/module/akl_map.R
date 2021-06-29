station <- tibble(
  site = c(
    "Queen Street", "Glen Eden", "Penrose", "Henderson", "Pakuranga",
    "Takapuna", "Patumahoe", "Customs St", "Khyber Pass", "Papatoetoe"
  ),
  code = case_when(
    site == "Queen Street" ~ "QUS",
    TRUE ~ toupper(substr(site, 1, 3))
  ),
  lng = c(
    174.7650, 174.6447, 174.8199, 174.6239, 174.8709,
    174.7651, 174.8282, 174.7683, 174.7693, 174.8645
  ),
  lat = -c(
    36.8482, 36.9225, 36.9045, 36.8679, 36.9130,
    36.7899, 37.2047, 36.8450, 36.8653, 36.9756
  )
)

map_ui <- function(id) {
  ns <- NS(id)

  tagList(mapboxerOutput(ns("map")))
}

map_server <- function(id) {
  module <- function(input, output, session) {
    output[["map"]] <- renderMapboxer({
      station %>%
        as_mapbox_source() %>%
        mapboxer(
          center = c(174.7645, -36.8509),
          zoom = 8
        ) %>%
        add_navigation_control() %>%
        add_circle_layer(
          circle_color = "white",
          circle_blur = .5
        )
    })
  }

  moduleServer(id, module)
}
