aqi_details_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(tagList(echarts4rOutput(ns("aqi_lineplot"))))
  )
}

aqi_details_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_lineplot"]] <- renderEcharts4r({
      e <- aqi_data %>%
        filter(
          location == make_clean_names(state[["map_onclick"]]),
          year(datetime) == state[["year"]]
        ) %>%
        e_charts(datetime) %>%
        e_axis_labels(y = "AQI") %>%
        e_line(aqi, symbol = "none") %>%
        e_legend(show = FALSE)

      for (x in c(50, 100, 150, 200, 300)) {
        e <- e %>%
          e_mark_line(
            data = list(
              yAxis = x,
              lineStyle = list(color = aqi_pal[[aqi_cat(x + 1)]])
            ),
            symbol = "none",
            title = ""
          )
      }

      e
    }) %>%
      bindCache(state[["map_onclick"]], state[["year"]])
  }

  moduleServer(id, module)
}
