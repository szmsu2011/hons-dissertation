aqi_lineplot_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_lineplot")))
}

aqi_lineplot_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_lineplot"]] <- renderEcharts4r({
      data %>%
        filter(location == make_clean_names(state[["map_onclick"]])) %>%
        e_charts(datetime) %>%
        e_line(aqi, symbol = "none") %>%
        e_datazoom(x_index = 0, show = FALSE) %>%
        e_legend(show = FALSE) %>%
        e_group("aqi_grp")
    }) %>%
      bindCache(state[["map_onclick"]])
  }

  moduleServer(id, module)
}
