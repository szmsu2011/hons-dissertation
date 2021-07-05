aqi_details_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_lineplot")))
}

aqi_details_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_lineplot"]] <- renderEcharts4r({
      if (is.null(state[["aqi_heatmap_datazoom"]])) {
        start <- with(data, ifelse(
          last(date) - years(1) <= first(date),
          0,
          (1 - years(1) / as.period(diff(range(date)))) * 100
        ))
      }

      e <- aqi_data %>%
        filter(location == make_clean_names(state[["map_onclick"]])) %>%
        mutate(tt = 'paste0(
          "Date: ", fmt_date(datetime), "<br />",
          "Date: ", format(datetime, "%H:%M:%S"), "<br />",
          "AQI: ", aqi, "<br />",
          "AQI Level: ", aqi_cat
        )') %>%
        e_charts(datetime) %>%
        e_line(aqi, symbol = "none") %>%
        # e_tooltip(
        #   axisPointer = list(type = "cross"),
        #   formatter = htmlwidgets::JS("
        #     function(params) {
        #       return params.name
        #     }
        #   ")
        # ) %>%
        e_datazoom(x_index = 0, start = start, end = 100, show = FALSE) %>%
        e_legend(show = FALSE) %>%
        e_group("aqi_grp")

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
      bindCache(state[["map_onclick"]])
  }

  moduleServer(id, module)
}
