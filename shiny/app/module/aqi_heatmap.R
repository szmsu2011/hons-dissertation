aqi_heatmap_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_heatmap")))
}

aqi_heatmap_server <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_heatmap"]] <- renderEcharts4r({
      .data <- aqi_data %>%
        filter(location == make_clean_names(state[["map_onclick"]])) %>%
        as_tibble() %>%
        group_by(date = date(datetime), location) %>%
        summarise(agg_aqi = as.numeric(Max(aqi))) %>%
        ungroup() %>%
        mutate(
          aqi_cat = aqi_cat(agg_aqi),
          tt = paste0(
            "Date: ", fmt_date(date), "<br />",
            "Max AQI: ", agg_aqi, "<br />",
            "Max AQI Level: ", aqi_cat
          )
        ) %>%
        as_tsibble(index = date)

      ini_row <- slice_head(.data, n = 1)

      if (ini_row[["date"]] != floor_date(ini_row[["date"]], "week")) {
        ini_row <- ini_row %>%
          mutate(aqi_cat = NA, date = floor_date(ini_row[["date"]], "week"))

        .data <- bind_rows(.data, ini_row) %>%
          fill_gaps()
      }

      level <- levels(.data[["aqi_cat"]])

      .data <- .data %>%
        mutate(
          wday = wday(date, label = TRUE),
          aqi_cat = as.numeric(aqi_cat),
          yw = paste0(Year(date), " W", sprintf("%02d", Week(date)))
        )

      .data %>%
        e_charts(yw) %>%
        e_heatmap(wday, aqi_cat, bind = tt) %>%
        e_visual_map(aqi_cat,
          type = "piecewise",
          calculable = FALSE,
          pieces = map2(
            level, seq_along(level),
            function(x, i) {
              list(value = i, label = x, color = unname(aqi_pal[i]))
            }
          )
        ) %>%
        e_tooltip(
          axisPointer = list(type = "cross"),
          formatter = htmlwidgets::JS("
            function(params) {
              return(params.name)
            }
          ")
        ) %>%
        e_y_axis(inverse = TRUE) %>%
        e_datazoom(x_index = 0) %>%
        e_title(paste0(
          "Max AQI at ",
          state[["map_onclick"]],
          " [",
          paste(range(year(.data[["date"]])), collapse = "-"),
          "]"
        ))
    }) %>%
      bindCache(state[["map_onclick"]])
  }

  moduleServer(id, module)
}
