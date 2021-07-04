aqi_heatmap_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_heatmap")))
}

aqi_heatmap_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_heatmap"]] <- renderEcharts4r({
      data <- aqi_data %>%
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

      ini_row <- slice_head(data, n = 1)

      if (ini_row[["date"]] != floor_date(ini_row[["date"]], "week")) {
        ini_row <- ini_row %>%
          mutate(aqi_cat = NA, date = floor_date(ini_row[["date"]], "week"))
        ini_c <- year(ini_row[[1]]) != year(first(data[[1]]))
        data <- bind_rows(data, ini_row) %>%
          fill_gaps()
      }

      level <- levels(data[["aqi_cat"]])

      data <- data %>%
        mutate(
          wday = wday(date, label = TRUE),
          aqi_cat = as.numeric(aqi_cat),
          week_start = floor_date(date, "week"),
          x = paste(
            year(week_start),
            month(week_start, label = TRUE),
            day(week_start)
          )
        )

      if (is.null(state[["aqi_heatmap_datazoom"]])) {
        start <- with(data, ifelse(
          last(date) - years(1) <= first(date),
          0,
          (1 - years(1) / as.period(diff(range(date)))) * 100
        ))
      }

      data %>%
        e_charts(x) %>%
        e_heatmap(wday, aqi_cat, bind = tt) %>%
        e_visual_map(aqi_cat,
          type = "piecewise",
          orient = "horizontal",
          top = "top", left = "center",
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
              return params.name;
            }
          ")
        ) %>%
        e_y_axis(inverse = TRUE) %>%
        e_x_axis(formatter = htmlwidgets::JS("
          function(value) {
            return value.substring(0, 8);
          }
        ")) %>%
        e_datazoom(x_index = 0, start = start, end = 100) %>%
        e_title(paste(
          "Daily Max AQI,",
          state[["map_onclick"]]
        )) %>%
        e_capture("datazoom") %>%
        e_group("aqi_grp") %>%
        e_connect_group("aqi_grp")
    }) %>%
      bindCache(state[["map_onclick"]])

    observeEvent(input[["aqi_heatmap_datazoom"]], {
      state[["aqi_heatmap_datazoom"]] <- input[["aqi_heatmap_datazoom"]]
    })
  }

  moduleServer(id, module)
}
