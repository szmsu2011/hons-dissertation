aqi_heatmap_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_heatmap")))
}

aqi_heatmap_mod <- function(id, state) {
  module <- function(input, output, session) {
    ns <- session[["ns"]]

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
          top = "top",
          left = "center",
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

    observeEvent(input[["aqi_heatmap_clicked_data"]], {
      aqi_date_selected <- input[["aqi_heatmap_clicked_data"]][["value"]]
      state[["aqi_date_selected"]] <- ymd(aqi_date_selected[1]) +
        which(wday(1:7, TRUE) == aqi_date_selected[2]) - 1

      output[["aqi_quantile"]] <- renderEcharts4r({
        day_data <- aqi_data %>%
          filter(
            location == make_clean_names(state[["map_onclick"]]),
            date(datetime) == state[["aqi_date_selected"]]
          ) %>%
          mutate(hour = hour(datetime))

        aqi_data %>%
          filter(location == make_clean_names(state[["map_onclick"]])) %>%
          as_tibble() %>%
          group_by(hour = hour(datetime)) %>%
          summarise(
            lower = quantile(aqi, prob = .025, na.rm = TRUE),
            median = quantile(aqi, prob = .5, na.rm = TRUE),
            upper = quantile(aqi, prob = .975, na.rm = TRUE)
          ) %>%
          select(-hour) %>%
          bind_cols(day_data) %>%
          e_charts(hour) %>%
          e_line(aqi) %>%
          e_scatter(median, symbol_size = 5, itemStyle = list(color = "#808080")) %>%
          e_legend(show = FALSE) %>%
          e_error_bar(lower, upper) %>%
          e_axis_labels(x = "Hour of Day") %>%
          e_x_axis(nameLocation = "center") %>%
          e_title(paste(
            state[["map_onclick"]], "AQI,",
            fmt_date(state[["aqi_date_selected"]])
          ))
      }) %>%
        bindCache(state[["map_onclick"]], state[["aqi_date_selected"]])

      showModal(modalDialog(
        echarts4rOutput(ns("aqi_quantile")),
        footer = NULL,
        size = "l",
        easyClose = TRUE
      ))
    })
  }

  moduleServer(id, module)
}
