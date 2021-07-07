aqi_details_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      actionButton(ns("aqi_lineplot"), "Hourly Series"),
      actionButton(ns("test"), "Test")
    ),
    fluidRow(tagList(echarts4rOutput(ns("aqi_details"))))
  )
}

aqi_details_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_details"]] <- renderEcharts4r({
      if (state[["aqi_details_display"]] == "lineplot") {
        if (is.null(state[["aqi_heatmap_datazoom"]])) {
          start <- with(data, ifelse(
            last(date) - years(1) <= first(date),
            0,
            (1 - years(1) / as.period(diff(range(date)))) * 100
          ))
        } else {
          start <- state[["aqi_heatmap_datazoom"]][["start"]]
          end <- state[["aqi_heatmap_datazoom"]][["end"]]
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
          e_axis_labels(y = "AQI") %>%
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
      } else if (state[["aqi_details_display"]] == "quantile") {
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
          e_x_axis(nameLocation = "center")
      } else {
        if (test) print("")
      }
    }) %>%
      bindCache( #FIXME Cache keys and dataZoom clashes
        state[["map_onclick"]],
        state[["aqi_details_display"]],
        state[["aqi_date_selected"]]
      )

    observeEvent(input[["aqi_lineplot"]], {
      state[["aqi_details_display"]] <- "lineplot"
    })

    observeEvent(input[["test"]], {
      state[["aqi_details_display"]] <- "test"
    })
  }

  moduleServer(id, module)
}
