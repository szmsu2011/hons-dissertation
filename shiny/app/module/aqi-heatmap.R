aqi_heatmap_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("aqi_heatmap"), height = 220))
}

aqi_heatmap_mod <- function(id, state) {
  module <- function(input, output, session) {
    ns <- session[["ns"]]

    output[["aqi_heatmap"]] <- renderEcharts4r({
      data <- filter(aqi_data, location == make_clean_names(state[["map_onclick"]]))

      req(state[["year"]] %in% year(data[["datetime"]]))

      data <- data %>%
        filter(year(datetime) == state[["year"]]) %>%
        as_tibble() %>%
        group_by(date = date(datetime)) %>%
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

      level <- levels(data[["aqi_cat"]])

      data %>%
        mutate(aqi_cat = as.numeric(aqi_cat)) %>%
        e_charts(date) %>%
        e_calendar(range = state[["year"]]) %>%
        e_heatmap(aqi_cat, bind = tt, coord_system = "calendar") %>%
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
        e_tooltip(formatter = htmlwidgets::JS("
          function(params) {
            return params.name;
          }
        ")) %>%
        e_title(paste(
          "Daily Max AQI,",
          state[["map_onclick"]]
        ))
    }) %>%
      bindCache(state[["map_onclick"]], state[["year"]])

    observeEvent(input[["aqi_heatmap_clicked_data"]], {
      aqi_date_selected <- input[["aqi_heatmap_clicked_data"]][["value"]][1]
      state[["aqi_date_selected"]] <- ymd(aqi_date_selected)

      output[["aqi_quantile"]] <- renderEcharts4r({
        data <- aqi_data %>%
          filter(location == make_clean_names(state[["map_onclick"]]))

        day_data <- data %>%
          filter(date(datetime) == state[["aqi_date_selected"]]) %>%
          mutate(hour = hour(datetime))

        e <- data %>%
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
          e_axis_labels(x = "(UTC+12:00)") %>%
          e_x_axis(nameLocation = "end") %>%
          e_title(paste(
            state[["map_onclick"]], "AQI,",
            fmt_date(state[["aqi_date_selected"]])
          ))

        for (x in c(50, 100, 150, 200, 300)) {
          ac <- as.character(aqi_cat(x + 1))
          pat <- "ealthy|sitive"

          e <- e %>%
            e_mark_line(
              data = list(
                yAxis = x,
                lineStyle = list(color = aqi_pal[[ac]]),
                label = list(formatter = case_when(
                  ac == "Unhealthy" ~ "Unhealthy",
                  grepl(pat, ac) ~ gsub(pat, "\\.", ac),
                  TRUE ~ ac
                ))
              ),
              symbol = "none"
            )
        }

        e
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
