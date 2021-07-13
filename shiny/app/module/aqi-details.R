aqi_details_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(tagList(echarts4rOutput(ns("aqi_details"))))
  )
}

aqi_details_mod <- function(id, state) {
  module <- function(input, output, session) {
    ns <- session[["ns"]]

    output[["aqi_details"]] <- renderEcharts4r({
      data <- filter(con_data, location == make_clean_names(state[["map_onclick"]]))

      req(state[["year"]] %in% year(data[["datetime"]]))

      data <- filter(data, year(datetime) == state[["year"]])

      pol <- names(data)[2:7]

      data <- data %>%
        mutate(aqi_pol = toupper(data[2:7] %>%
          array_branch(1) %>%
          map_chr(function(x) {
            p <- pol[which.max(x / c(25, 50, 200, 350, 10, 150))]
            if (!length(p)) NA else p
          }))) %>%
        as_tibble() %>%
        group_by(date = date(datetime)) %>%
        summarise(aqi_pol = factor(names(table(aqi_pol))[1], toupper(pol))) %>%
        ungroup() %>%
        mutate(tt = paste0(
          "Date: ", fmt_date(date), "<br />",
          "Key AQI Constituent: ", aqi_pol, "<br />"
        )) %>%
        as_tsibble(index = date)

      level <- levels(data[["aqi_pol"]])
      col <- qualitative_hcl(length(level))

      data %>%
        mutate(aqi_pol = as.numeric(aqi_pol)) %>%
        e_charts(date) %>%
        e_calendar(range = state[["year"]]) %>%
        e_heatmap(aqi_pol, bind = tt, coord_system = "calendar") %>%
        e_visual_map(aqi_pol,
          type = "piecewise",
          orient = "horizontal",
          top = "top",
          left = "center",
          pieces = map2(
            level, seq_along(level),
            function(x, i) {
              list(value = i, label = x, color = col[i])
            }
          )
        ) %>%
        e_title(paste(
          "Key Pollutant,",
          state[["map_onclick"]]
        )) %>%
        e_tooltip(formatter = JS("
          function(params) {
            return params.name;
          }
        "))
    }) %>%
      bindCache(state[["map_onclick"]], state[["year"]])

    observeEvent(input[["aqi_details_clicked_data"]], {
      con_date_selected <- input[["aqi_details_clicked_data"]][["value"]][1]
      state[["con_date_selected"]] <- ymd(con_date_selected)

      con_selected <- input[["aqi_details_clicked_data"]][["name"]]
      con_selected <- gsub("(.*)t: (.*)<(.*)", "\\2", con_selected)

      output[["con_quantile"]] <- renderEcharts4r({
        data <- con_data %>%
          filter(location == make_clean_names(state[["map_onclick"]])) %>%
          mutate(pol = !!sym(tolower(con_selected)))

        day_data <- data %>%
          filter(date(datetime) == state[["con_date_selected"]]) %>%
          mutate(hour = hour(datetime))

        threshold <- c(25, 50, 200, 350, 10, 150)

        data %>%
          as_tibble() %>%
          group_by(hour = hour(datetime)) %>%
          summarise(
            lower = quantile(pol, prob = .025, na.rm = TRUE),
            median = quantile(pol, prob = .5, na.rm = TRUE),
            upper = quantile(pol, prob = .975, na.rm = TRUE)
          ) %>%
          select(-hour) %>%
          bind_cols(day_data) %>%
          e_charts(hour) %>%
          e_line(pol) %>%
          e_scatter(median, symbol_size = 5, itemStyle = list(color = "#808080")) %>%
          e_legend(show = FALSE) %>%
          e_error_bar(lower, upper) %>%
          e_mark_line(
            data = list(
              yAxis = threshold[tolower(con_selected) == names(con_data)[2:7]],
              lineStyle = list(color = "steelblue"),
              label = list(formatter = "threshold")
            ),
            symbol = "none"
          ) %>%
          e_axis_labels(x = "(UTC+12:00)") %>%
          e_x_axis(nameLocation = "end") %>%
          e_title(paste0(
            state[["map_onclick"]], " ", con_selected, ", ",
            fmt_date(state[["con_date_selected"]])
          ))
      }) %>%
        bindCache(state[["map_onclick"]], state[["con_date_selected"]])

      showModal(modalDialog(
        echarts4rOutput(ns("con_quantile")),
        footer = NULL,
        size = "l",
        easyClose = TRUE
      ))
    })
  }

  moduleServer(id, module)
}
