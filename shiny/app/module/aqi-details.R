aqi_details_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(tagList(echarts4rOutput(ns("aqi_lineplot"))))
  )
}

aqi_details_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["aqi_lineplot"]] <- renderEcharts4r({
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
        e_tooltip(formatter = htmlwidgets::JS("
          function(params) {
            return params.name;
          }
        ")) %>%
        e_group("aqi_grp")
    }) %>%
      bindCache(state[["map_onclick"]], state[["year"]])
  }

  moduleServer(id, module)
}
