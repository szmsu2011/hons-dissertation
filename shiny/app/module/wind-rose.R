wind_rose_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("wind_rose")))
}

wind_rose_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["wind_rose"]] <- renderEcharts4r({
      .data <- wind_data %>%
        filter(
          location == "takapuna",
          year(datetime) == 2020,
          month(datetime) == 1
        ) %>%
        mutate(
          wind_dir = case_when(
            wind_dir >= 180 ~ wind_dir - 180,
            TRUE ~ wind_dir + 180
          ),
          wd = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE),
          ws = cut(ws, c(seq(0, 6, 2), max(ws, na.rm = TRUE)))
        ) %>%
        as_tibble() %>%
        count(ws, wd, .drop = FALSE) %>%
        mutate(p = n / sum(n)) %>%
        select(-n) %>%
        pivot_wider(names_from = ws, values_from = p, values_fill = 0)

      name <- names(.data)[-1]

      e <- .data %>%
        e_charts(wd) %>%
        e_angle_axis(ws, type = "category") %>%
        e_radius_axis(max = .5) %>%
        e_polar()

      for (y in name) {
        e <- e %>%
          e_bar_(y,
            coord_system = "polar",
            name = gsub("(\\(|\\[)(\\d),(\\d+\\.?\\d*)(\\)|\\])", "\\2-\\3", y) %>%
              paste("m/s"),
            stack = "stack"
          )
      }

      e
    })
  }

  moduleServer(id, module)
}
