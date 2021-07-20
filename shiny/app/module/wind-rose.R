wind_rose_ui <- function(id) {
  ns <- NS(id)

  tagList(echarts4rOutput(ns("wind_rose")))
}

wind_rose_mod <- function(id, state) {
  module <- function(input, output, session) {
    output[["wind_rose"]] <- renderEcharts4r({
      pat_ws <- "(\\(|\\[)(\\d),(\\d+\\.?\\d*)(\\])"
      pat_wd <- "(\\(|\\[)(\\d+),(\\d+)(\\])"

      e <- (.data <- wind_data %>%
        filter(
          location == "takapuna",
          year(datetime) == 2020,
          month(datetime) == 1
        ) %>%
        mutate(
          ws = cut(ws, c(0:3 * 2, max(ws, na.rm = TRUE)), include.lowest = TRUE),
          wind_dir = case_when(
            wind_dir >= 180 ~ wind_dir - 180,
            TRUE ~ wind_dir + 180
          ),
          wd = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE),
          ws = gsub(pat_ws, "\\2-\\3 m/s", ws),
          wd = gsub(pat_wd, "% Towards \\2-\\3 deg", wd) %>%
            factor(gsub(pat_wd, "% Towards \\2-\\3 deg", levels(wd)))
        ) %>%
        as_tibble() %>%
        count(ws, wd, .drop = FALSE) %>%
        mutate(p = round(n / sum(n) * 100, 2)) %>%
        select(-n) %>%
        pivot_wider(names_from = ws, values_from = p, values_fill = 0)) %>%
        e_charts(wd) %>%
        e_angle_axis(wd, type = "category", axisLabel = list(formatter = "")) %>%
        e_radius_axis(max = 50, axisLabel = list(formatter = "{value}%")) %>%
        e_polar() %>%
        e_legend(right = 0, orient = "vertical") %>%
        e_tooltip(trigger = "axis") %>%
        e_title("Jan")

      for (y in names(.data)[-1]) {
        e <- e %>%
          e_bar_(y, coord_system = "polar", stack = "stack")
      }

      e
    })
  }

  moduleServer(id, module)
}
