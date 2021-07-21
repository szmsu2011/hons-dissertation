wind_rose_ui <- function(id) {
  ns <- NS(id)

  column(
    column(
      tagList(echarts4rOutput(ns("wind_rose1"))),
      tagList(echarts4rOutput(ns("wind_rose3"))),
      tagList(echarts4rOutput(ns("wind_rose5"))),
      tagList(echarts4rOutput(ns("wind_rose7"))),
      tagList(echarts4rOutput(ns("wind_rose9"))),
      tagList(echarts4rOutput(ns("wind_rose11"))),
      width = 6
    ),
    column(
      tagList(echarts4rOutput(ns("wind_rose2"))),
      tagList(echarts4rOutput(ns("wind_rose4"))),
      tagList(echarts4rOutput(ns("wind_rose6"))),
      tagList(echarts4rOutput(ns("wind_rose8"))),
      tagList(echarts4rOutput(ns("wind_rose10"))),
      tagList(echarts4rOutput(ns("wind_rose12"))),
      width = 6
    ),
    width = 12
  )
}

wind_rose_mod <- function(id, state) {
  module <- function(input, output, session) {
    pat_ws <- "(\\(|\\[)(\\d),(\\d+\\.?\\d*)(\\])"
    pat_wd <- "(\\(|\\[)(\\d+),(\\d+)(\\])"

    e_wind_rose <- function(wind_data, yr, mth, loc) {
      e <- (data <- wind_data %>%
        filter(
          location == make_clean_names(loc),
          year(datetime) == yr,
          month(datetime) == mth
        ) %>%
        mutate(
          wind_dir = case_when(
            wind_dir >= 180 ~ wind_dir - 180,
            TRUE ~ wind_dir + 180
          ),
          wd = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE),
          wd = gsub(pat_wd, "% Towards \\2-\\3 deg", wd) %>%
            factor(gsub(pat_wd, "% Towards \\2-\\3 deg", levels(wd))),
          ws = cut(ws,
            unique(c(0:3 * 2, max(ws, na.rm = TRUE))),
            include.lowest = TRUE
          ),
          ws = gsub(pat_ws, "\\2-\\3 m/s", ws)
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
        e_title(paste(yr, month(mth, label = TRUE)), loc) %>%
        e_tooltip(trigger = "axis")

      for (y in names(data)[-1]) {
        e <- e %>%
          e_bar_(y, coord_system = "polar", stack = "stack")
      }

      e
    }

    map(1:12, function(i) {
      output[["wind_rose" %>% paste0(i)]] <- renderEcharts4r({
        e_wind_rose(wind_data, state[["year_wind"]], i, state[["map_onclick"]])
      }) %>%
        bindCache(state[["year_wind"]], state[["map_onclick"]], i)
    })
  }

  moduleServer(id, module)
}
