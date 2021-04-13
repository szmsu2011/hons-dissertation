source("prelim/echarts4r-demo/packages.R")

#################### Air Temp + AQI Time Series ####################

at_data <- "data/akl-airtemp-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

aqi_data <- "data/akl-aqi-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

env_data <- dplyr::full_join(at_data, aqi_data) %>%
  dplyr::mutate(
    .Y = lubridate::year(datetime),
    .M = lubridate::month(datetime, label = TRUE),
    .YM = tsibble::yearmonth(datetime),
    .D = lubridate::day(datetime),
    .H = lubridate::hour(datetime)
  )

#################### {plotly} Hourly Heatmap ####################

p <- env_data %>%
  dplyr::filter(location == "takapuna") %>%
  ggplot(aes(.D, .H, fill = airtemp)) +
  geom_tile(col = NA) +
  scale_fill_viridis(
    option = "C",
    na.value = "white"
  ) +
  ggplot2::scale_x_continuous(breaks = c(1, 10, 20, 31)) +
  ggplot2::scale_y_continuous(
    breaks = 0:23,
    trans = "reverse"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = element_blank(),
    panel.spacing = unit(.2, "mm")
  ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::facet_grid(.Y ~ .M)

plotly::ggplotly(p)

#################### {echarts4r} Hourly Heatmap ####################

ym <- unique(env_data[[".YM"]])
n.m <- length(unique(env_data[[".M"]]))
n.y <- length(unique(env_data[[".Y"]]))

e <- list()

for (i in seq_along(ym)) {
  e[[i]] <- env_data %>%
    dplyr::mutate(
      .D = as.character(.D),
      .H = as.character(.H)
    ) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(
      location == "takapuna",
      .YM == ym[i]
    ) %>%
    e_charts(
      .D,
      width = paste0(100 / n.m - 1, "vw"),
      height = paste0(100 / n.y - 1, "vh"),
      elementId = as.character(i)
    ) %>%
    e_heatmap(.H, airtemp) %>%
    e_visual_map(
      airtemp,
      inRange = list(
        color = gsub("FF", "", viridis(11, option = "C"))
      ),
      calculable = FALSE
    ) %>%
    e_y_axis(inverse = TRUE)
}

e[[i]] <- e_connect(e[[i]], as.character(seq_len(i - 1)))

rlang::eval_tidy(new_quosure(
  expr(e_arrange(!!!e, rows = n.y, cols = n.m)),
  env = env(e = e)
))

#################### {echarts4r} Calendar Heatmap ####################

env_data %>%
  as_tibble() %>%
  dplyr::filter(location == "takapuna") %>%
  dplyr::mutate(.date = lubridate::date(datetime)) %>%
  dplyr::group_by(.date) %>%
  dplyr::summarise(
    airtemp = mean(airtemp, na.rm = TRUE),
    aqi = mean(aqi, na.rm = TRUE)
  ) %>%
  dplyr::mutate(.Y = lubridate::year(.date)) %>%
  dplyr::group_by(.Y) %>%
  e_charts(.date) %>%
  e_calendar(range = "2019", top = "40") %>%
  e_calendar(range = "2020", top = "260") %>%
  e_heatmap(airtemp, coord_system = "calendar") %>%
  e_visual_map(
    airtemp,
    inRange = list(
      color = gsub("FF", "", viridis(11, option = "C"))
    )
  ) %>%
  e_tooltip()

#################### {echarts4r} Line-Scatter Linking ####################

loc <- unique(env_data[["location"]])

env_data %>% # Very Slow
  as_tibble() %>%
  dplyr::select(datetime, location, airtemp) %>%
  tidyr::pivot_wider(
    names_from = location,
    values_from = airtemp
  ) %>%
  e_charts(datetime) %>%
  e_line_(loc[1], symbol = "none") %>%
  e_line_(loc[2], x_index = 1, y_index = 1, symbol = "none") %>%
  e_line_(loc[3], x_index = 2, y_index = 2, symbol = "none") %>%
  e_line_(loc[4], x_index = 3, y_index = 3, symbol = "none") %>%
  e_line_(loc[5], x_index = 4, y_index = 4, symbol = "none") %>%
  e_grid(height = "11%", top = "5%") %>%
  e_grid(height = "11%", top = "23%") %>%
  e_grid(height = "11%", top = "41%") %>%
  e_grid(height = "11%", top = "59%") %>%
  e_grid(height = "11%", top = "77%") %>%
  e_y_axis(gridIndex = 0, index = 0) %>%
  e_x_axis(gridIndex = 0, index = 0) %>%
  e_y_axis(gridIndex = 1, index = 1) %>%
  e_x_axis(gridIndex = 1, index = 1) %>%
  e_y_axis(gridIndex = 2, index = 2) %>%
  e_x_axis(gridIndex = 2, index = 2) %>%
  e_y_axis(gridIndex = 3, index = 3) %>%
  e_x_axis(gridIndex = 3, index = 3) %>%
  e_y_axis(gridIndex = 4, index = 4) %>%
  e_x_axis(gridIndex = 4, index = 4) %>%
  e_datazoom(x_index = 0:4) %>%
  e_tooltip("axis") %>%
  e_title("airtemp")

## {plotly} equivalent is much faster
invisible(purrr::map(paste0(
  "prelim/ggplot2-demo/",
  c("packages", "function", "plot"),
  ".R"
), source))

gg_plots(env_data, airtemp) %>% plotly::ggplotly()
