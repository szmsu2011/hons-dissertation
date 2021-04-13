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
    e_chart(
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
