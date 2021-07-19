import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "prelim/ggplot2-demo/",
  c("packages", "data-clean", "covid19-lvl", "function", "plot")
)

# wind_dir_data <- read_wind_dir_data(
#   "data-raw/akl-wind-dir.csv",
#   c(
#     "glen_eden", "henderson", "penrose", "queen_street",
#     "takapuna", "pakuranga", "khyber_pass"
#   )
# )

# readr::write_csv(wind_dir_data, "data/akl-wind-dir.csv")

wind_dir_data <- "data/akl-wind-dir.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

wind_data <- "data/akl-env-data.csv" %>%
  readr::read_csv(
    col_types = paste0("Tc", paste(rep("d", 14), collapse = "")),
    locale = locale(tz = "Pacific/Auckland")
  ) %>%
  as_tsibble(index = datetime, key = location) %>%
  dplyr::filter(location %in% unique(wind_dir_data[["location"]])) %>%
  dplyr::select(ws) %>%
  dplyr::inner_join(wind_dir_data) %>%
  dplyr::filter(!is.na(ws), !is.na(wind_dir))

## Wind Data Time Series in Glen Eden, 2020
wind_data %>%
  dplyr::filter(location == "glen_eden", year(datetime) == 2020) %>%
  as_tibble() %>%
  dplyr::group_by(date = as_date(datetime)) %>%
  dplyr::summarise(
    x = vector_mean(wind_dir, ws, 180, 1),
    y = vector_mean(wind_dir, ws, 180, 2)
  ) %>%
  dplyr::mutate(
    month = month(date, label = TRUE),
    r = sqrt(x^2 + y^2),
    theta = acos(x / r) * 180 / pi
  ) %>%
  ggplot(aes(theta, r, xend = theta, yend = 0)) +
  geom_segment(col = "steelblue", alpha = .8) +
  ggplot2::scale_y_continuous(expand = expansion()) +
  ggplot2::scale_x_continuous(breaks = seq(0, 315, 45), limits = c(0, 360)) +
  ggplot2::coord_polar() +
  ggplot2::facet_wrap(vars(month), nrow = 2) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Wind Speed and Direction in Glen Eden, 2020",
    x = "Wind Direction (Pointing Towards)",
    y = "Wind Speed"
  )

# akl <- akl_city_grid %>%
#   filter(name %in% unique(wind_data[["location"]])) %>%
#   mutate(row = case_when(row == 3 ~ 2, row == 5 ~ 3, TRUE ~ 1))
#
# wind_data %>%
#   dplyr::filter(location == "takapuna") %>%
#   as_tibble() %>%
#   dplyr::group_by(WoY = week(datetime)) %>%
#   dplyr::summarise(
#     x = vector_mean(wind_dir, ws, 180, 1),
#     y = vector_mean(wind_dir, ws, 180, 2)
#   ) %>%
#   dplyr::mutate(
#     r = sqrt(x^2 + y^2),
#     theta = acos(x / r) * 180 / pi
#   ) %>%
#   ggplot(aes(theta, 0, xend = theta, yend = r, col = WoY)) +
#   geom_segment(arrow = arrow(15, length = unit(.02, "npc"))) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 315, 45), limits = c(0, 360)) +
#   ggplot2::scale_colour_gradient2(
#     low = "blue", mid = "red", high = "orange",
#     midpoint = 26
#   ) +
#   ggplot2::coord_polar() +
#   ggplot2::labs(
#     title = "Average Wind Velocity by Week of Year in Takapuna",
#     x = "Direction",
#     y = "ABS(Mean Wind Velocity)"
#   )
#
# wind_data %>%
#   dplyr::filter(location == "takapuna") %>%
#   as_tibble() %>%
#   dplyr::group_by(Hour = hour(datetime)) %>%
#   dplyr::summarise(
#     x = vector_mean(wind_dir, ws, 180, 1),
#     y = vector_mean(wind_dir, ws, 180, 2)
#   ) %>%
#   dplyr::mutate(
#     r = sqrt(x^2 + y^2),
#     theta = acos(x / r) * 180 / pi
#   ) %>%
#   ggplot(aes(theta, 0, xend = theta, yend = r, col = Hour)) +
#   geom_segment(arrow = arrow(15, length = unit(.02, "npc"))) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 315, 45), limits = c(0, 360)) +
#   ggplot2::scale_colour_gradient2(
#     low = "blue", mid = "red", high = "orange",
#     midpoint = 12.5
#   ) +
#   ggplot2::coord_polar() +
#   ggplot2::labs(
#     title = "Average Wind Velocity by Hour of Day in Takapuna",
#     x = "Direction",
#     y = "ABS(Mean Wind Velocity)"
#   )
#
# wind_data %>%
#   as_tibble() %>%
#   dplyr::group_by(WoY = week(datetime), location) %>%
#   dplyr::summarise(
#     x = vector_mean(wind_dir, ws, 180, 1),
#     y = vector_mean(wind_dir, ws, 180, 2)
#   ) %>%
#   dplyr::mutate(
#     r = sqrt(x^2 + y^2),
#     theta = acos(x / r) * 180 / pi
#   ) %>%
#   ggplot(aes(theta, 0, xend = theta, yend = r, col = WoY)) +
#   geom_segment(arrow = arrow(15, length = unit(.02, "npc"))) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 315, 45), limits = c(0, 360)) +
#   ggplot2::scale_colour_gradient2(
#     low = "blue", mid = "red", high = "orange",
#     midpoint = 26
#   ) +
#   facet_geo(~location, grid = akl) +
#   ggplot2::coord_polar() +
#   ggplot2::labs(
#     title = "Average Wind Velocity by Week of Year",
#     x = "Direction",
#     y = "ABS(Mean Wind Velocity)"
#   )
#
# wind_data %>%
#   as_tibble() %>%
#   dplyr::group_by(Hour = hour(datetime), location) %>%
#   dplyr::summarise(
#     x = vector_mean(wind_dir, ws, 180, 1),
#     y = vector_mean(wind_dir, ws, 180, 2)
#   ) %>%
#   dplyr::mutate(
#     r = sqrt(x^2 + y^2),
#     theta = acos(x / r) * 180 / pi
#   ) %>%
#   ggplot(aes(theta, 0, xend = theta, yend = r, col = Hour)) +
#   geom_segment(arrow = arrow(15, length = unit(.02, "npc"))) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 315, 45), limits = c(0, 360)) +
#   ggplot2::scale_colour_gradient2(
#     low = "blue", mid = "red", high = "orange",
#     midpoint = 12.5
#   ) +
#   facet_geo(~location, grid = akl) +
#   ggplot2::coord_polar() +
#   ggplot2::labs(
#     title = "Average Wind Velocity by Hour of Day",
#     x = "Direction",
#     y = "ABS(Mean Wind Velocity)"
#   )
