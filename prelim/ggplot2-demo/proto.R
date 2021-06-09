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
  as_tsibble(index = datetime, key = location) %>%
  dplyr::filter(location == "glen_eden")

## Mean Wind Velocity by Month, Hour of Day, Week of Year and Day of Year
wind_dir_data %>%
  as_tibble() %>%
  dplyr::group_by(Month = month(datetime)) %>%
  dplyr::summarise(
    x = vector_mean(wind_dir, 1, 180, 1),
    y = vector_mean(wind_dir, 1, 180, 2)
  ) %>%
  dplyr::mutate(
    r = sqrt(x^2 + y^2),
    theta = acos(x / r) * 180 / pi
  ) %>%
  ggplot(aes(theta, 0, xend = theta, yend = r, col = Month)) +
  geom_segment(arrow = arrow(15, length = unit(.04, "npc"))) +
  ggplot2::scale_colour_gradient2(
    low = "blue", mid = "red", high = "orange",
    midpoint = 6.5
  ) +
  ggplot2::xlim(c(0, 360)) +
  ggplot2::coord_polar() +
  ggplot2::labs(y = "ABS(Mean Wind Velocity)")

wind_dir_data %>%
  as_tibble() %>%
  dplyr::group_by(Hour = hour(datetime)) %>%
  dplyr::summarise(
    x = vector_mean(wind_dir, 1, 180, 1),
    y = vector_mean(wind_dir, 1, 180, 2)
  ) %>%
  dplyr::mutate(
    r = sqrt(x^2 + y^2),
    theta = acos(x / r) * 180 / pi
  ) %>%
  ggplot(aes(theta, 0, xend = theta, yend = r, col = Hour)) +
  geom_segment(arrow = arrow(15, length = unit(.04, "npc"))) +
  ggplot2::scale_colour_gradient2(
    low = "blue", mid = "red", high = "orange",
    midpoint = 12.5
  ) +
  ggplot2::xlim(c(0, 360)) +
  ggplot2::coord_polar() +
  ggplot2::labs(y = "ABS(Mean Wind Velocity)")

wind_dir_data %>%
  as_tibble() %>%
  dplyr::group_by(WoY = week(datetime)) %>%
  dplyr::summarise(
    x = vector_mean(wind_dir, 1, 180, 1),
    y = vector_mean(wind_dir, 1, 180, 2)
  ) %>%
  dplyr::mutate(
    r = sqrt(x^2 + y^2),
    theta = acos(x / r) * 180 / pi
  ) %>%
  ggplot(aes(theta, 0, xend = theta, yend = r, col = WoY)) +
  geom_segment(arrow = arrow(15, length = unit(.04, "npc"))) +
  ggplot2::scale_colour_gradient2(
    low = "blue", mid = "red", high = "orange",
    midpoint = 26
  ) +
  ggplot2::xlim(c(0, 360)) +
  ggplot2::coord_polar() +
  ggplot2::labs(y = "ABS(Mean Wind Velocity)")
