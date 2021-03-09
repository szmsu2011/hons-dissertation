invisible(sapply(
  paste0(
    "prelim/ggplot2_demo/",
    c("packages", "data_clean", "plot"),
    ".R"
  ),
  source
))

#################### Rainfall Time Series ####################

## Tidy Data
rf_data <- "data-raw/akl-rainfall.csv" %>%
  clean_data("rainfall") %>%
  tidyr::drop_na() %>%
  dplyr::filter(between(year(datetime), 1980, 1990)) %>%
  as_tsibble(index = datetime, key = location)

## Time Plot
rf_data %>%
  fabletools::autoplot(rainfall) +
  labs(x = "date")

## Choose Albert Park
abrf_data <- rf_data %>%
  dplyr::filter(location == "albert_park") %>%
  mutate(date = lubridate::as_date(datetime)) %>%
  tsibble(index = date, key = location) %>%
  fill_gaps()

## Seasonal Plot
abrf_data %>%
  gg_season(rainfall, period = "year")

#FIXME
## Seasonal Subseries Plot
# abrf_data %>%
#   gg_subseries(rainfall)

## Lag Plot
abrf_data %>%
  gg_lag(rainfall, geom = "point")

## (/P)ACF Plot
abrf_data %>%
  feasts::ACF(rainfall) %>%
  autoplot()
abrf_data %>%
  feasts::PACF(rainfall) %>%
  autoplot()
