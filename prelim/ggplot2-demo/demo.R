invisible(purrr::map(
  paste0(
    "prelim/ggplot2-demo/",
    c(
      "packages", "data_clean",
      "function", "plot", "feasts-src"
    ),
    ".R"
  ), source
))

#################### Air Temp Time Series ####################

## Tidy Data
at_data <- "data-raw/akl-airtemp.csv" %>%
  clean_data("airtemp", n_loc = 5) %>%
  dplyr::filter(between(year(datetime), 2019, 2020)) %>%
  as_tsibble(index = datetime, key = location) %>%
  tsibble::fill_gaps()

## Time Plot
gg_plots(at_data)

## Seasonal Boxplot
gg_botsplot(at_data)

#################### AQI Time Series ####################

## Tidy Data
aqi_data <- "data-raw/akl-aqi.csv" %>%
  clean_data("aqi", n_loc = 5) %>%
  dplyr::filter(
    between(year(datetime), 2019, 2020),
    aqi > 0
  ) %>%
  as_tsibble(index = datetime, key = location) %>%
  tsibble::fill_gaps()

## Time Plot
gg_plots(aqi_data)

## Seasonal Boxplot
gg_botsplot(aqi_data)

#################### {feasts} ####################

## Seasonal Plot
feasts::gg_season(at_data)

## (/P)ACF Plot
at_data %>%
  feasts::ACF() %>%
  autoplot()

at_data %>%
  feasts::PACF() %>%
  autoplot()

## Lag Plot
at_data %>% # Very slow # Not sure how to specify season
  dplyr::filter(location == "takapuna") %>%
  feasts::gg_lag(geom = "point")

## Seasonal Plot
feasts::gg_season(aqi_data)

## (/P)ACF Plot
aqi_data %>%
  feasts::ACF() %>%
  autoplot()

aqi_data %>%
  feasts::PACF() %>%
  autoplot()

## Lag Plot
aqi_data %>% # Very slow # Not sure how to specify season
  dplyr::filter(location == "takapuna") %>%
  feasts::gg_lag(geom = "point")
