invisible(purrr::map(paste0(
  "prelim/ggplot2-demo/",
  c(
    "packages", "data-clean",
    "function", "plot"
  ),
  ".R"
), source))

#################### Air Temp Time Series ####################

## Tidy Data
at_data <- "data-raw/akl-airtemp.csv" %>%
  clean_data("airtemp", n_loc = 5) %>%
  dplyr::filter(between(year(datetime), 2019, 2020)) %>%
  as_tsibble(index = datetime, key = location) %>%
  tsibble::fill_gaps()
# readr::write_csv(at_data, "data/akl-airtemp-19-20.csv")

## Time Plot
gg_plots(at_data, check_anom = "SHESD")

## Seasonal Boxplot
gg_botsplot(at_data, outlier.shape = 1)
gg_botsplot(at_data, period = "day", outlier.shape = 1)

#################### AQI Time Series ####################

## Tidy Data
aqi_data <- "data-raw/akl-aqi.csv" %>%
  clean_data("aqi", n_loc = 5) %>%
  dplyr::filter(
    between(year(datetime), 2019, 2020), aqi > 0
  ) %>%
  as_tsibble(index = datetime, key = location) %>%
  tsibble::fill_gaps()
# readr::write_csv(aqi_data, "data/akl-aqi-19-20.csv")

## Time Plot
gg_plots(aqi_data, check_anom = "SHESD")

aqi_data %>%
  dplyr::filter(location == "queen_street") %>%
  gg_plots(check_anom = "SHESD")

## Seasonal Boxplot
gg_botsplot(aqi_data, outlier.shape = 1)

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
