invisible(purrr::map(
  paste0(
    "prelim/ggplot2_demo/",
    c("packages", "data_clean"),
    ".R"
  ),
  source
))

#################### Air Temp Time Series ####################

## Tidy Data
at_data <- "data-raw/akl-airtemp.csv" %>%
  clean_data("airtemp", n_loc = 5) %>%
  dplyr::filter(between(year(datetime), 2019, 2020)) %>%
  as_tsibble(index = datetime, key = location) %>%
  fill_gaps()

## Time Plot
at_data %>%
  fabletools::autoplot(airtemp) +
  labs(x = "date")

## Seasonal Plot
at_data %>% # Very slow
  gg_season(airtemp, period = "day") +
  theme(legend.position = "none") +
  labs(x = "hour of day")

at_data %>% # Very slow # Need more work to see pattern
  dplyr::filter(location == "takapuna") %>%
  gg_season(airtemp, period = "day") +
  # geom_smooth() + # Unable to ignore group
  theme(legend.position = "none") +
  labs(x = "hour of day")

at_data %>%
  gg_season(airtemp, period = "week") +
  theme(legend.position = "none") +
  labs(x = "day of week")

at_data %>% # Bad automatic colour scheme
  gg_season(airtemp, period = "year") +
  geom_smooth(method = "gam") +
  labs(x = "month")

# FIXME # Very slow, freezes
## Seasonal Subseries Plot
# at_data %>%
#   gg_subseries(airtemp)

## Lag Plot
at_data %>% # Very slow # Not sure how to specify season
  dplyr::filter(location == "takapuna") %>%
  gg_lag(airtemp, geom = "point")

## (/P)ACF Plot
at_data %>%
  feasts::ACF(airtemp) %>%
  autoplot()

at_data %>%
  feasts::PACF(airtemp) %>%
  autoplot()

#################### AQI Time Series ####################

## Tidy Data
aqi_data <- "data-raw/akl-aqi.csv" %>%
  clean_data("aqi", n_loc = 5) %>%
  dplyr::filter(between(year(datetime), 2019, 2020)) %>%
  as_tsibble(index = datetime, key = location) %>%
  fill_gaps()

## Time Plot
aqi_data %>%
  fabletools::autoplot(aqi) +
  labs(x = "date")

## Seasonal Plot
aqi_data %>% # Very slow
  gg_season(aqi, period = "day") +
  theme(legend.position = "none") +
  labs(x = "hour of day") +
  coord_cartesian(ylim = c(0, 100))

aqi_data %>%
  gg_season(aqi, period = "week") +
  theme(legend.position = "none") +
  labs(x = "day of week") +
  coord_cartesian(ylim = c(0, 100))

aqi_data %>% # Bad automatic colour scheme
  gg_season(aqi, period = "year") +
  labs(x = "month") +
  coord_cartesian(ylim = c(0, 100))

# FIXME # Very slow, freezes
## Seasonal Subseries Plot
# aqi_data %>%
#   gg_subseries(aqi)


## Lag Plot
aqi_data %>% # Very slow # Not sure how to specify season
  dplyr::filter(location == "takapuna") %>%
  gg_lag(aqi, geom = "point")


## (/P)ACF Plot
aqi_data %>%
  feasts::ACF(aqi) %>%
  autoplot()

aqi_data %>%
  feasts::PACF(aqi) %>%
  autoplot()


