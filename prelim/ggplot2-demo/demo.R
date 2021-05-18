invisible(purrr::map(paste0(
  "prelim/ggplot2-demo/",
  c(
    "packages", "data-clean", "covid19-lvl",
    "function", "plot"
  ),
  ".R"
), source))

#################### AQI Time Series ####################

## Tidy Data
aqi_data <- "data-raw/akl-aqi.csv" %>%
  clean_data("aqi", n_loc = 7) %>%
  dplyr::filter(
    between(year(datetime), 2019, 2020), aqi > 0
  ) %>%
  as_tsibble(index = datetime, key = location) %>%
  tsibble::fill_gaps() %>%
  dplyr::mutate(
    akl_level = get_covid19level(datetime, "AKL"),
    nz_level = get_covid19level(datetime, "NZ_not_AKL"),
    covid19_period = get_covid19period(nz_level, datetime)
  )

# readr::write_csv(aqi_data, "data/akl-aqi-19-20.csv")

## Time Plot
gg_plots(aqi_data)
aqi_data %>%
  dplyr::filter(location == "queen_street") %>%
  gg_plots(check_anom = "SHESD")

## Seasonal Boxplot
gg_botsplot(aqi_data, outlier.shape = 1)
gg_botsplot(aqi_data, period = "years", outlier.shape = 1)

## Seasonal Quantile Plot
gg_seasquantile(aqi_data)
gg_seasquantile(aqi_data, period = "years", q = (0:4) / 4)
gg_seasquantile(aqi_data, period = "day")

## The Effect of Lockdown on AQI
aqi_data %>%
  ggplot(aes(x = covid19_period, y = aqi)) +
  geom_boxplot(outlier.shape = 1) +
  ggplot2::facet_grid(. ~ location) +
  ggplot2::scale_y_continuous(trans = "sqrt")

source("prelim/echarts4r-demo/function.R")

## Categorical Time Series Heatmap
aqi_data %>%
  dplyr::mutate(aqi = ordered(aqi_cat(aqi), names(aqi_pal))) %>%
  cat_heats(aqi, aqi_pal)

cat_heats(aqi_data, akl_level, covid_alert_pal)
cat_heats(aqi_data, nz_level, covid_alert_pal)

## {geofacet}
aqi_data %>%
  gg_seasquantile(q = seq(.01, .9, .01), polar = TRUE) +
  facet_geo(~location, grid = akl_city_grid)

aqi_data %>%
  gg_seasquantile(
    period = "week", q = seq(.01, .9, .01),
    polar = TRUE
  ) +
  facet_geo(~location, grid = akl_city_grid)


# #################### Air Temp Time Series ####################
#
# ## Tidy Data
# at_data <- "data-raw/akl-airtemp.csv" %>%
#   clean_data("airtemp", n_loc = 5) %>%
#   dplyr::filter(between(year(datetime), 2019, 2020)) %>%
#   as_tsibble(index = datetime, key = location) %>%
#   tsibble::fill_gaps()
# # readr::write_csv(at_data, "data/akl-airtemp-19-20.csv")
#
# ## Time Plot
# gg_plots(at_data, check_anom = "SHESD")
#
# ## Seasonal Boxplot
# gg_botsplot(at_data, outlier.shape = 1)
# gg_botsplot(at_data, period = "day", outlier.shape = 1)

# #################### {feasts} ####################
#
# ## Seasonal Plot
# feasts::gg_season(at_data)
#
# ## (/P)ACF Plot
# at_data %>%
#   feasts::ACF() %>%
#   autoplot()
#
# at_data %>%
#   feasts::PACF() %>%
#   autoplot()
#
# ## Lag Plot
# at_data %>% # Very slow # Not sure how to specify season
#   dplyr::filter(location == "takapuna") %>%
#   feasts::gg_lag(geom = "point")
#
# ## Seasonal Plot
# feasts::gg_season(aqi_data)
#
# ## (/P)ACF Plot
# aqi_data %>%
#   feasts::ACF() %>%
#   autoplot()
#
# aqi_data %>%
#   feasts::PACF() %>%
#   autoplot()
#
# ## Lag Plot
# aqi_data %>% # Very slow # Not sure how to specify season
#   dplyr::filter(location == "takapuna") %>%
#   feasts::gg_lag(geom = "point")
