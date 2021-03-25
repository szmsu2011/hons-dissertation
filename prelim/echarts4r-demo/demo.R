invisible(purrr::map(paste0(
  "prelim/echarts4r-demo/",
  c("packages", "function", "plot"),
  ".R"
), source))

#################### Air Temp Time Series ####################

at_data <- "data/akl-airtemp-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

## Extract Timestamp from tsibble Object
ts_timestamp(at_data, period = lubridate::hours(2))
ts_timestamp(at_data, period = 5)
ts_timestamp(at_data, period = 3.14)
ts_timestamp(at_data, period = "month")

## {echarts4r} Time Series Heatmap
e_heats(at_data)

#################### AQI Time Series ####################

aqi_data <- "data/akl-aqi-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

## {echarts4r} Time Series Heatmap
e_heats(aqi_data)
