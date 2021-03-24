invisible(purrr::map(paste0(
  "prelim/echarts4r-demo/",
  c("packages", "function", "plot"),
  ".R"
), source))

#################### Air Temp Time Series ####################

## Tidy Data
at_data <- "data/akl-airtemp-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

## Extract Timestamp from tsibble Object
ts_timestamp(as_tibble(at_data))
ts_timestamp(at_data, period = lubridate::days(7))
ts_timestamp(at_data, period = 24L)
ts_timestamp(at_data, period = 24)
ts_timestamp(at_data, period = 3.14)
ts_timestamp(at_data, period = "month")
