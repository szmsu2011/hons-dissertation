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
ts_timestamp(as_tibble(at_data), custom_period = NULL)
ts_timestamp(at_data, custom_period = NULL)
ts_timestamp(at_data, custom_period = lubridate::days(7))
ts_timestamp(at_data, custom_period = 24L)
ts_timestamp(at_data, custom_period = 3.14)
ts_timestamp(at_data, custom_period = "1 day")
