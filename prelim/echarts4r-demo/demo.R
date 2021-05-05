invisible(purrr::map(paste0(
  "prelim/echarts4r-demo/",
  c("packages", "function", "plot"),
  ".R"
), source))

#################### Air Temp + AQI Time Series ####################

at_data <- "data/akl-airtemp-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

aqi_data <- "data/akl-aqi-19-20.csv" %>%
  readr::read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  as_tsibble(index = datetime, key = location)

env_data <- dplyr::full_join(at_data, aqi_data)

## {echarts4r} Time Series Heatmap
e_heats(env_data, aggregate = "mean") %>% e_title("airtemp")
