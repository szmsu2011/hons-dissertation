rstudioapi::getActiveDocumentContext()$path %>%
  dirname %>%
  setwd
source("packages.R")

as_datetime = function(x) {
  dnt = double(length(x))
  dnt[grep("-", x)] = x[grep("-", x)] %>%
    strptime("%Y-%m-%d %H:%M:%S") %>%
    format("%Y-%m-%d %H:%M:%S")
  dnt[grep("/", x)] = x[grep("/", x)] %>%
    strptime("%d/%m/%Y %H:%M") %>%
    format("%Y-%m-%d %H:%M:%S")
  dnt %>% as.POSIXct
}

read_data = function(x, value_name = "value") {
  data = read.csv(x)
  names(data) = c("datetime", gsub(".*@ (\\w)|[.]", "\\1", data[3, -1]))
  data = data[-(1:5), ]
  data$datetime %<>% as_datetime
  data %<>% tidyr::gather(location, value, names(data)[-1])
  data$value %<>% as.numeric
  names(data)[3] = value_name
  data %>%
    na.omit %>%
    as_tsibble(location, datetime)
}

data = read_data("data/akl_rainfall.csv", "rainfall")
