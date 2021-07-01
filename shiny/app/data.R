data <- read_csv("data/akl-env-data.csv",
  col_types = paste0("Tc", paste(rep("d", 14), collapse = "")),
  locale = locale(tz = "Pacific/Auckland")
) %>%
  filter(year(datetime) > 2015) %>%
  mutate(
    aqi_cat = aqi_cat(aqi),
    date = as_date(datetime),
    hour = hour(datetime)
  ) %>%
  as_tsibble(index = datetime, key = location)

aqi_data <- select(data, c(aqi, aqi_cat))
data <- select(data, -c(aqi, aqi_cat))
