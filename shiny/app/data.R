data <- read_csv("data/akl-env-data.csv",
  col_types = paste0("Tc", paste(rep("d", 14), collapse = "")),
  locale = locale(tz = "Pacific/Auckland")
) %>%
  mutate(
    datetime = as_datetime(datetime) + hours(12),
    aqi_cat = aqi_cat(aqi),
    hour = hour(datetime)
  ) %>%
  filter(year(datetime) > 2015) %>%
  as_tsibble(index = datetime, key = location)

aqi_data <- select(data, c(aqi, aqi_cat))
con_data <- select(data, aqi, !!sym("pm2.5"), pm10, no2, so2, co, o3)

station <- tibble(
  site = c(
    "Queen Street", "Glen Eden", "Penrose", "Henderson", "Pakuranga",
    "Takapuna", "Patumahoe", "Customs St", "Khyber Pass", "Papatoetoe"
  ),
  lng = c(
    174.7650, 174.6447, 174.8199, 174.6239, 174.8709,
    174.7651, 174.8282, 174.7683, 174.7693, 174.8645
  ),
  lat = -c(
    36.8482, 36.9225, 36.9045, 36.8679, 36.9130,
    36.7899, 37.2047, 36.8450, 36.8653, 36.9756
  )
)
