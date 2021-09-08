source(textConnection(readLines("../akl-air-quality/data/prep.R")[seq_len(20)]))

## Break full data into locations
data <- map(unique(data[["location"]]), function(loc) {
  data %>%
    filter(location == loc) %>%
    select(-c(!!sym("pm2.5"), pm10, no2, so2, co, o3, location, aqi_cat))
}) %>%
  set_names(unique(data[["location"]]))

## Remove starting NAs (row)
data <- map(data, function(x) {
  starting_na <- max(map_dbl(array_branch(x, 2), function(y) {
    which.min(is.na(y)) - 1
  }))
  if (starting_na > 0) {
    x[-seq_len(starting_na), ]
  } else {
    x
  }
})

## Remove >20% NA columns and locations with >20% NA AQI
data <- map(data, function(x) {
  select_if(x, ~ mean(is.na(.)) < .2)
})
data <- data[map_lgl(data, function(x) {
  "aqi" %in% names(x)
})]

## Fill NA with mean
data <- map(data, function(x) {
  mutate(x, across(-c(datetime), function(z) {
    replace_na(z, mean(z, na.rm = TRUE))
  }))
})

## Aggregate to daily observation
vector_mean <- function(theta_deg, r = 1, offset_deg = 0, out) {
  theta_rad <- (theta_deg - offset_deg) * pi / 180
  xy <- cbind(x = r * cos(theta_rad), y = r * sin(theta_rad)) %>%
    colMeans(na.rm = TRUE)
  x <- xy[1]
  y <- xy[2]
  if (out == "theta") {
    (atan(y / x) / pi + (x < 0) + 2 * (x > 0 & y < 0)) * 180
  } else if (out == "r") {
    sqrt(x^2 + y^2)
  }
}
data_raw <- map(data, function(x) {
  if ("ws" %in% names(x)) class(x[["ws"]]) <- "ws"
  if ("wind_dir" %in% names(x)) class(x[["wind_dir"]]) <- "wd"
  x %>%
    as_tibble() %>%
    group_by(date = as_date(datetime)) %>%
    select(-datetime) %>%
    summarise(
      across(!contains("date"), function(y) {
        if (class(y) == "wd") {
          vector_mean(y, ws, out = "theta")
        } else {
          mean(y, na.rm = TRUE)
        }
      })
    )
})

## Categorise wind direction
data <- map(data_raw, function(x) {
  if ("wind_dir" %in% names(x)) {
    mutate(x, wind_dir = cut(wind_dir, seq(0, 360, 30), include.lowest = TRUE))
  } else {
    x
  }
})
