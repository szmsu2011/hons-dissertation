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

Max <- function(x) {
  x <- max(x, na.rm = TRUE)
  ifelse(x > -Inf, round(x), NA)
}
Mean <- function(x) round(mean(x, na.rm = TRUE))
Median <- function(x) round(median(x, na.rm = TRUE))

fmt_date <- stamp("March 1, 1999")

# Week <- function(date) {
#   map(
#     unique(year(date)),
#     function(x) {
#       d <- date[year(date) == x]
#       w <- (seq(yday(first(d)), yday(last(d))) +
#         (wday(floor_date(d, "year")) - 1)
#         %% 7 + 6) %/% 7
#       last_day <- ceiling_date(last(d), "year") - days(1)
#       if (wday(last_day) != 7) {
#         w[d %in% seq(last_day, by = -1, length = wday(last_day))] <- 1
#       }
#       w
#     }
#   ) %>%
#     flatten_dbl()
# }

Year <- function(date) {
  map(
    unique(year(date)),
    function(x) {
      d <- date[year(date) == x]
      y <- rep(x, length(d))
      last_day <- ceiling_date(last(d), "year") - days(1)
      if (wday(last_day) != 7) {
        y[d %in% seq(last_day, by = -1, length = wday(last_day))] <- x + 1
      }
      y
    }
  ) %>%
    flatten_dbl()
}

aqi_cat <- function(x) {
  cut(x,
    breaks = c(-Inf, 50, 100, 150, 200, 300, Inf),
    labels = fct_inorder(c(
      "Good", "Moderate", "Unhealthy for Sensitive",
      "Unhealthy", "Very Unhealthy", "Hazardous"
    ))
  )
}

aqi_pal <- c(
  "Good" = "#00E400",
  "Moderate" = "#FFFF00",
  "Unhealthy for Sensitive" = "#FF7E00",
  "Unhealthy" = "#FF0000",
  "Very Unhealthy" = "#8F3F97",
  "Hazardous" = "#7E0023"
)
