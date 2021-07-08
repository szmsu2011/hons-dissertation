Max <- function(x) {
  x <- max(x, na.rm = TRUE)
  ifelse(x > -Inf, round(x), NA)
}

Mean <- function(x) round(mean(x, na.rm = TRUE))

Median <- function(x) round(median(x, na.rm = TRUE))

fmt_date <- stamp("March 1, 1999")

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
