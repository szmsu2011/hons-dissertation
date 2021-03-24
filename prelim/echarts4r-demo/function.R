floor_hour <- function(x) lubridate::floor_date(x, "hour")
floor_min <- function(x) lubridate::floor_date(x, "minute")
wday_ <- function(x) lubridate::wday(x, label = TRUE)
month_ <- function(x) lubridate::month(x, label = TRUE)


xy_labs <- function(period) {
  i <- period == c("minute", "hour", "day", "week", "month", "year")

  c(
    xlab = c(
      "floor_min", "floor_hour", "as_date", "yearweek", "yearmonth", "year"
    )[i],
    ylab = c(
      "second", "minute", "hour", "wday_", "day", "month_"
    )[i]
  )
}


ts_timestamp <- function(data, period = NULL) {
  if (is.numeric(period)) {
    ts_interval <- feasts:::interval_to_period(tsibble::interval(data))
    period <- period /
      if (lubridate::is.period(period)) ts_interval else 1
    mapping <- list(
      period_n = ((seq_len(nrow(data)) - 1) %/% period + 1) %>%
        as.character(),
      obs_n = rep_len(seq_len(period), nrow(data)) %>%
        as.character()
    )
  } else {
    idx <- tsibble::index_var(data)
    mapping <- list(
      period_n = data[[idx]] %>%
        (eval(sym(xy_labs(period)[["xlab"]])))() %>%
        as.character(),
      obs_n = data[[idx]] %>%
        (eval(sym(xy_labs(period)[["ylab"]])))() %>%
        as.character()
    )
  }
  dplyr::mutate(data, !!!mapping)
}
