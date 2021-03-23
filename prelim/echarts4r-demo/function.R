ts_timestamp <- function(data, custom_period = NULL) {
  if (!is_tsibble(data)) {
    rlang::abort(paste0(
      "Objects of type ",
      paste0(class(data), collapse = ", "),
      " not supported. ",
      "Consider applying as_tsibble() to data."
    ))
  }
  idx <- sym(tsibble::index_var(data))
  ts_interval <- feasts:::interval_to_period(tsibble::interval(data))

  if (is.null(custom_period)) {
    dplyr::mutate(data,
      hour = lubridate::hour(!!idx),
      wday = lubridate::wday(!!idx, label = TRUE),
      day = lubridate::day(!!idx),
      month = lubridate::month(!!idx, label = TRUE)
    )
  } else if (lubridate::is.period(custom_period)) {
    period <- custom_period / ts_interval
    dplyr::mutate(data,
      period_n = (seq_len(nrow(data)) - 1) %/% period + 1
    )
  } else if (is_integer(custom_period)) {
    period <- custom_period
    dplyr::mutate(data,
      period_n = (seq_len(nrow(data)) - 1) %/% period + 1
    )
  } else {
    rlang::abort(
      "Expecting custom_period of type NULL, Period or integer."
    )
  }
}
