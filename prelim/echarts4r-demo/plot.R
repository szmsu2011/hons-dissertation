e_heats <- function(data, y = NULL, period = NULL, ...) {
  data <- tsibble::fill_gaps(data)
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  idx <- sym(tsibble::index_var(data))
  keys <- tsibble::key(data)
  ts_interval <- feasts:::interval_to_period(tsibble::interval(data))
  standard_period <- c(
    "minute", "hour", "day", "week", "month", "year"
  )

  if (is.null(period)) {
    period <- names(
      fabletools::get_frequencies(period, data, .auto = "smallest")
    )
  }
  if (period %in% standard_period) {
    data <- ts_timestamp(data, period)
  } else if (is.numeric(period) & period >= 2) {
    if (!lubridate::is.period(period)) {
      period <- as.integer(round(period))
    }
    data <- ts_timestamp(data, period)
  } else {
    rlang::abort("Invalid period")
  }
  if (!is.integer(period) & period <= ts_interval) {
    rlang::abort(
      "The data must contain at least one observation per period."
    )
  }

  data %>%
    dplyr::group_by(!!!keys) %>%
    e_charts(period_n, timeline = TRUE) %>%
    e_heatmap_("obs_n", deparse(y), ...) %>%
    e_visual_map_(
      deparse(y),
      inRange = list(
        color = gsub("FF", "", viridis(3, option = "C"))
      )
    ) %>%
    e_tooltip(e_tooltip_item_formatter(),
      axisPointer = list(type = "cross")
    ) %>%
    e_title(paste(deparse(y), "by", keys)) %>%
    e_axis_labels(
      y = gsub("_", "", xy_labs(period)[["ylab"]])
    ) %>%
    e_y_axis(
      inverse = TRUE,
      nameLocation = "start"
    )
}
