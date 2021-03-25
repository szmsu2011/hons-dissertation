e_heats <- function(data, y = NULL, ...) {
  data <- tsibble::fill_gaps(data)
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  keys <- tsibble::key(data)
  ts_interval <- feasts:::interval_to_period(tsibble::interval(data))
  period <- names(
    fabletools::get_frequencies(NULL, data, .auto = "smallest")
  )
  valid_period <- c("minute", "hour", "day", "week", "month", "year")

  if (period %in% valid_period) {
    data <- ts_timestamp(data, period)
  } else {
    rlang::abort("Data with unsupported interval")
  }
  if (period <= ts_interval) {
    rlang::abort(
      "The data must contain at least one observation per period."
    )
  }
  if (length(keys) > 0) {
    data <- dplyr::mutate(data,
      .key = rlang::eval_tidy(new_quosure(
        expr(as.character(interaction(!!!keys))),
        env = env(keys = keys)
      ))
    ) %>%
      update_tsibble(key = .key) %>%
      dplyr::select(period_n, obs_n, !!y) %>%
      dplyr::group_by(.key)
  }

  data %>%
    e_charts(
      period_n,
      timeline = length(keys) > 0
    ) %>%
    e_heatmap_("obs_n", deparse(y), ...) %>%
    e_visual_map_(
      deparse(y),
      inRange = list(
        color = gsub("FF", "", viridis(11, option = "C"))
      )
    ) %>%
    e_tooltip(e_tooltip_item_formatter(),
      axisPointer = list(type = "cross")
    ) %>%
    e_title(paste(
      deparse(y), ifelse(
        length(keys) > 0,
        paste("by", paste(keys, collapse = ":")),
        ""
      )
    )) %>%
    e_axis_labels(
      y = gsub("_", "", xy_labs(period)[["ylab"]])
    ) %>%
    e_y_axis(
      inverse = TRUE,
      nameLocation = "start"
    )
}
