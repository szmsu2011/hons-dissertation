e_heats <- function(data, y = NULL,
                    aggregate = c("none", "mean", "median", "total"),
                    ...) {
  data <- tsibble::fill_gaps(data)
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  aggregate <- match.arg(aggregate)
  idx <- tsibble::index_var(data)
  keys <- tsibble::key(data)
  ts_interval <- feasts:::interval_to_period(tsibble::interval(data))
  period <- names(
    fabletools::get_frequencies(NULL, data, .auto = "smallest")
  )
  valid_period <- c("minute", "hour", "day", "week", "month", "year")

  if (length(keys) > 0) {
    data <- data %>%
      dplyr::mutate(
        .key = rlang::eval_tidy(new_quosure(
          expr(as.character(interaction(!!!keys))),
          env = env(keys = keys)
        ))
      ) %>%
      update_tsibble(key = .key)

    if (aggregate != "none") {
      data <- data %>%
        as_tibble() %>%
        dplyr::group_by(!!sym(idx)) %>%
        dplyr::summarise(
          !!deparse(y) := (eval(sym(aggregate)))(!!y, na.rm = TRUE)
        ) %>%
        dplyr::mutate(.key = paste0("_", aggregate, "_")) %>%
        dplyr::bind_rows(data) %>%
        as_tsibble(index = idx, key = .key)
    }
  }

  ini_row <- (if (length(keys) > 0) {
    dplyr::filter(data, .key == last(data[[".key"]]))[1, ]
  } else {
    data[1, ]
  }) %>% dplyr::select(!!y)
  ini_row[[deparse(y)]] <- NA

  if (!ini_row[[idx]] == floor_date(ini_row[[idx]], period)) {
    ini_row[[idx]] <- floor_date(ini_row[[idx]], period)
    data <- dplyr::bind_rows(data, ini_row) %>%
      tsibble::fill_gaps()
  }

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
  if (length(keys) > 0) data <- dplyr::group_by(data, .key)

  data %>%
    e_charts(.period_n,
      timeline = length(keys) > 0
    ) %>%
    e_heatmap_(".obs_n", deparse(y), ...) %>%
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
    ) %>%
    e_datazoom(x_index = 0) %>%
    e_datazoom(y_index = 0) %>%
    e_timeline_opts(
      right = 50,
      left = 200,
      top = 5
    )
}
