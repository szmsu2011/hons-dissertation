gg_plots <- function(data, y = NULL, ...,
                     check_anon = c("none", "anomalize")) {
  data <- tsibble::fill_gaps(data)
  y <- guess_plot_var(data, !!enquo(y))
  check_anon <- match.arg(check_anon)
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- tsibble::key(data)

  if (check_anon == "anomalize") {
    data <- data %>% dplyr::mutate(
      anomaly = anon_anomalize(get_remainder(data, !!y))
    )
  }

  mapping <- aes(
    x = !!dplyr::sym(idx),
    y = !!enquo(y),
    colour = (
      if (n_key > 1) interaction(!!!keys) else NULL
    )
  )

  p <- ggplot(data, mapping) +
    geom_line(...)

  if (n_key > 1) {
    p <- p +
      ggplot2::scale_colour_discrete(
        name = paste0(keys, collapse = ".")
      )
  }
  if (check_anon != "none") {
    p <- p +
      geom_point(
        data = dplyr::filter(data, anomaly == "Yes", !is.na(!!y)),
        aes(colour = NULL), colour = "red"
      )
  }

  p
}


gg_botsplot <- function(data, y = NULL, period = NULL, ...) {
  data <- tsibble::fill_gaps(data)
  y <- guess_plot_var(data, !!enquo(y))
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- tsibble::key(data)
  ts_interval <- interval_to_period(interval(data))

  if (is.null(period)) {
    period <- names(
      fabletools::get_frequencies(period, data, .auto = "largest")
    )
  }
  if (period <= ts_interval) {
    rlang::abort(
      "The data must contain at least one observation per seasonal period."
    )
  }

  data <- data %>%
    dplyr::mutate(period_chr = extract_period(!!sym(idx), period))

  n_period <- length(unique(data[["period_chr"]]))

  guide_breaks <- unique(data[["period_chr"]])[seq(
    1, n_period,
    by = max(1, (n_key * n_period) %/% 20)
  )]

  mapping <- aes(
    x = period_chr,
    y = !!enquo(y)
  )

  p <- ggplot(data, mapping) +
    geom_boxplot(...) +
    ggplot2::xlab("") +
    ggplot2::scale_x_discrete(breaks = guide_breaks)

  if (n_key > 1) {
    p <- p +
      ggplot2::facet_grid(
        cols = vars(!!!purrr::map(
          keys,
          function(x) expr(format(!!x))
        )),
        scale = "free_x"
      )
  }

  p
}




#################### Experimental, Incomplete ####################

gg_f <- function(data, y, period = NULL, facet_period = NULL, max_col = 15,
                 labels = c("none", "left", "right", "both"), ...) {
  data <- tsibble::fill_gaps(data)

  #################### feasts::gg_season ####################

  labels <- match.arg(labels)
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- key(data)
  ts_interval <- interval_to_period(tsibble::interval(data))

  if (is.null(period)) {
    period <- names(fabletools::get_frequencies(period, data, .auto = "largest"))
  }
  if (is.numeric(period)) {
    period <- period * ts_interval
  }
  if (period <= ts_interval) {
    rlang::abort(
      "The data must contain at least one observation per seasonal period."
    )
  }

  if (!is.null(facet_period)) {
    if (is.numeric(facet_period)) {
      facet_period <- facet_period * ts_interval
    }
    facet_period <- lubridate::as.period(facet_period)

    if (facet_period <= ts_interval) {
      rlang::abort(
        "The data must contain at least one observation per seasonal period."
      )
    }
  }

  data <- as_tibble(data)
  data[c("facet_id", "id")] <- time_identifier(
    data[[idx]], period,
    within = facet_period,
    interval = ts_interval
  )
  data[idx] <- time_offset_origin(data[[idx]], period)

  num_ids <- length(unique(data[["id"]]))

  #################### feasts::gg_season ####################

  ## Failed Code
  # data <- data %>%
  #   dplyr::mutate(
  #     period_chr = extract_period(!!sym(idx), period),
  #     period_id = period_identifier(!!sym(idx), period),
  #     period_num = period_as_num(
  #       !!sym(idx), period_chr, period, ts_interval
  #     )
  #   )
  #
  # mapping <- aes(
  #   x = period_chr,
  #   y = !!enquo(y)
  # )
  #
  # data %>%
  #   ggplot(mapping) +
  #   geom_line(aes(
  #     x = period_num,
  #     y = !!enquo(y),
  #     group = period_id
  #   )) +
  #   geom_boxplot() +
  #   xlab("")
}
