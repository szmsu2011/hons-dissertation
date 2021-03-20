gg_plots <- function(data, y = NULL, ...,
                     check_anom = c("none", "SHESD")) {
  data <- tsibble::fill_gaps(data)
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  check_anom <- match.arg(check_anom)
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- tsibble::key(data)

  if (check_anom == "SHESD") {
    data <- data %>% dplyr::mutate(
      anomaly = anom_SHESD(get_remainder(data, !!y))
    )
  }

  mapping <- aes(
    x = !!dplyr::sym(idx),
    y = !!enquo(y)
  )

  p <- ggplot(data, mapping) +
    geom_line(colour = "steelblue", ...)

  if (check_anom != "none") {
    p <- p +
      geom_point(
        data = dplyr::filter(data, anomaly == "Yes", !is.na(!!y)),
        aes(colour = NULL), colour = "red"
      )
  }
  if (n_key > 1) {
    p <- p +
      ggplot2::facet_grid(
        rows = vars(!!!purrr::map(
          keys,
          function(x) expr(format(!!x))
        ))
      )
  }

  p
}


gg_botsplot <- function(data, y = NULL, period = NULL, ...) {
  data <- tsibble::fill_gaps(data)
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- tsibble::key(data)
  ts_interval <- feasts:::interval_to_period(tsibble::interval(data))

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
