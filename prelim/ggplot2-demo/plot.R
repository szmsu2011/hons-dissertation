gg_plots <- function(data, y = NULL, ...,
                     check_anom = c("none", "SHESD")) {
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  check_anom <- match.arg(check_anom)
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- tsibble::key(data)
  data <- tsibble::fill_gaps(data) %>%
    dplyr::mutate(.iso = is_isolated(!!y))

  if (check_anom == "SHESD") {
    data <- data %>% dplyr::mutate(
      .anomaly = anom_SHESD(get_remainder(data, !!y))
    )
  }

  mapping <- aes(
    x = !!dplyr::sym(idx),
    y = !!enquo(y)
  )

  p <- ggplot(data, mapping) +
    geom_line(col = "steelblue", ...)

  if (check_anom != "none") {
    p <- p +
      geom_point(
        data = dplyr::filter(data, .anomaly == "Yes", !is.na(!!y)),
        aes(col = NULL), col = "red"
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
  if (sum(data[[".iso"]]) > 0) {
    p <- p +
      geom_point(
        data = dplyr::filter(data, .iso),
        col = "purple"
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
    dplyr::mutate(.period = extract_period(!!sym(idx), period))

  n_period <- length(unique(data[[".period"]]))

  guide_breaks <- unique(data[[".period"]])[seq(
    1,
    n_period,
    by = max(1, (n_key * n_period) %/% 20)
  )]

  mapping <- aes(
    x = .period,
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


gg_seasquantile <- function(data, y = NULL, period = NULL,
                            q = seq(.1, 1, .01), ...) {
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
    get_seasquantile(y, idx, keys, period, q) %>%
    tidyr::pivot_longer(
      dplyr::starts_with(paste0(deparse(y), "_")),
      names_to = ".quantile",
      values_to = ".value"
    ) %>%
    dplyr::mutate(.quantile = ordered(
      sprintf("%1.0f%%", 100 * as.numeric(
        gsub(paste0(deparse(y), "_"), "", .quantile)
      )),
      levels = sprintf("%1.0f%%", 100 * rev(q))
    ))

  n_period <- length(unique(data[[".period"]]))

  guide_breaks <- unique(data[[".period"]])[seq(
    1,
    n_period,
    by = max(1, (n_key * n_period) %/% 20)
  )]

  mapping <- aes(
    x = .period,
    y = .value,
    group = .quantile,
    col = .quantile
  )

  p <- ggplot(data, mapping) +
    geom_point() +
    geom_line(...) +
    ggplot2::labs(x = "", y = deparse(y), col = "quantile") +
    ggplot2::scale_x_discrete(breaks = guide_breaks) +
    ggplot2::scale_colour_manual(
      values = rev(colorspace::diverge_hcl(length(q), c = 110))
    )

  if (length(q) > 10) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }

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
