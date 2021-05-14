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

  x_breaks <- unique(data[[".period"]])[seq(
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
    ggplot2::scale_x_discrete(breaks = x_breaks)

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
                            q = seq(.01, 1, .01),
                            polar = FALSE, ...) {
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
    dplyr::mutate(.quantile = 100 * as.numeric(
      gsub(paste0(deparse(y), "_"), "", .quantile)
    ))

  n_period <- length(unique(data[[".period"]]))

  x_breaks <- unique(data[[".period"]])[seq(
    1,
    n_period,
    by = max(1, (n_key * n_period) %/% ifelse(polar, 40, 20))
  )]

  mapping <- aes(
    x = .period,
    y = .value,
    group = .quantile,
    col = .quantile
  )

  p <- ggplot(data, mapping) +
    geom_point() +
    ggplot2::labs(x = "", y = deparse(y), col = "quantile") +
    ggplot2::scale_x_discrete(breaks = x_breaks) +
    colorspace::scale_colour_continuous_diverging(
      mid = 50,
      c1 = 120
    )

  if (!polar) {
    p <- p +
      geom_line(...)
  } else {
    p <- p +
      geom_polygon(fill = NA, ...) +
      ggplot2::coord_polar()
  }

  if (n_key > 1) {
    p <- p +
      ggplot2::facet_grid(
        cols = vars(!!!purrr::map(
          keys,
          function(x) expr(format(!!x))
        )),
        scale = ifelse(polar, "fixed", "free_x")
      )
  }

  p
}


cat_heats <- function(data, y, pal, ...) {
  data <- tsibble::fill_gaps(data)
  y <- substitute(y)
  idx <- tsibble::index_var(data)
  n_key <- tsibble::n_keys(data)
  keys <- tsibble::key(data)
  ts_interval <- feasts:::interval_to_period(tsibble::interval(data))
  period <- names(
    fabletools::get_frequencies(NULL, data, .auto = "smallest")
  )
  valid_period <- c("minute", "hour", "day", "week", "month", "year")
  valid_keys <- keys[
    purrr::map(keys, function(x) length(unique(data[[x]]))) > 1
  ]

  if (length(keys) != length(valid_keys)) {
    data <- data %>%
      update_tsibble(key = as.character(valid_keys))
    keys <- valid_keys
  }

  if (length(keys) > 0) {
    data <- data %>%
      dplyr::mutate(
        .key = rlang::eval_tidy(new_quosure(
          expr(as.character(interaction(!!!keys))),
          env = env(keys = keys)
        ))
      ) %>%
      update_tsibble(key = .key)
  }

  ini_row <- (if (length(keys) > 0) {
    dplyr::filter(data, .key == first(data[[".key"]]))[1, ]
  } else {
    data[1, ]
  }) %>% dplyr::select_if(names(.) %in% c(deparse(y), ".key"))
  ini_row[[deparse(y)]] <- NA

  if (!ini_row[[idx]] == floor_date(ini_row[[idx]], period)) {
    ini_row[[idx]] <- floor_date(ini_row[[idx]], period)
    data <- dplyr::bind_rows(data, ini_row) %>%
      tsibble::fill_gaps()
  }

  if (period %in% valid_period) {
    data <- ts_timestamp(data, period) %>%
      dplyr::mutate(.obs_n = fct_rev(fct_inorder(.obs_n)))
  } else {
    rlang::abort("Data with unsupported interval")
  }
  if (period <= ts_interval) {
    rlang::abort(
      "The data must contain at least one observation per period."
    )
  }

  mapping <- aes(
    x = .period_n,
    y = .obs_n,
    col = !!y
  )

  n_period <- length(unique(data[[".period_n"]]))
  n_obs_p <- nlevels(data[[".obs_n"]])

  x_breaks <- unique(data[[".period_n"]])[seq(
    1,
    n_period,
    by = max(1, n_period %/% 5)
  )]
  y_breaks <- unique(data[[".obs_n"]])[seq(
    1,
    n_obs_p,
    by = max(1, (n_key * n_obs_p) %/% 20)
  )]

  p <- data %>%
    ggplot(mapping) +
    geom_tile(aes(fill = after_scale(col)), ...) +
    ggplot2::scale_colour_manual(
      values = pal,
      drop = FALSE
    ) +
    ggplot2::scale_y_discrete(breaks = y_breaks) +
    ggplot2::scale_x_discrete(breaks = x_breaks) +
    ggplot2::labs(
      x = "",
      y = xy_labs(period)[["ylab"]]
    )

  if (n_key > 1) {
    p <- p +
      ggplot2::facet_grid(
        rows = vars(.key)
      )
  }

  p
}
