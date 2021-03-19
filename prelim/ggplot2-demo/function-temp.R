get_STL_remainder <- function(data, y = NULL) {
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  data <- tsibble::fill_gaps(data) %>%
    tidyr::fill(!!y)

  r <- data %>%
    fabletools::model(
      feasts::STL(
        !!y ~ trend() + season(window = "period")
      )
    ) %>%
    fabletools::components() %>%
    update_tsibble(key = tsibble::key_vars(data)) %>%
    dplyr::select(
      !!y, remainder, dplyr::contains("season_"),
      -c(.model, dplyr::last_col())
    )

  sigma <- suppressWarnings(
    purrr::map_dbl(array_branch(r, 2), sd)
  ) %>%
    na.omit()

  r[["remainder"]] <- r[["remainder"]] +
    (purrr::map(
      sigma[-(1:2)],
      function(s) {
        if (s * 5 < sigma[1]) {
          r[, which(sigma == s)]
        } else {
          rep(0, nrow(r))
        }
      }
    ) %>%
      as_tibble() %>%
      rowSums())

  dplyr::select(r, remainder)
}


get_ARIMA_resid <- function(data, y = NULL, order = c(1, 0, 1)) {
  y <- data[[deparse(feasts:::guess_plot_var(data, !!enquo(y)))]]

  p_adf <- suppressWarnings(
    tseries::adf.test(y, alternative = "explosive")[["p.value"]]
  )

  if (p_adf <= .05) {
    order[2] <- ifelse(order[2] == 0, 1, order[2])
  }
  resid <- arima(y, order)[["residuals"]]

  data %>%
    dplyr::select(!!index(data)) %>%
    dplyr::mutate(remainder = as.numeric(resid))
}


get_remainder <- function(data, y = NULL, arima = FALSE, ...) {
  r_tsbl <- get_STL_remainder(data, !!enquo(y))

  if (arima) r_tsbl <- get_ARIMA_resid(r_tsbl, remainder, ...)

  r_tsbl
}


anom_anomalize <- function(data) {
  if (length(tsibble::key_vars(data)) > 1L) {
    data <- data %>%
      dplyr::mutate(
        .key = as.character(rlang::exec(
          interaction,
          data %>%
            as_tibble() %>%
            dplyr::select(!!!tsibble::key(data))
        ))
      ) %>%
      update_tsibble(key = .key)
  }
  if (length(tsibble::key_vars(data)) == 1L) {
    data <- data %>%
      dplyr::rename(.key = tsibble::key_vars(data))
  } else {
    data <- data %>%
      dplyr::mutate(.key = rep(0, nrow(data))) %>%
      update_tsibble(key = .key)
  }

  purrr::map(
    unique(data[[tsibble::key_vars(data)]]),
    function(key_lvls) {
      (data %>%
        dplyr::filter(.key == key_lvls) %>%
        anomalize(remainder, alpha = .02))[["anomaly"]]
    }
  ) %>% purrr::flatten_chr()
}


extract_period <- function(idx, period) {
  if (period == "day") {
    paste0(
      sprintf("%02d", lubridate::hour(idx)),
      ":00"
    )
  } else if (period == "week") {
    lubridate::wday(idx, label = TRUE)
  } else if (period == "month") {
    paste0(
      "W0",
      (lubridate::day(idx) - 1) %/% 7 + 1
    )
  } else if (period == "year") {
    lubridate::month(idx, label = TRUE)
  } else {
    stop("Invalid period")
  }
}
