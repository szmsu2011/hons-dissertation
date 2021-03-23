get_remainder <- function(data, y = NULL) {
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  data <- tsibble::fill_gaps(data) %>%
    tidyr::fill(!!y)

  data %>%
    fabletools::model(
      feasts::STL(
        !!y ~ trend() + season(window = "period"),
        robust = TRUE
      )
    ) %>%
    fabletools::components() %>%
    update_tsibble(key = tsibble::key_vars(data)) %>%
    dplyr::select(remainder, -.model)
}


anom_SHESD <- function(data) {
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
        anomalize(remainder, alpha = .011))[["anomaly"]]
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
    rlang::abort("Invalid period")
  }
}
