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
  } else if (period == "years") {
    as.character(lubridate::year(idx))
  }
}


quantile_f <- function(p) {
  purrr::map(
    p,
    function(p) {
      eval(parse(
        text = paste0("function(x) quantile(x, probs = ", p, ", na.rm = TRUE)")
      ))
    }
  ) %>% purrr::set_names(p)
}


get_seasquantile <- function(data, y, idx, keys, period, q) {
  suppressMessages(data %>%
    dplyr::mutate(.period = extract_period(!!sym(idx), period)) %>%
    as_tibble() %>%
    dplyr::group_by(!!!keys, .period) %>%
    dplyr::summarise(across(y, quantile_f(q))) %>%
    dplyr::ungroup())
}


is_isolated <- function(x) {
  !is.na(x) & is.na(head(c(1, x), -1)) &
    is.na(tail(c(x, 1), -1))
}


aqi_cat <- function(x) {
  cut(x,
    breaks = c(0, 50, 100, 150, 200, 300, Inf),
    labels = fct_inorder(c(
      "Good", "Moderate", "Unhealthy for Sensitive",
      "Unhealthy", "Very Unhealthy", "Hazardous"
    ))
  )
}


aqi_pal <- c(
  "Good" = "#00E400",
  "Moderate" = "#FFFF00",
  "Unhealthy for Sensitive" = "#FF7E00",
  "Unhealthy" = "#FF0000",
  "Very Unhealthy" = "#8F3F97",
  "Hazardous" = "#7E0023"
)


covid_alert_pal <- c(
  "Level 0" = "white",
  "Level 1" = "lightyellow",
  "Level 2" = "yellow",
  "Level 3" = "orange",
  "Level 4" = "darkorange"
)


akl_city_grid <- tibble(
  name = c(
    "glen_eden", "henderson", "penrose", "queen_street",
    "takapuna", "pakuranga", "patumahoe"
  ),
  code = c("GL", "HD", "PR", "QS", "TK", "PK", "PT"),
  row = c(3, 2, 3, 2, 1, 3, 5),
  col = c(1, 1, 2, 2, 2, 3, 2)
)
