floor_hour <- function(x) lubridate::floor_date(x, "hour")
floor_min <- function(x) lubridate::floor_date(x, "minute")
wday_ <- function(x) lubridate::wday(x, label = TRUE)
month_ <- function(x) lubridate::month(x, label = TRUE)


xy_labs <- function(period) {
  i <- period == c("minute", "hour", "day", "week", "month", "year")
  c(
    xlab = c(
      "floor_min", "floor_hour", "as_date", "yearweek", "yearmonth", "year"
    )[i],
    ylab = c(
      "second", "minute", "hour", "wday_", "day", "month_"
    )[i]
  )
}


ts_timestamp <- function(data, period) {
  if (is.numeric(period)) {
    keys <- tsibble::key(data)
    ts_interval <- feasts:::interval_to_period(tsibble::interval(data))
    period <- period /
      if (lubridate::is.period(period)) ts_interval else 1

    if (length(keys) > 0) {
      data <- dplyr::mutate(data,
        .key = rlang::eval_tidy(new_quosure(
          expr(as.character(interaction(!!!keys))),
          env = env(keys = keys)
        ))
      )
    } else {
      data <- dplyr::mutate(.key = "")
    }
    mapping <- list(
      .period_n = purrr::map(
        unique(data[[".key"]]),
        function(key_lvls) {
          ((seq_len(sum(data[[".key"]] == key_lvls)) - 1) %/%
            period + 1) %>% as.integer()
        }
      ) %>% purrr::flatten_chr(),
      .obs_n = purrr::map(
        unique(data[[".key"]]),
        function(key_lvls) {
          rep_len(seq_len(period), sum(data[[".key"]] == key_lvls))
        }
      ) %>% purrr::flatten_chr()
    )
    data <- dplyr::select(data, -.key)
  } else {
    idx <- tsibble::index_var(data)
    mapping <- list(
      .period_n = data[[idx]] %>%
        (eval(sym(xy_labs(period)[["xlab"]])))() %>%
        as.character(),
      .obs_n = data[[idx]] %>%
        (eval(sym(xy_labs(period)[["ylab"]])))() %>%
        as.character()
    )
  }
  dplyr::mutate(data, !!!mapping)
}


total <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) {
    NA
  } else {
    sum(x, na.rm = na.rm)
  }
}


tooltip_content <- function(data, y, idx, keys) {
  tt_data <- data %>%
    as.data.frame() %>%
    dplyr::relocate(!!idx, !!!keys, !!y)

  name <- names(tt_data)

  label <- purrr::map(
    purrr::array_branch(tt_data, 1),
    function(x) {
      paste0(
        c(rbind(
          paste0("\n<br />", name, ": "),
          as.character(x)
        )),
        collapse = ""
      )
    }
  ) %>% purrr::flatten_chr()

  gsub("^\n<br />", "", label)
}
