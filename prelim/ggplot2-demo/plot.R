extract_period <- function(idx, period) {
  f <-
    if (period == "day") {
      function(x) {
        paste0(
          sprintf("%02d", lubridate::hour(x)),
          ":00"
        )
      }
    } else if (period == "week") {
      function(x) {
        lubridate::wday(x, label = TRUE)
      }
    } else if (period == "month") {
      function(x) {
        paste0(
          "W0",
          (lubridate::day(x) - 1) %/% 7 + 1
        )
      }
    } else if (period == "year") {
      function(x) {
        lubridate::month(x, label = TRUE)
      }
    } else {
      stop("Invalid period")
    }

  f(idx)
}


period_identifier <- function(idx, period) {
  if (period == "day") {
    period <- paste0("y", period)
  }

  eval(parse(text = paste0(
    "lubridate::", period, "(idx)"
  ))) %>% as.character()
}


gg_plots <- function(data, y, group_by, ...) {
  idx <- tsibble::index_var(data)

  mapping <- aes(
    x = !!dplyr::sym(idx),
    y = !!substitute(y),
    colour = !!substitute(group_by)
  )

  data %>%
    ggplot(mapping) +
    geom_line(...)
}


gg_botsplot <- function(data, y, period, ...) {
  idx <- tsibble::index_var(data)
  data <- data %>%
    dplyr::mutate(period_time = extract_period(!!sym(idx), period))

  mapping <- aes(
    x = period_time,
    y = !!substitute(y)
  )

  data %>%
    ggplot(mapping) +
    geom_boxplot() +
    xlab("")
}








## Decomposition Plot
# taka_at_data <- "data-raw/akl-airtemp.csv" %>%
#   clean_data("airtemp", n_loc = 5) %>%
#   dplyr::filter(
#     between(year(datetime), 2019, 2020),
#     location == "takapuna"
#   ) %>%
#   dplyr::select(-location) %>%
#   as_tsibble(index = datetime) %>%
#   fill_gaps() %>%
#   tidyr::fill(airtemp)
#
# taka_at_data %>% # What window?
#   fabletools::model(
#     feasts::STL(airtemp ~ trend(window = 365) +
#       season(window = "periodic"),
#     robust = TRUE
#     )
#   ) %>%
#   fabletools::components() %>%
#   fabletools::autoplot()
