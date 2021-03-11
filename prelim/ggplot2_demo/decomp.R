invisible(sapply(
  paste0(
    "prelim/ggplot2_demo/",
    c("packages", "data_clean"),
    ".R"
  ),
  source
))

taka_at_data <- "data-raw/akl-airtemp.csv" %>%
  clean_data("airtemp", n_loc = 5) %>%
  dplyr::filter(
    between(year(datetime), 2019, 2020),
    location == "takapuna"
  ) %>%
  dplyr::select(-location) %>%
  as_tsibble(index = datetime) %>%
  fill_gaps() %>%
  tidyr::fill(airtemp)

taka_at_data %>% # FIXME # What window?
  fabletools::model(
    feasts::STL(airtemp ~ trend(window = 365) +
      season(window = "periodic"),
    robust = TRUE
    )
  ) %>%
  fabletools::components() %>%
  fabletools::autoplot()
