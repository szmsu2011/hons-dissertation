as_datetime <- function(x) {
  dnt <- double(length(x))
  dnt[grep("-", x)] <- x[grep("-", x)] %>%
    strptime("%Y-%m-%d %H:%M:%S") %>%
    format("%Y-%m-%d %H:%M:%S")
  dnt[grep("/", x)] <- x[grep("/", x)] %>%
    strptime("%d/%m/%Y %H:%M") %>%
    format("%Y-%m-%d %H:%M:%S")
  dnt %>%
    lubridate::as_datetime() %>%
    lubridate::as_datetime(tz = "Pacific/Auckland")
}

get_varname <- function(x, line = 3) {
  varname <- readr::read_csv(x, n_max = 3)[line, -1]
  c("datetime", gsub(".*@ (\\w)|[.]", "\\1", varname)) %>%
    janitor::make_clean_names()
}

clean_data <- function(x, value_name = "value", n_loc = 1) {
  data <- readr::read_csv(x,
    col_types = paste0(c("c", rep("d", n_loc)), collapse = ""),
    skip = 6, col_names = get_varname(x)
  )
  data$datetime <- as_datetime(data$datetime)
  data %>%
    tidyr::pivot_longer(!datetime,
      names_to = "location",
      values_to = value_name
    )
}

read_env_file <- function(x, dir) {
  readr::read_csv(paste0(dir, x),
    col_types = "cctcd"
  ) %>%
    dplyr::filter(!is.na(Time)) %>%
    dplyr::mutate(datetime = lubridate::as_datetime(
      mdy(Date) + hms(Time) - hours(12),
      tz = "Pacific/Auckland"
    )) %>%
    dplyr::select(-c(Site, Date, Time)) %>%
    dplyr::mutate(
      location = gsub("\\d_([a-z_]*).csv", "\\1", x)
    )
}

read_env_data <- function(dir) {
  env_files <- list.files(dir)
  data <- tibble()
  for (x in env_files) {
    data <- dplyr::bind_rows(data, read_env_file(x, dir))
  }
  data %>%
    dplyr::filter(!tsibble::are_duplicated(
      data,
      index = datetime, key = c(location, Parameter)
    )) %>%
    as_tsibble(index = datetime, key = c(location, Parameter)) %>%
    tidyr::pivot_wider(
      names_from = Parameter,
      values_from = Value
    ) %>%
    tsibble::fill_gaps()
}
