as_datetime <- function(x) {
  dnt <- double(length(x))
  dnt[grep("-", x)] <- x[grep("-", x)] %>%
    strptime("%Y-%m-%d %H:%M:%S") %>%
    format("%Y-%m-%d %H:%M:%S")
  dnt[grep("/", x)] <- x[grep("/", x)] %>%
    strptime("%d/%m/%Y %H:%M") %>%
    format("%Y-%m-%d %H:%M:%S")
  dnt %>% as.POSIXct()
}

get_varname <- function(x, line = 3) {
  varname <- readr::read_csv(x)[line, -1]
  c("datetime", gsub(".*@ (\\w)|[.]", "\\1", varname)) %>%
    janitor::make_clean_names()
}

clean_data <- function(x, value_name = "value") {
  data <- readr::read_csv(x,
    skip = 6, col_types = "cdddd",
    col_names = get_varname(x)
  )
  data$datetime <- as_datetime(data$datetime)
  data %>%
    tidyr::pivot_longer(!datetime,
      names_to = "location",
      values_to = value_name
    )
}
