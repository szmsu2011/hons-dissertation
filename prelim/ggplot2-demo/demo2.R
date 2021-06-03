import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "prelim/ggplot2-demo/",
  c("packages", "data-clean", "covid19-lvl", "function", "plot")
)

dir <- "data-raw/envir-data/"
env_files <- list.files(dir)
env_data <- tibble()

for (x in env_files) {
  env_data <- dplyr::bind_rows(env_data, as_tibble(read_env_data(x, dir)))
}

env_data <- as_tsibble(env_data, index = datetime, key = location) #FIXME
