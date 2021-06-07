import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "prelim/ggplot2-demo/",
  c("packages", "data-clean", "covid19-lvl", "function", "plot")
)

# env_data <- read_env_data("data-raw/envir-data/")

# readr::write_csv(env_data, "data/akl-env-data.csv")

env_data <- "data/akl-env-data.csv" %>%
  readr::read_csv(
    col_types = paste0("Tc", paste(rep("d", 14), collapse = "")),
    locale = locale(tz = "Pacific/Auckland")
  ) %>%
  as_tsibble(index = datetime, key = location) %>%
  dplyr::mutate(
    akl_level = get_covid19level(datetime, "AKL"),
    nz_level = get_covid19level(datetime, "NZ_not_AKL"),
    covid19_period = get_covid19period(nz_level, datetime)
  )
