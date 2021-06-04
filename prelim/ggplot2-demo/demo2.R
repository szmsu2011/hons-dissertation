import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "prelim/ggplot2-demo/",
  c("packages", "data-clean", "covid19-lvl", "function", "plot")
)

# env_data <- read_env_data("data-raw/envir-data/")

# readr::write_csv(env_data, "data/akl-env-data.csv")

env_data <- readr::read_csv(
  "data/akl-env-data.csv",
  col_types = paste0("Tc", paste(rep("d", 16), collapse = ""))
) %>%
  as_tsibble(index = datetime, key = location)
