import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "prelim/ggplot2-demo/",
  c("packages", "function")
)

wind_dir_data <- "data/akl-wind-dir.csv" %>%
  read_csv(col_types = "Tcd") %>%
  mutate(datetime = datetime + hours(12))

data <- "data/akl-env-data.csv" %>%
  read_csv(col_types = "Tcdddddddddddddd") %>%
  mutate(
    datetime = datetime + hours(12),
    aqi_cat = aqi_cat(aqi)
  ) %>%
  filter(year(datetime) > 2015) %>%
  left_join(wind_dir_data, by = c("datetime", "location")) %>%
  as_tsibble(index = datetime, key = location) %>%
  bind_rows(tibble(
    datetime = ymd(20160101),
    location = c("customs_st", "papatoetoe")
  )) %>%
  fill_gaps()

## Missingness of data
g <- data %>%
  mutate(
    location = stringr::str_to_title(gsub("_", " ", location)) %>%
      fct_reorder(-get_lat(location)),
    across(-(1:2), function(x) {
      case_when(is.na(x) ~ "Missing", TRUE ~ "Data")
    })
  ) %>%
  pivot_longer(-(1:2), names_to = "Parameter", values_to = "value") %>%
  ggplot(aes(datetime, Parameter, fill = value)) +
  geom_tile() +
  facet_grid(location ~ .) +
  scale_x_datetime(expand = expansion()) +
  scale_y_discrete(expand = expansion()) +
  labs(x = "", fill = "") +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    text = element_text(size = 16),
    legend.position = "top"
  )

ggsave("img/missing.png", g, width = 13, height = 13)
