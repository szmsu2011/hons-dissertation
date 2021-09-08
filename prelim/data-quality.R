import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "prelim/ggplot2-demo/",
  c("packages", "data-clean")
)

## Environmental data (raw)
dir <- "data-raw/envir-data/"
env_files <- list.files(dir)
n <- 0
for (x in env_files) {
  n <- n + paste0(dir, x) %>%
    read_csv(col_types = "cctcd") %>%
    nrow()
}
n

## Missing time-stamp (but with date)
n <- 0
for (x in env_files) {
  n <- n + paste0(dir, x) %>%
    read_csv(col_types = "cctcd") %>%
    filter(is.na(Time)) %>%
    nrow()
}
n

## Missing values (with time-stamp)
n <- 0
for (x in env_files) {
  n <- n + paste0(dir, x) %>%
    read_csv(col_types = "cctcd") %>%
    filter(is.na(Value)) %>%
    nrow()
}
n

## Negative values
n <- 0
for (x in env_files) {
  n <- n + paste0(dir, x) %>%
    read_csv(col_types = "cctcd") %>%
    filter(Value < 0) %>%
    nrow()
}
n

data <- tibble()
for (x in env_files) {
  data <- bind_rows(data, read_env_file(x, dir))
}

## Duplicate values
(dup <- data %>%
  duplicates(index = datetime, key = c(location, Parameter)) %>%
  arrange(location, Parameter, datetime)) %>%
  nrow()

## Redundant records among duplicate values
data %>%
  are_duplicated(index = datetime, key = c(location, Parameter)) %>%
  sum()

## Inconsistent duplicate values
# first_dup <- dup %>%
#   filter(!are_duplicated(dup, index = datetime, key = c(location, Parameter))) %>%
#   as.data.frame()
# bad_dup <- tibble()
# for (i in seq_len(nrow(first_dup))) {
#   match_record <- dup %>%
#     filter(
#       Parameter == first_dup[i, 1],
#       datetime == first_dup[i, 3],
#       location == first_dup[i, 4]
#     )
#   is_consistent <- with(match_record, diff(range(Value)) == 0)
#   if (!is_consistent) {
#     bad_dup <- bind_rows(bad_dup, match_record)
#   }
# }
# bad_dup <- bad_dup %>%
#   arrange(location, Parameter, datetime) %>%
#   select(location, datetime, Parameter, Value)
# write_csv(bad_dup, "data-raw/envir-data-bad-dup.csv")
## Very slow
bad_dup <- read_csv("data-raw/envir-data-bad-dup.csv")
nrow(bad_dup)

data <- data %>%
  filter(!are_duplicated(data, index = datetime, key = c(location, Parameter))) %>%
  as_tsibble(index = datetime, key = c(location, Parameter))

## Implicit time gaps (in records)
nrow(fill_gaps(data)) - nrow(data)

data <- pivot_wider(data, names_from = Parameter, values_from = Value)

## Implicit time gaps (in observations)
nrow(fill_gaps(data)) - nrow(data)

## Wind data (raw)
data <- read_csv(x,
  col_types = paste0("cc", paste0(rep("d", 14), collapse = "")),
  col_names = get_wind_dir_varname(x)
)[-1, ]

## Inconsistent time format (with "hh:mm:ss" format)
with(data, length(grep(":00:00$", time))) * 7 ## The rest with "hh:mm" format

data <- data %>%
  mutate(
    time_cleaned = gsub(":00:00", ":00", time),
    datetime = lubridate::as_datetime(
      dmy(date) + hm(time_cleaned) - hours(12),
      tz = "Pacific/Auckland"
    )
  ) %>%
  select(-c(1:2, 17)) %>%
  pivot_longer(!datetime,
    names_to = "location",
    values_to = "wind_dir"
  ) %>%
  filter(location %in% c(
    "glen_eden", "henderson", "penrose", "queen_street",
    "takapuna", "pakuranga", "khyber_pass"
  )) %>%
  as_tsibble(index = datetime, key = location)

nrow(data)

## Missing values
sum(is.na(data))

## No implicit time gaps
nrow(fill_gaps(data)) - nrow(data)

source("../akl-air-quality/shiny/function.R")

wind_dir_data <- "data/akl-wind-dir.csv" %>%
  read_csv(col_types = "Tcd") %>%
  mutate(datetime = datetime + hours(12))

data <- "data/akl-env-data.csv" %>%
  read_csv(col_types = "Tcdddddddddddddd") %>%
  mutate(
    datetime = datetime + hours(12),
    aqi_cat = aqi_cat(aqi)
  ) %>%
  left_join(wind_dir_data, by = c("datetime", "location")) %>%
  as_tsibble(index = datetime, key = location)

## Cleaned data
nrow(data)
length(print(names(data)))
print(sum(is.na(select(data, -aqi_cat)))) / nrow(data) / 15

## All data in NZST
with(data, range(datetime))
with(filter(data, location == "customs_st"), range(datetime))
