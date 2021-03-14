extract_period <- function(idx, period) {
  if (period == "day") {
    paste0(
      sprintf("%02d", lubridate::hour(idx)),
      ":00"
    )
  } else if (period == "week") {
    lubridate::wday(idx, label = TRUE)
  } else if (period == "month") {
    paste0(
      "W0",
      (lubridate::day(idx) - 1) %/% 7 + 1
    )
  } else if (period == "year") {
    lubridate::month(idx, label = TRUE)
  } else {
    stop("Invalid period")
  }
}




#################### Failed Functions ####################

# one_less <- function(period) {
#   case_when(
#     period == "day" ~ "hour",
#     period == "week" ~ "day",
#     period == "month" ~ "week"
#   )
# }
#
#
# period_seq <- function(start, period_step,
#                        break_idx, end_idx) {
#   n <- diff(break_idx)
#
#   c(
#     seq(start, by = period_step, length = n[1]),
#     purrr::as_vector(map(
#       n[-1],
#       function(x) {
#         seq(1, by = period_step, length = x)
#       }
#     )),
#     seq(1, by = period_step, length = end_idx - sum(n))
#   )
# }
#
#
# period_as_num <- function(idx, period_chr,
#                           period, ts_interval) {
#   period_discrete <-
#     if (is.na(suppressWarnings(as.numeric(period_chr[1])))) {
#       as.numeric(gsub(":00|W0", "", period_chr))
#     } else {
#       as.numeric(period_chr)
#     }
#
#   if (period == "day") {
#     return(period_discrete - min(period_discrete) + 1)
#   }
#
#   break_idx <- c(1, which(diff(period_discrete) < 0) + 1)
#   period_step <-
#     if (period == "month") {
#       days(days_in_month(1:12))
#     } else {
#       ts_interval /
#         eval(parse(text = paste0(
#           "lubridate::", one_less(period), "s(1)"
#         )))
#     }
#
#   period_seq(
#     period_discrete[1], period_step,
#     break_idx, length(period_discrete)
#   ) %>%
#     (function(x) x - min(x) + 1)()
# }
#
#
# period_identifier <- function(idx, period) {
#   if (period == "day") {
#     period <- paste0("y", period)
#   }
#
#   eval(parse(text = paste0(
#     "lubridate::", period, "(idx)"
#   ))) %>% as.character()
# }
