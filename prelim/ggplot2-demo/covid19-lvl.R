get_covid19level <- function(date, scope = c("AKL", "NZ_not_AKL")) {
  scope <- match.arg(scope)
  date <- lubridate::as_date(date)

  lvl <- ordered(
    dplyr::case_when(
      date < dmy("21 March 2020") ~ "Level_0",
      between(date, dmy("21 March 2020"), dmy("22 March 2020")) ~ "Level_2",
      between(date, dmy("23 March 2020"), dmy("24 March 2020")) ~ "Level_3",
      between(date, dmy("25 March 2020"), dmy("26 April 2020")) ~ "Level_4",
      between(date, dmy("27 April 2020"), dmy("12 May 2020")) ~ "Level_3",
      between(date, dmy("13 May 2020"), dmy("7 June 2020")) ~ "Level_2",
      between(date, dmy("8 June 2020"), dmy("11 August 2020")) ~ "Level_1",
      between(date, dmy("12 August 2020"), dmy("29 August 2020")) ~ "Level_3", # NZ @ Level_2
      between(date, dmy("30 August 2020"), dmy("20 September 2020")) ~ "Level_2",
      between(date, dmy("21 September 2020"), dmy("6 October 2020")) ~ "Level_2", # NZ @ Level_1
      between(date, dmy("7 October 2020"), dmy("13 February 2021")) ~ "Level_1",
      between(date, dmy("14 February 2021"), dmy("16 February 2021")) ~ "Level_3", # NZ @ Level_2
      between(date, dmy("17 February 2021"), dmy("21 February 2021")) ~ "Level_2", # NZ @ Level_1
      between(date, dmy("22 February 2021"), dmy("27 February 2021")) ~ "Level_1",
      between(date, dmy("28 February 2021"), dmy("6 March 2021")) ~ "Level_3", # NZ @ Level_2
      between(date, dmy("7 March 2021"), dmy("12 March 2021")) ~ "Level_2", # NZ @ Level_1
      TRUE ~ "Level_1"
    ),
    levels = paste("Level", 0:4, sep = "_")
  )

  if (scope == "NZ_not_AKL") {
    lvl[between(date, dmy("12 August 2020"), dmy("29 August 2020"))] <- "Level_2"
    lvl[between(date, dmy("21 September 2020"), dmy("6 October 2020"))] <- "Level_1"
    lvl[between(date, dmy("14 February 2021"), dmy("16 February 2021"))] <- "Level_2"
    lvl[between(date, dmy("17 February 2021"), dmy("21 February 2021"))] <- "Level_1"
    lvl[between(date, dmy("28 February 2021"), dmy("6 March 2021"))] <- "Level_2"
    lvl[between(date, dmy("7 March 2021"), dmy("12 March 2021"))] <- "Level_1"
  }

  lvl
}


get_covid19period <- function(lvl, date) {
  dplyr::case_when(
    lubridate::year(date) < 2020 ~ "Before 2020",
    TRUE ~ case_when(
      lvl %in% paste0("Level_", 0:3) ~ "Level_0~3",
      TRUE ~ "Level_4"
    )
  ) %>%
    ordered(c("Before 2020", "Level_0~3", "Level_4"))
}
