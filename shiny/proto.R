library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(ggTimeSeries)

import <- function(dir, file) {
  invisible(purrr::map(c(paste0(dir, file, ".R")), source))
}
import(
  "../prelim/ggplot2-demo/",
  c("packages", "data-clean", "covid19-lvl", "function", "plot")
)
import(
  "../prelim/echarts4r-demo/",
  c("packages", "function", "plot")
)

Max <- function(x) {
  x <- max(x, na.rm = TRUE)
  ifelse(x > -Inf, round(x), NA)
}
Mean <- function(x) round(mean(x, na.rm = TRUE))
Median <- function(x) round(median(x, na.rm = TRUE))
fmt_date <- stamp("March 1, 1999")

env_data <- read_csv("../data/akl-env-data.csv",
  col_types = paste0("Tc", paste(rep("d", 14), collapse = "")),
  locale = locale(tz = "Pacific/Auckland")
) %>%
  filter(year(datetime) > 2015) %>%
  mutate(
    aqi_cat = aqi_cat(aqi),
    date = as_date(datetime),
    hour = hour(datetime)
  ) %>%
  as_tsibble(index = datetime, key = location)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    box(
      selectInput("var", "Parameter", gsub(
        "X", "x",
        toupper(names(env_data[3:16]))
      )),
      width = 3
    ),
    box(
      selectInput("loc", "Location", map_chr(
        unique(env_data[["location"]]),
        function(x) {
          x <- strsplit(x, "_")[[1]]
          paste(
            toupper(substring(x, 1, 1)), substring(x, 2),
            sep = "", collapse = " "
          )
        }
      )),
      width = 3
    ),
    box(
      selectInput("yr", "Year", sort(unique(year(env_data[["datetime"]])))),
      width = 3
    ),
    box(
      selectInput("agg", "Aggregate With", c("Max", "Mean", "Median")),
      width = 2
    ),
    box(plotOutput("p", click = "plot_click"), uiOutput("back"), width = 12),
    tags[["head"]](tags[["style"]](HTML(
      ".content-wrapper, .right-side {
        background-color: #ffffff;
      }"
    )))
  )
)

server <- function(input, output, ...) {
  current_day <- reactiveVal()

  current_data <- reactive({
    .data <- filter(
      env_data,
      location == make_clean_names(input[["loc"]]),
      year(datetime) == input[["yr"]]
    )
    if (!nrow(.data)) {
      return("empty")
    }
    if (!length(current_day())) {
      return(.data %>%
        as_tibble() %>%
        group_by(date = date(datetime), location) %>%
        summarise(
          agg_var = !!sym(tolower(input[["var"]])) %>%
            eval(sym(input[["agg"]]))(),
          agg_aqi = aqi %>%
            eval(sym(input[["agg"]]))() %>%
            as.numeric()
        ) %>%
        ungroup() %>%
        mutate(
          aqi_cat = aqi_cat(agg_aqi),
          month = month(date, label = TRUE),
          mday = day(date)
        ) %>%
        as_tsibble(index = date))
    }
    filter(.data, date == current_day())
  }) %>%
    bindCache(
      input[["var"]], input[["agg"]],
      input[["loc"]], input[["yr"]],
      current_day()
    )

  output[["p"]] <- renderPlot(
    {
      if (!is.character(current_data())) {
        intrvl <- feasts:::interval_to_period(interval(current_data()))
        if (intrvl == days(1)) {
          if (!all(is.na(current_data()[["agg_var"]]))) {
            p <- current_data() %>%
              ggplot_calendar_heatmap("date",
                ifelse(input[["var"]] == "AQI", "aqi_cat", "agg_var"),
                monthBorderSize = .6
              ) +
              geom_text(
                data = mutate(current_data(),
                  wday = wday(date, week_start = 1),
                  week = (seq(yday(first(date)), yday(last(date))) +
                    (wday(floor_date(date, unit = "year"), week_start = 1) - 1)
                    %% 7 + 6) %/% 7 + # !!! Accommodate bug in {ggTimeSeries}
                    ifelse(wday(floor_date(date, unit = "year")) == 2, 1, 0),
                  text_col = case_when(
                    input[["var"]] == "AQI" ~ case_when(
                      agg_aqi > 150 ~ "w",
                      TRUE ~ "b"
                    ),
                    TRUE ~ case_when(
                      agg_var < quantile(
                        current_data()[["agg_var"]], .95,
                        na.rm = TRUE
                      ) ~ "w",
                      TRUE ~ "b"
                    )
                  )
                ),
                aes(week, wday, label = agg_var, col = text_col),
                show.legend = FALSE,
                size = ifelse(
                  substr(input[["var"]], 1, 2) == "BC",
                  1.5, 2.5
                )
              ) +
              scale_colour_manual(values = c(b = "black", w = "white")) +
              theme_bw() +
              labs(
                title = paste0(
                  "Daily ", input[["agg"]], " ", input[["var"]], " at ",
                  input[["loc"]], " in ", input[["yr"]]
                ),
                x = "", y = ""
              ) +
              theme(
                legend.key.size = unit(1, "lines"),
                axis.ticks = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                strip.text = element_blank(),
                legend.position = "top"
              )
            if (input[["var"]] == "AQI") {
              p <- p + scale_fill_manual(values = aqi_pal, drop = FALSE) +
                guides(fill = guide_legend(title = "Level", nrow = 1))
            } else {
              p <- p + scale_fill_viridis(option = "C") +
                guides(fill = guide_colourbar(title = input[["var"]])) +
                theme(legend.key.width = unit(.11, "npc"))
            }
            return(p)
          } else {
            return(ggplot(tibble(x = 1, y = 1), aes(x, y)) +
              geom_text(label = "NO DATA", size = 11) +
              theme_void())
          }
        } else {
          if (!all(is.na(current_data()[[tolower(input[["var"]])]]))) {
            p <- current_data() %>%
              ggplot(aes(hour, !!sym(tolower(input[["var"]])))) +
              geom_errorbar(
                data = (.day_data <- filter(
                  env_data,
                  location == make_clean_names(input[["loc"]]),
                  year(datetime) == input[["yr"]]
                ) %>%
                  as_tibble() %>%
                  group_by(hour = hour(datetime)) %>%
                  summarise(
                    lower = quantile(
                      !!sym(tolower(input[["var"]])),
                      prob = .025,
                      na.rm = TRUE
                    ),
                    q50 = quantile(
                      !!sym(tolower(input[["var"]])),
                      prob = .5,
                      na.rm = TRUE
                    ),
                    upper = quantile(
                      !!sym(tolower(input[["var"]])),
                      prob = .975,
                      na.rm = TRUE
                    )
                  )),
                mapping = aes(hour, q50, ymin = lower, ymax = upper),
                col = "grey55",
                width = .2
              ) +
              geom_point(
                data = .day_data,
                mapping = aes(hour, q50),
                col = "grey55",
                size = .5
              ) +
              geom_line(size = .8, col = "steelblue") +
              theme_bw() +
              labs(
                title = paste0(
                  "Hourly ", input[["var"]], " at ", input[["loc"]],
                  " on ", fmt_date(current_day())
                ),
                x = "Hour of the Day", y = "AQI"
              ) +
              theme(
                panel.grid = element_blank(),
                legend.position = "top"
              )
            if (input[["var"]] == "AQI") {
              p <- p + geom_point(
                aes(fill = aqi_cat),
                shape = 21, col = "steelblue",
                size = 2
              ) + scale_fill_manual(values = aqi_pal, drop = FALSE) +
                guides(fill = guide_legend(title = "Level", nrow = 1))
            } else {
              p <- p + geom_point(col = "steelblue", size = 2)
            }
            return(p)
          } else {
            return(ggplot(tibble(x = 1, y = 1), aes(x, y)) +
              geom_text(label = "NO DATA", size = 11) +
              theme_void())
          }
        }
      } else {
        return(ggplot(tibble(x = 1, y = 1), aes(x, y)) +
          geom_text(label = "NO DATA", size = 11) +
          theme_void())
      }
    },
    res = 110
  ) %>%
    bindCache(
      input[["var"]], input[["agg"]],
      input[["loc"]], input[["yr"]],
      current_day()
    )

  observe({
    if (!is.character(current_data())) {
      cd <- input[["plot_click"]]
      if (!is.null(cd)) {
        cd[["x"]] <- floor(cd[["x"]] + .5)
        cd[["y"]] <- floor(cd[["y"]] + .5)
      }
      if (wday(floor_date(current_data()[["date"]], unit = "year")) == 2) {
        # !!! Accommodate bug in {ggTimeSeries}
        cd[["x"]] <- cd[["x"]] - 1
      }
      intrvl <- feasts:::interval_to_period(interval(current_data()))
      date <- ymd(paste0(input[["yr"]], "-01-01")) +
        7 * (cd[["x"]] - 1) + cd[["y"]] -
        wday(floor_date(current_data()[["date"]][1], unit = "year"), week_start = 1)
      if (all(
        cd[["mapping"]][["x"]] == "WeekOfYear", year(date) == input[["yr"]],
        !is.null(cd), length(date) == 1, intrvl == days(1)
      )) {
        current_day(date)
      }
      if (length(current_day())) {
        if (year(current_day()) != input[["yr"]]) {
          current_day(
            update(current_day(), year = as.numeric(input[["yr"]]))
          )
        }
      }
    }
  })

  output[["back"]] <- renderUI({
    if (length(current_day())) {
      actionButton("clear", "Back", icon("chevron-left"))
    }
  })

  observeEvent(input[["clear"]], current_day(NULL))

  observeEvent(input[["plot_click"]], current_day(NULL))
}

shinyApp(ui, server)
