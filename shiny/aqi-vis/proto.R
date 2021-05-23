library(shiny)
library(ggTimeSeries)

invisible(purrr::map(c(
  paste0(
    "../../prelim/ggplot2-demo/",
    c(
      "packages", "data-clean", "covid19-lvl",
      "function", "plot"
    ), ".R"
  ),
  paste0(
    "../../prelim/echarts4r-demo/",
    c("packages", "function", "plot"),
    ".R"
  )
), source))

aqi_hourly <- "../../data/akl-aqi-19-20.csv" %>%
  read_csv(locale = locale(tz = "Pacific/Auckland")) %>%
  filter(year(datetime) == 2019, location == "queen_street") %>%
  mutate(
    aqi_cat = aqi_cat(aqi),
    date = as_date(datetime),
    hour = hour(datetime),
    text_col = case_when(
      aqi > 150 ~ "w",
      TRUE ~ "b"
    )
  ) %>%
  as_tsibble(index = datetime)

aqi_daily <- aqi_hourly %>%
  as_tibble() %>%
  group_by(date = date(datetime), location) %>%
  summarise(max_aqi = max(aqi, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    aqi_cat = aqi_cat(max_aqi),
    month = month(date, label = TRUE),
    mday = day(date)
  ) %>%
  as_tsibble(index = date)

ui <- fluidPage(
  plotOutput("p", click = "plot_click"),
  uiOutput("back")
)

server <- function(input, output, ...) {
  day_range <- unique(aqi_daily[["date"]])
  current_day <- reactiveVal()

  current_data <- reactive({
    if (!length(current_day())) {
      return(aqi_daily)
    }
    filter(aqi_hourly, date == current_day())
  })

  output[["p"]] <- renderPlot(
    {
      intrvl <- feasts:::interval_to_period(interval(current_data()))
      if (intrvl == days(1)) {
        return(current_data() %>%
          ggplot_calendar_heatmap("date", "aqi_cat", monthBorderSize = .6) +
          geom_text(
            data = mutate(.data,
              wday = wday(date, week_start = 1),
              week = (seq(yday(first(date)), yday(last(date))) +
                (wday(floor_date(date, unit = "year"), week_start = 1) - 1)
                %% 7 + 6) %/% 7,
              text_col = case_when(
                max_aqi > 150 ~ "w",
                TRUE ~ "b"
              )
            ),
            aes(week, wday, label = max_aqi, col = text_col),
            show.legend = FALSE, size = 2.9
          ) +
          scale_fill_manual(values = aqi_pal, drop = FALSE) +
          scale_colour_manual(values = c(b = "black", w = "white")) +
          guides(fill = guide_legend(title = "Level", nrow = 1)) +
          theme_bw() +
          labs(
            title = paste("Daily Max AQI at", "Queen Street,", "2019"),
            x = "", y = ""
          ) +
          theme(
            legend.key.size = unit(1, "lines"),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            strip.text = element_blank(),
            legend.position = "top"
          ))
      }
      current_data() %>%
        ggplot(aes(hour, aqi)) +
        geom_col(aes(fill = aqi_cat)) +
        geom_text(
          aes(label = aqi, col = text_col),
          size = 3, vjust = 2,
          show.legend = FALSE
        ) +
        scale_x_continuous(expand = expansion()) +
        scale_fill_manual(values = aqi_pal, drop = FALSE) +
        scale_colour_manual(values = c(b = "black", w = "white")) +
        guides(fill = guide_legend(title = "Level", nrow = 1)) +
        theme_bw() +
        labs(
          title = paste("Hourly AQI at", "Queen Street,", current_day()),
          x = "Hour of the Day", y = "AQI"
        ) +
        theme(
          legend.key.size = unit(1, "lines"),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          legend.position = "top"
        ) +
        scale_y_continuous(expand = expansion())
    },
    res = 110
  )

  observe({
    cd <- input[["plot_click"]]
    if (!is.null(cd)) {
      cd[["x"]] <- floor(cd[["x"]] + .5)
      cd[["y"]] <- floor(cd[["y"]] + .5)
    }
    intrvl <- feasts:::interval_to_period(interval(current_data()))
    date <- ymd(paste0(2019, "-01-01")) + 7 * (cd[["x"]] - 1) + cd[["y"]] -
      wday(floor_date(current_data()[["date"]][1], unit = "year"), week_start = 1)
    if (all(
      cd[["mapping"]][["x"]] == "WeekOfYear", year(date) == 2019,
      !is.null(cd), length(date) == 1, intrvl == days(1)
    )) {
      current_day(date)
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
