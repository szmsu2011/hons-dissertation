library(shiny)

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
    hour = hour(datetime)
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
  plotlyOutput("p"),
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

  output[["p"]] <- renderPlotly({
    intrvl <- feasts:::interval_to_period(interval(current_data()))
    mapping <-
      if (intrvl == days(1)) {
        list(aes(mday, month), aes(label = max_aqi))
      } else {
        list(aes(hour, date), aes(label = aqi))
      }

    p <- current_data() %>%
      ggplot(mapping[[1]]) +
      geom_tile(aes(fill = aqi_cat), width = .95, height = .95) +
      geom_text(mapping[[2]], size = 3) +
      scale_x_continuous(expand = expansion()) +
      scale_fill_manual(values = aqi_pal, drop = FALSE) +
      guides(fill = guide_legend(title = "AQI", nrow = 1)) +
      theme_bw() +
      labs(x = "Hour of the Day", y = "Date") +
      theme(
        axis.ticks = element_blank(),
        panel.border = element_blank()
      )
    if (intrvl == days(1)) {
      return(p +
        labs(x = "Day of the Month", y = "Month") +
        scale_y_discrete(expand = expansion(), limits = rev))
    }
    p
  })

  observe({
    cd <- event_data("plotly_click")
    date <- ymd(paste(2019, (12:1)[cd[["y"]]], cd[["x"]]))

    if (!(is.null(cd[["x"]]) | is.na(date))) {
      current_day(
        ymd(paste(2019, (12:1)[cd[["y"]]], cd[["x"]]))
      )
    }
  })

  output[["back"]] <- renderUI({
    if (length(current_day())) {
      actionButton("clear", "Back", icon("chevron-left"))
    }
  })

  observeEvent(input[["clear"]], current_day(NULL))
}

shinyApp(ui, server)
