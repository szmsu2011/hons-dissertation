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
      mapping <-
        if (intrvl == days(1)) {
          list(aes(mday, month), aes(label = max_aqi))
        } else {
          list(aes(hour, aqi), aes(label = aqi))
        }
      p <- current_data() %>%
        ggplot(mapping[[1]]) +
        scale_x_continuous(expand = expansion()) +
        scale_fill_manual(values = aqi_pal, drop = FALSE) +
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
        )
      if (intrvl == days(1)) {
        return(p +
          geom_tile(aes(fill = aqi_cat), width = .95, height = .95) +
          geom_text(mapping[[2]], size = 3) +
          labs(
            title = paste("Daily Max AQI at", "Queen Street,", "2019"),
            x = "Day of the Month", y = "Month"
          ) +
          scale_y_discrete(expand = expansion(), limits = rev))
      }
      p + geom_col(aes(fill = aqi_cat)) +
        geom_text(mapping[[2]], size = 3, vjust = -.5) +
        scale_y_continuous(expand = c(0, 0, .1, 0))
    },
    res = 110
  )

  observe({
    cd <- input[["plot_click"]]
    if (!is.null(cd)) {
      cd <- map(c("x", "y"), function(x) floor(cd[[x]] + .5))
    }
    intrvl <- feasts:::interval_to_period(interval(current_data()))
    date <- ymd(paste(2019, (12:1)[cd[[2]]], cd[[1]]))
    if (!(is.null(cd[[1]]) | is.na(date) | intrvl != days(1))) {
      current_day(date)
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
