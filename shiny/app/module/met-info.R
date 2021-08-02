met_info_ui <- function(id) {
  ns <- NS(id)

  tagList(reactableOutput(ns("met_info")))
}

met_info_mod <- function(id, state) {
  module <- function(input, output, session) {
    data <- append_data(NULL, "queen_street") %>%
      filter(year(datetime) == 2020, month(datetime) == 1)

    bar_chart <- function(label, width, fill, height = "16px") {
      if (is.na(label)) {
        return(NULL)
      }
      width <- paste0(width * 100, "%")

      div(
        style = list(display = "flex", alignItems = "center"),
        label,
        div(
          style = list(flexGrow = 1, marginLeft = "8px", background = NULL),
          div(style = list(background = fill, width = width, height = height))
        )
      )
    }

    bc <- qualitative_hcl(4)

    data %>%
      as_tibble() %>%
      mutate(
        date = stamp("March 1")(datetime),
        hour = sprintf("%02d:00", hour(datetime))
      ) %>%
      select(date, hour, temp, rh, wind_dir, ws, aqi) %>%
      reactable(
        columns = list(
          date = colDef(name = "Date", grouped = JS("
            function(cellInfo) {
              return cellInfo.value;
            }
          ")),
          hour = colDef(name = "Time", width = 55),
          temp = colDef(
            name = "Temperature", align = "left", cell = function(value) {
              bar_chart(value, value / max(data[["temp"]], na.rm = TRUE), bc[1])
            }
          ),
          rh = colDef(
            name = "Relative Humidity", align = "left", cell = function(value) {
              bar_chart(value, value / max(data[["rh"]], na.rm = TRUE), bc[2])
            }
          ),
          ws = colDef(
            name = "Wind Speed", align = "left", cell = function(value) {
              bar_chart(value, value / max(data[["ws"]], na.rm = TRUE), bc[3])
            }
          )
        ),
        groupBy = "date",
        defaultExpanded = TRUE,
        pagination = FALSE
      )
  }

  moduleServer(id, module)
}
