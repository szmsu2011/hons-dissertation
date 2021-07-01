callback_ui <- function(id) {
  ns <- NS(id)

  tagList(
    column(
      width = 6,
      verbatimTextOutput(ns("map_info"))
    )
  )
}

callback_server <- function(id, state, ...) {
  arg <- list(...)

  module <- function(input, output, session) {
    observeEvent(state[["map_onclick"]], {
      output[["map_info"]] <- renderPrint({
        arg[[1]]()
      })
    })
  }

  moduleServer(id, module)
}
