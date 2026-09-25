# Shiny module: a table (Fluent DetailsList) with the rows saved so far.
box::use(
  shiny,
  shiny.fluent[DetailsList, Text, reactOutput, renderReact],
)

#' @export
ui <- function(id, title = "Answers so far") {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    Text(variant = "large", class = "card-title", shiny$icon("table-list"), title),
    reactOutput(ns("table"))
  )
}

#' `responses` is a reactive returning a data frame.
#' @export
server <- function(id, responses) {
  shiny$moduleServer(id, function(input, output, session) {
    output$table <- renderReact({
      data <- responses()
      # One column definition per column of the data frame.
      columns <- lapply(names(data), function(name) {
        list(key = name, fieldName = name, name = name, minWidth = 90, isResizable = TRUE)
      })
      DetailsList(items = data, columns = columns, selectionMode = 0, compact = TRUE)
    })
  })
}
