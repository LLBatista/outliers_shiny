# Shiny module: a table with the answers collected so far.
box::use(
  shiny,
  tools[toTitleCase],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("table-list"), "Answers so far"),
    shiny$uiOutput(ns("empty")),
    shiny$div(class = "table-responsive", shiny$tableOutput(ns("table")))
  )
}

#' `responses` is a reactive returning a data frame of answers.
#' @export
server <- function(id, responses) {
  shiny$moduleServer(id, function(input, output, session) {
    # Shown instead of an empty table.
    output$empty <- shiny$renderUI({
      if (nrow(responses()) == 0) {
        shiny$p(
          class = "empty-state",
          "No answers saved yet. They will appear here after you submit."
        )
      }
    })

    output$table <- shiny$renderTable(
      {
        data <- responses()
        shiny$req(nrow(data) > 0)
        # "product" -> "Product", "positive_lot" -> "Positive Lot"
        names(data) <- toTitleCase(gsub("_", " ", names(data)))
        data
      },
      width = "100%",
      striped = TRUE
    )
  })
}
