# Shiny module: a table with saved answers.
box::use(
  shiny,
  tools[toTitleCase],
)

#' @export
ui <- function(id, title = "Answers so far") {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("table-list"), title),
    shiny$uiOutput(ns("empty")),
    shiny$div(class = "table-responsive", shiny$tableOutput(ns("table")))
  )
}

#' `responses` is a reactive returning a data frame of answers;
#' `empty_text` is shown instead of the table when it has no rows.
#' @export
server <- function(id, responses,
                   empty_text = "No answers saved yet. They will appear here after you save one.") {
  shiny$moduleServer(id, function(input, output, session) {
    output$empty <- shiny$renderUI({
      if (nrow(responses()) == 0) shiny$p(class = "empty-state", empty_text)
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
