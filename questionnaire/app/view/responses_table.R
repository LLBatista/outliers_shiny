# Shiny module: a table with the answers collected so far.
box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$h3("Answers so far"),
    shiny$tableOutput(ns("table"))
  )
}

#' `responses` is a reactive returning a data frame of answers.
#' @export
server <- function(id, responses) {
  shiny$moduleServer(id, function(input, output, session) {
    output$table <- shiny$renderTable(responses())
  })
}
