# Shiny module: the page for the "Linearity" experiment.
box::use(
  shiny,
)

box::use(
  app/view/insert_product,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$column(5, insert_product$ui(ns("product"))),   # 👈 reused module
    shiny$column(
      7,
      shiny$div(
        class = "app-card",
        shiny$h3(shiny$icon("chart-line"), "Linearity measurements"),
        shiny$p("Coming soon: the fields for this experiment.")
      )
    )
  )
}

#' @export
server <- function(id, product_id) {
  shiny$moduleServer(id, function(input, output, session) {
    insert_product$server("product", product_id = product_id)
  })
}