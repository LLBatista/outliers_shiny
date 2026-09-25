# Shiny module: the page for the "Detection Capability" experiment.
box::use(
  shiny,
)

box::use(
  app/view/insert_product,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  # For now only the product and lot; the fields for this experiment go here later.
  insert_product$ui(ns("product"))
}

#' @export
server <- function(id, product_id) {
  shiny$moduleServer(id, function(input, output, session) {
    insert_product$server("product", product_id = product_id)
  })
}
