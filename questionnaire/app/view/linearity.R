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
  # For now only the product and lot; the fields for this experiment go here later.
  insert_product$ui(ns("product"))
}

#' Passes everything on to the product form (see insert_product.R).
#' @export
server <- function(id, product_id, checked_instruments, add_product_lot) {
  shiny$moduleServer(id, function(input, output, session) {
    insert_product$server("product",
      product_id = product_id,
      checked_instruments = checked_instruments,
      add_product_lot = add_product_lot
    )
  })
}
