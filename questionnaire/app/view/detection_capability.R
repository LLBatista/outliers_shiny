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

#' Passes everything on to the product form (see insert_product.R).
#' @export
server <- function(id, ...) {
  args <- list(...)
  shiny$moduleServer(id, function(input, output, session) {
    do.call(insert_product$server, c(list("product"), args))
  })
}
