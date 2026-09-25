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
  # Form and measurements side by side on wide screens (tablet in landscape),
  # one above the other on narrower ones (tablet held upright).
  shiny$div(
    class = "row",
    shiny$div(class = "col-lg-5", insert_product$ui(ns("product"))),
    shiny$div(
      class = "col-lg-7",
      shiny$div(
        class = "app-card",
        shiny$h3(shiny$icon("chart-line"), "Detection Capability measurements"),
        shiny$p(class = "placeholder-text", "Coming soon: the fields for this experiment.")
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
