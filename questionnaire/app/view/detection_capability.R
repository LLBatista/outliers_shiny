# Shiny module: the page for the "Detection Capability" experiment.
box::use(
  shiny,
  shiny.fluent[FontIcon, Stack, Text],
)

box::use(
  app/view/insert_product,
)

#' @export
ui <- function(id, product_id) {
  ns <- shiny$NS(id)
  # Form on the left, measurements on the right (they stack on small screens).
  Stack(
    horizontal = TRUE,
    wrap = TRUE,
    tokens = list(childrenGap = 24),
    shiny$div(class = "column-narrow", insert_product$ui(ns("product"), product_id)),
    shiny$div(
      class = "column-wide app-card",
      Text(
        variant = "large", class = "card-title",
        FontIcon(iconName = "TestBeaker"), "Detection capability measurements"
      ),
      Text("Coming soon: the fields for this experiment.")
    )
  )
}

#' @export
server <- function(id, product_id) {
  shiny$moduleServer(id, function(input, output, session) {
    insert_product$server("product", product_id = product_id)
  })
}
