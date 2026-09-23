# Shiny module: the form where the user picks a product and a date.
box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$wellPanel(
    shiny$h3("Which product did you use?"),
    shiny$selectInput(
      ns("product"),
      label = "Product",
      choices = NULL
    ),
    shiny$dateInput(
      ns("date"),
      label = "Date of use",
      value = Sys.Date(),
      max = Sys.Date()
    ),
    shiny$actionButton(ns("submit"), "Submit", class = "btn-primary")
  )
}

#' Server part of the form.
#'
#' `products` is a character vector with the options to show.
#' Returns a reactive that holds the latest submitted answer.
#' @export
server <- function(id, products) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(
      session,
      "product",
      choices = c("Choose a product..." = "", products)
    )

    shiny$eventReactive(input$submit, {
      shiny$validate(
        shiny$need(input$product != "", "Please choose a product."),
        shiny$need(length(input$date) == 1, "Please choose a date.")
      )
      list(product = input$product, date = input$date)
    })
  })
}
