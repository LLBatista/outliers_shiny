# Shiny module: the form where the user picks a product and a date.
box::use(
  shiny,
)
box::use(
  app/logic/products[lots_for_product],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("box-open"), 
             "Which product did you use?"),
    shiny$selectInput(
      ns("product"),
      label = "Product",
      choices = NULL
    ),
    shiny$selectInput(ns('lot'), 
                      label = 'Lot', 
                      choices = NULL) ,
    shiny$dateInput(
      ns("date"),
      label = "Date of use",
      value = Sys.Date(),
      max = Sys.Date()
    ),
    shiny$actionButton(ns("submit"), 
                       "Submit", 
                       class = "btn-primary", 
                       icon = shiny$icon("check"))
  )
}

#' Server part of the form.
#'
#' `products` is a character vector with the options to show.
#' Returns a reactive that holds the latest submitted answer.
#' @export
server <- function(id, product_id) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(
      session,
      "product",
      choices = c("Choose a product..." = "", unique(product_id$product))
    )
    
    shiny$observeEvent(input$product, {
      shiny$updateSelectInput(
        session, 'lot',
        choices = lots_for_product(product_id, input$product)
      )
    })

    shiny$eventReactive(input$submit, {
      shiny$validate(
        shiny$need(input$product != "", "Please choose a product."),
        shiny$need(shiny$isTruthy(input$lot), 'Please choose a lot.'),
        shiny$need(length(input$date) == 1, "Please choose a date.")
      )
      list(product = input$product, lot = input$lot, date = input$date)
    })
  })
}
