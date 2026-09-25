# Shiny module: the form where the user picks a product and its lot.
box::use(
  shiny,
  shiny.fluent[
    Dropdown.shinyInput, PrimaryButton.shinyInput, Stack, Text, updateDropdown.shinyInput
  ],
)

box::use(
  app/logic/fluent_helpers[as_options],
  app/logic/products[lots_for_product],
)

#' @export
ui <- function(id, product_id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    Stack(
      tokens = list(childrenGap = 16),
      Text(
        variant = "large", class = "card-title",
        shiny$icon("box-open"), "Which product did you use?"
      ),
      Dropdown.shinyInput(
        ns("product"),
        label = "Product",
        placeholder = "Choose a product...",
        options = as_options(unique(product_id$product))
      ),
      # Filled in by the server once a product is chosen.
      Dropdown.shinyInput(
        ns("lot"),
        label = "Lot",
        placeholder = "Choose a lot...",
        options = list()
      ),
      shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
      shiny$div(
        PrimaryButton.shinyInput(
          ns("submit"),
          text = "Submit",
          iconProps = list(iconName = "CheckMark")
        )
      )
    )
  )
}

#' Returns a reactive that holds the latest submitted answer.
#' @export
server <- function(id, product_id) {
  shiny$moduleServer(id, function(input, output, session) {
    # When a product is chosen, show only its lots (and clear the old choice).
    shiny$observeEvent(input$product, {
      updateDropdown.shinyInput(
        session, "lot",
        value = NULL,
        options = as_options(lots_for_product(product_id, input$product))
      )
    })

    submission <- shiny$eventReactive(input$submit, {
      shiny$validate(
        shiny$need(shiny$isTruthy(input$product), "Please choose a product."),
        shiny$need(shiny$isTruthy(input$lot), "Please choose a lot.")
      )
      list(product = input$product, lot = input$lot)
    })

    # Shows the validate() messages under the form.
    output$message <- shiny$renderText({
      submission()
      ""
    })

    submission
  })
}
