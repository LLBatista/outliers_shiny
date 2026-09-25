# Shiny module: the form where the user picks a product and its lot.
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
    shiny$h3(shiny$icon("box-open"), "Which product did you use?"),
    shiny$selectInput(ns("product"), "Product", choices = NULL, selectize = FALSE, width = "100%"),
    shiny$selectInput(ns("lot"), "Lot", choices = NULL, selectize = FALSE, width = "100%"),
    shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
    shiny$actionButton(ns("submit"), "Save answer",
      class = "btn-primary", icon = shiny$icon("check")
    ),
    # Confirmation after saving; screen readers read it out (it is a live region).
    shiny$div(class = "form-success", shiny$textOutput(ns("saved")))
  )
}

#' Server part of the form.
#'
#' `product_id` is a table with the columns product and lot.
#' Returns a reactive that holds the latest saved answer.
#' @export
server <- function(id, product_id) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "product",
      choices = c("Choose a product..." = "", unique(product_id$product))
    )

    # Only the lots of the chosen product.
    shiny$observeEvent(input$product, {
      lots <- if (input$product == "") character(0) else lots_for_product(product_id, input$product)
      shiny$updateSelectInput(session, "lot",
        choices = c("Choose a lot..." = "", lots),
        selected = ""
      )
    })

    # Error messages: shown after a click on Save, hidden again when an answer changes.
    show_message <- shiny$reactiveVal(FALSE)
    shiny$observeEvent(input$submit, show_message(TRUE))
    shiny$observeEvent(list(input$product, input$lot), show_message(FALSE), ignoreInit = TRUE)

    submission <- shiny$eventReactive(input$submit, {
      shiny$validate(
        shiny$need(input$product != "", "Please choose a product."),
        shiny$need(shiny$isTruthy(input$lot), "Please choose a lot.")
      )
      list(product = input$product, lot = input$lot)
    })

    output$message <- shiny$renderText({
      if (show_message()) submission()
      ""
    })

    # After a successful submit, confirm it and empty the form so the same answer
    # isn't saved twice. The confirmation goes away when the user starts a new answer.
    saved_message <- shiny$reactiveVal("")
    shiny$observeEvent(input$product, if (input$product != "") saved_message(""))
    output$saved <- shiny$renderText(saved_message())

    shiny$observeEvent(submission(), {
      answer <- submission()
      saved_message(paste0("Answer saved: ", answer$product, ", lot ", answer$lot, "."))
      shiny$updateSelectInput(session, "product", selected = "")
      shiny$updateSelectInput(session, "lot", choices = c("Choose a lot..." = ""), selected = "")
    })

    submission
  })
}
