# Shiny module: "Lot not listed? Add it" - a small form under a lot dropdown.
# Used for the control lots (daily check) and the product lots (experiments).
box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  # <details> opens and closes by itself in the browser: no server code needed.
  shiny$tags$details(
    class = "change-box",
    shiny$tags$summary("Lot not listed? Add it"),
    shiny$textInput(ns("new_lot"), "New lot number", width = "100%"),
    shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
    shiny$actionButton(ns("add"), "Add lot", icon = shiny$icon("plus")),
    shiny$div(class = "form-success", shiny$textOutput(ns("added_message")))
  )
}

#' `existing()`: the lots already in the list (a reactive).
#' `add(lot)`: saves the new lot and logs the change (a function from main.R).
#' Returns a reactive with the lot that was just added.
#' @export
server <- function(id, existing, add) {
  shiny$moduleServer(id, function(input, output, session) {
    show_message <- shiny$reactiveVal(FALSE)
    shiny$observeEvent(input$add, show_message(TRUE))
    shiny$observeEvent(input$new_lot, show_message(FALSE), ignoreInit = TRUE)

    # The lot is added (and logged) here, so that anything reacting to `added()`
    # already sees it in the list.
    added <- shiny$eventReactive(input$add, {
      lot <- trimws(input$new_lot)
      shiny$validate(
        shiny$need(lot != "", "Please type the lot number."),
        shiny$need(!(lot %in% existing()), "This lot is already in the list.")
      )
      add(lot)
      lot
    })

    output$message <- shiny$renderText({
      if (show_message()) added()
      ""
    })

    added_message <- shiny$reactiveVal("")
    shiny$observeEvent(input$new_lot, if (input$new_lot != "") added_message(""))
    output$added_message <- shiny$renderText(added_message())

    shiny$observeEvent(added(), {
      added_message(paste("Lot", added(), "added and logged."))
      shiny$updateTextInput(session, "new_lot", value = "")
    })

    added
  })
}
