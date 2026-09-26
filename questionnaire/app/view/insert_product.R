# Shiny module: the form where the user picks the instrument, the product and its lot.
box::use(
  shiny,
)

box::use(
  app/logic/products[lots_for_product],
  app/view/field_errors,
  app/view/lot_changes,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("box-open"), "Instrument, product and lot"),
    # Only instruments with a daily check on the chosen date can be used.
    shiny$selectInput(ns("instrument"), "Instrument",
      choices = NULL, selectize = FALSE, width = "100%"
    ),
    field_errors$message_ui(ns("instrument")),
    shiny$uiOutput(ns("no_instrument")),
    shiny$selectInput(ns("product"), "Product", choices = NULL, selectize = FALSE, width = "100%"),
    field_errors$message_ui(ns("product")),
    shiny$selectInput(ns("lot"), "Lot", choices = NULL, selectize = FALSE, width = "100%"),
    field_errors$message_ui(ns("lot")),
    shiny$conditionalPanel(
      condition = "input.product != ''",
      ns = ns,
      lot_changes$ui(ns("lot_changes"))
    ),
    shiny$actionButton(ns("submit"), "Save answer",
      class = "btn-primary", icon = shiny$icon("check")
    ),
    # Confirmation after saving; screen readers read it out (it is a live region).
    shiny$div(class = "form-success", shiny$textOutput(ns("saved")))
  )
}

#' Server part of the form.
#'
#' - `product_id()`: table with the columns product and lot (reactive: it grows
#'   when a lot is added)
#' - `checked_instruments()`: instruments with a daily check on the chosen date
#' - `changes()`: the change log (to show who added a lot)
#' - `add_product_lot(product, lot)`, `remove_product_lot(product, lot)`: lot changes
#' Returns a reactive that holds the latest saved answer.
#' @export
server <- function(id, product_id, checked_instruments, changes, add_product_lot,
                   remove_product_lot) {
  shiny$moduleServer(id, function(input, output, session) {
    # --- Instrument: only the ones checked on the chosen date -------------------
    shiny$observeEvent(checked_instruments(), ignoreNULL = FALSE, {
      available <- checked_instruments()
      current <- shiny$isolate(input$instrument)
      shiny$updateSelectInput(session, "instrument",
        choices = c("Choose an instrument..." = "", available),
        selected = if (isTRUE(current %in% available)) current else ""
      )
    })

    output$no_instrument <- shiny$renderUI({
      if (length(checked_instruments()) == 0) {
        shiny$p(
          class = "notice",
          shiny$icon("circle-info"),
          "No instrument has had its daily check on this date yet. Do the first run of",
          "the day on the instrument first (Start over, then answer \"Yes\")."
        )
      }
    })

    # --- Product and lot -------------------------------------------------------------
    shiny$observeEvent(product_id(), {
      shiny$updateSelectInput(session, "product",
        choices = c("Choose a product..." = "", unique(product_id()$product)),
        selected = shiny$isolate(input$product)
      )
    })

    lots <- shiny$reactive({
      if (!shiny$isTruthy(input$product)) {
        return(character(0))
      }
      lots_for_product(product_id(), input$product)
    })

    # The lot the user chose, kept so it survives when the list is refreshed.
    chosen_lot <- shiny$reactiveVal("")
    shiny$observeEvent(input$lot, chosen_lot(input$lot))
    shiny$observeEvent(input$product, chosen_lot(""))

    # Only the lots of the chosen product.
    shiny$observeEvent(lots(), ignoreNULL = FALSE, {
      current <- shiny$isolate(chosen_lot())
      shiny$updateSelectInput(session, "lot",
        choices = c("Choose a lot..." = "", lots()),
        selected = if (current %in% lots()) current else ""
      )
    })

    # Who added the chosen lot, "Remove this lot" and "Lot not listed? Add it".
    # Once added, the new lot is selected.
    added <- lot_changes$server("lot_changes",
      existing = lots,
      item = shiny$reactive(input$product),
      selected = shiny$reactive(input$lot),
      changes = changes,
      what = "product",
      add = add_product_lot,
      remove = remove_product_lot
    )
    shiny$observeEvent(added(), {
      chosen_lot(added())
      shiny$updateSelectInput(session, "lot",
        choices = c("Choose a lot..." = "", lots()),
        selected = added()
      )
    })

    # --- Saving ----------------------------------------------------------------------
    # Each message is shown under its field and removed when the field changes.
    field_errors$clear_on_change(input, session, c("instrument", "product", "lot"))

    submission <- shiny$eventReactive(input$submit, {
      instrument_error <- if (!shiny$isTruthy(input$instrument)) {
        "Please choose the instrument (only instruments with a daily check on this date)."
      } else if (!(input$instrument %in% checked_instruments())) {
        "This instrument has no daily check on this date."
      }
      shiny$req(field_errors$show(session, list(
        instrument = instrument_error,
        product = if (!shiny$isTruthy(input$product)) "Please choose a product.",
        lot = if (!shiny$isTruthy(input$lot)) "Please choose a lot."
      )))
      list(instrument = input$instrument, product = input$product, lot = input$lot)
    })

    # After a successful save, confirm it and empty product and lot so the same
    # answer isn't saved twice (the instrument stays: the next answer is usually on
    # the same one). The confirmation goes away when the user starts a new answer.
    saved_message <- shiny$reactiveVal("")
    shiny$observeEvent(input$product, if (input$product != "") saved_message(""))
    output$saved <- shiny$renderText(saved_message())

    shiny$observeEvent(submission(), {
      answer <- submission()
      saved_message(paste0(
        "Answer saved: ", answer$product, ", lot ", answer$lot, ", on ", answer$instrument, "."
      ))
      chosen_lot("")
      shiny$updateSelectInput(session, "product", selected = "")
      shiny$updateSelectInput(session, "lot", choices = c("Choose a lot..." = ""), selected = "")
    })

    submission
  })
}
