# Shiny module: the first section of the experiment form: the instrument, the product
# and its lot. Saving happens in experiment_form.R.
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
    )
  )
}

#' Server part of the form.
#'
#' - `product_id()`: table with the columns product and lot (reactive: it grows
#'   when a lot is added)
#' - `checked_instruments()`: instruments with a daily check on the chosen date
#' - `changes()`: the change log (to show who added a lot)
#' - `add_product_lot(product, lot)`, `remove_product_lot(product, lot)`: lot changes
#' Returns a list:
#' - `collect(focus)`: list(instrument, product, lot), or NULL after showing what is
#'   missing (`focus`: move focus to the first problem)
#' - `clear()`: empties product and lot (after saving; the instrument stays)
#' - `product()`: the chosen product
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
          "the day on the instrument first (Change details, then answer \"Yes\")."
        )
      }
    })

    # --- Product and lot -------------------------------------------------------------
    # The product the user chose, kept here: the list is refilled after every logged
    # change, and must not bring back a product that was just emptied after saving.
    chosen_product <- shiny$reactiveVal("")
    shiny$observeEvent(input$product, chosen_product(input$product))
    shiny$observeEvent(product_id(), {
      shiny$updateSelectInput(session, "product",
        choices = c("Choose a product..." = "", unique(product_id()$product)),
        selected = shiny$isolate(chosen_product())
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

    # --- Collecting the answers (when "Save answer" is pressed) ----------------------
    # Each message is shown under its field and removed when the field changes.
    field_errors$clear_on_change(input, session, c("instrument", "product", "lot"))

    collect <- function(focus = TRUE) {
      instrument_error <- if (!shiny$isTruthy(input$instrument)) {
        "Please choose the instrument (only instruments with a daily check on this date)."
      } else if (!(input$instrument %in% checked_instruments())) {
        "This instrument has no daily check on this date."
      }
      ok <- field_errors$show(session, focus = focus, list(
        instrument = instrument_error,
        product = if (!shiny$isTruthy(input$product)) "Please choose a product.",
        lot = if (!shiny$isTruthy(input$lot)) "Please choose a lot."
      ))
      if (ok) list(instrument = input$instrument, product = input$product, lot = input$lot)
    }

    # After a save: empty product and lot so the same answer isn't saved twice (the
    # instrument stays: the next answer is usually on the same one).
    clear <- function() {
      chosen_product("")
      chosen_lot("")
      shiny$updateSelectInput(session, "product", selected = "")
      shiny$updateSelectInput(session, "lot", choices = c("Choose a lot..." = ""), selected = "")
    }

    list(collect = collect, clear = clear, product = shiny$reactive(input$product))
  })
}
