# Shiny module: the form every experiment starts with (Detection Capability, Linearity,
# and experiments to come). Sections:
#   1. instrument, product and lot    (insert_product.R)
#   2. instrument fluids              (fluid_lots.R)
#   3. sample preparation             (sample_preparation.R)
# "Save answer" checks all sections and returns everything as one answer.
box::use(
  shiny,
)

box::use(
  app/view/fluid_lots,
  app/view/insert_product,
  app/view/sample_preparation,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    insert_product$ui(ns("product")),
    fluid_lots$ui(ns("fluids")),
    sample_preparation$ui(ns("samples")),
    shiny$div(
      class = "save-row",
      shiny$actionButton(ns("save"), "Save answer",
        class = "btn-primary", icon = shiny$icon("check")
      ),
      # What happens after saving, so nothing is carried over by surprise.
      shiny$p(
        class = "save-hint",
        "After saving, product and lot are emptied. Instrument, fluids and sample",
        "preparation stay filled in for your next answer: change them if they are different."
      ),
      # Confirmation after saving; screen readers read it out (it is a live region).
      shiny$div(class = "form-success", shiny$textOutput(ns("saved")))
    )
  )
}

# "Answer saved: ORG200, lot A1, on Analyzer 01. Expiry date of lot SF2601 corrected to
# 28 Feb 2027 in the list."
saved_text <- function(answer, corrections) {
  corrected <- vapply(corrections, function(c) {
    paste0(
      " Expiry date of lot ", c$lot, " corrected to ",
      format(as.Date(c$new), "%d %b %Y"), " in the list."
    )
  }, character(1))
  paste0(
    "Answer saved: ", answer$product, ", lot ", answer$lot, ", on ", answer$instrument, ".",
    paste(corrected, collapse = "")
  )
}

#' Arguments:
#' - `product_id()`, `checked_instruments()`, `add_product_lot`, `remove_product_lot`:
#'   see insert_product.R
#' - `fluids()`, `add_fluid_lot`, `remove_fluid_lot`, `undo_expiry`: see fluid_slot.R
#' - `correct_expiry(fluid, lot, old, new)`: corrects a lot's expiry date in the list
#' - `experiment_date()`: the date chosen on the landing page
#' - `changes()`: the change log
#' Returns a reactive with the latest saved answer (a named list, see responses.R).
#' @export
server <- function(id, product_id, checked_instruments, changes, add_product_lot,
                   remove_product_lot, fluids, experiment_date, add_fluid_lot,
                   remove_fluid_lot, correct_expiry, undo_expiry) {
  shiny$moduleServer(id, function(input, output, session) {
    product <- insert_product$server("product",
      product_id = product_id,
      checked_instruments = checked_instruments,
      changes = changes,
      add_product_lot = add_product_lot,
      remove_product_lot = remove_product_lot
    )
    fluid_section <- fluid_lots$server("fluids",
      fluids = fluids,
      experiment_date = experiment_date,
      changes = changes,
      add_lot = add_fluid_lot,
      remove_lot = remove_fluid_lot,
      undo_expiry = undo_expiry
    )
    samples <- sample_preparation$server("samples")

    # Checks every section, top to bottom; focus goes to the first problem only.
    # Expiry dates corrected from the bottle go into the list only when the whole
    # answer is complete and saved.
    submission <- shiny$eventReactive(input$save, {
      answer <- product$collect(focus = TRUE)
      fluid <- fluid_section$collect(focus = !is.null(answer))
      preparation <- samples$collect(focus = !is.null(answer) && !is.null(fluid))
      shiny$req(answer, fluid, preparation)
      for (c in fluid$corrections) correct_expiry(c$fluid, c$lot, c$old, c$new)
      list(answer = c(answer, fluid$answer, preparation), corrections = fluid$corrections)
    })

    # After a save: confirm it and empty product and lot (see the hint in the UI).
    saved_message <- shiny$reactiveVal("")
    shiny$observeEvent(product$product(), if (product$product() != "") saved_message(""))
    output$saved <- shiny$renderText(saved_message())

    shiny$observeEvent(submission(), {
      saved_message(saved_text(submission()$answer, submission()$corrections))
      product$clear()
    })

    shiny$reactive(submission()$answer)
  })
}
