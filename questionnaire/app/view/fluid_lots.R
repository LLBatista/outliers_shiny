# Shiny module: one instrument fluid (system buffer or system fluid) in the experiment
# form: its lot, the expiry date from data/fluids.csv (the user confirms it matches
# the bottle), adding a lot that is not listed, and - rarely - a second lot.
box::use(
  shiny,
)

box::use(
  app/logic/fluids[expiry_of, lots_for_fluid],
  app/view/field_errors,
  app/view/lot_changes,
)

# Lot, expiry date and "matches the bottle" for lot number `n` (1 or 2).
lot_slot_ui <- function(ns, n) {
  lot <- paste0("lot", n)
  shiny$tagList(
    shiny$selectInput(ns(lot), "Lot", choices = NULL, selectize = FALSE, width = "100%"),
    field_errors$message_ui(ns(lot)),
    shiny$conditionalPanel(
      condition = paste0("input.", lot, " != ''"),
      ns = ns,
      shiny$uiOutput(ns(paste0("expiry", n))),
      shiny$checkboxInput(ns(paste0("expiry_ok", n)), "The expiry date matches the bottle",
        width = "100%"
      ),
      field_errors$message_ui(ns(paste0("expiry_ok", n)))
    )
  )
}

#' `label`: how the fluid is called on screen, e.g. "System buffer".
#' @export
ui <- function(id, label) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "fluid-block",
    shiny$h4(class = "section-title", label),
    lot_slot_ui(ns, 1),
    lot_changes$ui(ns("lot_changes"), ask_expiry = TRUE),
    # A second lot of the same fluid: rarely needed, so it is hidden behind a button.
    shiny$conditionalPanel(
      condition = "!output.has_second",
      ns = ns,
      shiny$actionButton(ns("add_second"), paste("Add a second", tolower(label)),
        class = "btn-link link-action", icon = shiny$icon("plus")
      )
    ),
    shiny$conditionalPanel(
      condition = "output.has_second",
      ns = ns,
      shiny$div(
        class = "second-lot",
        shiny$h5(id = ns("second_title"), paste("Second", tolower(label))),
        lot_slot_ui(ns, 2),
        shiny$actionButton(ns("remove_second"), paste("Remove the second", tolower(label)),
          class = "btn-link link-action", icon = shiny$icon("xmark")
        )
      )
    )
  )
}

# "Expiry date 31 Mar 2027", or a warning when the lot had expired on the date of
# the experiment.
expiry_box <- function(expiry, experiment_date) {
  if (expiry == "") {
    return(shiny$div(class = "expiry-box expired", "No expiry date on file for this lot."))
  }
  expiry_date <- as.Date(expiry)
  expired <- isTRUE(expiry_date < as.Date(experiment_date))
  shiny$div(
    class = paste("expiry-box", if (expired) "expired"),
    shiny$span("Expiry date"),
    shiny$strong(format(expiry_date, "%d %b %Y")),
    if (expired) shiny$p(shiny$icon("triangle-exclamation"), "Expired before the experiment date.")
  )
}

# The error messages for `collect()`.
slot_errors <- function(input, n, other_lot = NULL) {
  lot <- input[[paste0("lot", n)]]
  errors <- list(
    if (!shiny$isTruthy(lot)) {
      "Please choose the lot."
    } else if (identical(lot, other_lot)) {
      "This is the same lot as the first one."
    },
    if (shiny$isTruthy(lot) && !isTRUE(input[[paste0("expiry_ok", n)]])) {
      "Please check the expiry date on the bottle."
    }
  )
  names(errors) <- paste0(c("lot", "expiry_ok"), n)
  errors
}

# Where a lot that was just added goes: into the second lot if that one is open and
# still empty (and the first is filled), otherwise into the first.
slot_for_new_lot <- function(has_second, lot1, lot2) {
  if (has_second && shiny$isTruthy(lot1) && !shiny$isTruthy(lot2)) "lot2" else "lot1"
}

#' - `fluid`: the name in data/fluids.csv, e.g. "System Buffer"
#' - `fluids()`: table with fluid, lot, expiry_date (reactive: it changes when a lot is added)
#' - `experiment_date()`: the date of the experiment (to warn about expired lots)
#' - `changes()`: the change log (to show who added a lot)
#' - `add_lot(fluid, lot, expiry_date)`, `remove_lot(fluid, lot)`: lot changes
#'
#' Returns a list:
#' - `collect(focus)`: list(lot, expiry, lot_2, expiry_2), or NULL after showing what
#'   is missing (`focus`: move focus to the first problem)
#' @export
server <- function(id, fluid, fluids, experiment_date, changes, add_lot, remove_lot) {
  shiny$moduleServer(id, function(input, output, session) {
    lots <- shiny$reactive(lots_for_fluid(fluids(), fluid))
    fields <- c("lot1", "expiry_ok1", "lot2", "expiry_ok2")
    field_errors$clear_on_change(input, session, fields)

    # The lot chosen in each list, kept here so it survives when the lists are refilled.
    chosen <- list(lot1 = shiny$reactiveVal(""), lot2 = shiny$reactiveVal(""))
    lapply(names(chosen), function(lot) {
      shiny$observeEvent(input[[lot]], chosen[[lot]](input[[lot]]))
    })

    # Fill both lists, and refill them when the lots change (keeping the choice).
    shiny$observeEvent(lots(), ignoreNULL = FALSE, {
      for (lot in names(chosen)) {
        current <- shiny$isolate(chosen[[lot]]())
        shiny$updateSelectInput(session, lot,
          choices = c("Choose a lot..." = "", lots()),
          selected = if (current %in% lots()) current else ""
        )
      }
    })

    # The expiry date is confirmed for one lot: choosing another lot asks again.
    lapply(1:2, function(n) {
      shiny$observeEvent(input[[paste0("lot", n)]], ignoreInit = TRUE, {
        shiny$updateCheckboxInput(session, paste0("expiry_ok", n), value = FALSE)
      })
      output[[paste0("expiry", n)]] <- shiny$renderUI({
        lot <- input[[paste0("lot", n)]]
        shiny$req(lot)
        expiry_box(expiry_of(fluids(), fluid, lot), experiment_date())
      })
    })

    # --- The second lot ---------------------------------------------------------------
    has_second <- shiny$reactiveVal(FALSE)
    output$has_second <- shiny$reactive(has_second())
    shiny$outputOptions(output, "has_second", suspendWhenHidden = FALSE)

    shiny$observeEvent(input$add_second, {
      has_second(TRUE)
      session$sendCustomMessage("focus-element", session$ns("second_title"))
    })
    shiny$observeEvent(input$remove_second, {
      has_second(FALSE)
      chosen$lot2("")
      shiny$updateSelectInput(session, "lot2", selected = "")
      field_errors$show(session, list(lot2 = "", expiry_ok2 = ""), focus = FALSE)
    })

    # --- Adding a lot that is not listed (selected once added) ------------------------
    added <- lot_changes$server("lot_changes",
      existing = lots,
      item = shiny$reactive(fluid),
      selected = shiny$reactive(input$lot1),
      changes = changes,
      what = "fluid",
      add = add_lot,
      remove = remove_lot,
      ask_expiry = TRUE
    )
    shiny$observeEvent(added(), {
      into <- slot_for_new_lot(has_second(), input$lot1, input$lot2)
      chosen[[into]](added())
      shiny$updateSelectInput(session, into,
        choices = c("Choose a lot..." = "", lots()),
        selected = added()
      )
    })

    collect <- function(focus = TRUE) {
      errors <- slot_errors(input, 1)
      if (has_second()) errors <- c(errors, slot_errors(input, 2, other_lot = input$lot1))
      if (!field_errors$show(session, errors, focus = focus)) {
        return(NULL)
      }
      second <- has_second()
      list(
        lot = input$lot1,
        expiry = expiry_of(fluids(), fluid, input$lot1),
        lot_2 = if (second) input$lot2 else "",
        expiry_2 = if (second) expiry_of(fluids(), fluid, input$lot2) else ""
      )
    }

    list(collect = collect)
  })
}
