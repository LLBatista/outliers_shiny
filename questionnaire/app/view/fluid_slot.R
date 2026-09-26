# Shiny module: one lot of an instrument fluid (system fluid or system buffer) in the
# experiment form:
# - the lot, and its expiry date from the fluids list;
# - "Does the expiry date on the bottle match?" - if not, the user enters the date on
#   the bottle. When the answer is saved, that date is used and corrects the fluids
#   list for everyone (logged; see experiment_form.R and main.R);
# - who corrected the date last, with "Undo this correction";
# - "Lot not listed? Add it" (with its expiry date).
box::use(
  shiny,
  stats[setNames],
)

box::use(
  app/logic/changes[changed_by_text],
  app/logic/fluids[expiry_of, last_expiry_correction, lots_for_fluid],
  app/view/date_field,
  app/view/field_errors,
  app/view/inputs,
  app/view/lot_changes,
)

yes_no <- c("Yes" = "yes", "No" = "no")

nice_date <- function(date) format(as.Date(date), "%d %b %Y")

#' `label`: the title shown above the lot, e.g. "System fluid".
#' @export
ui <- function(id, label) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "fluid-slot",
    shiny$h4(id = ns("title"), class = "section-title", label),
    shiny$selectInput(ns("lot"), "Lot", choices = NULL, selectize = FALSE, width = "100%"),
    field_errors$message_ui(ns("lot")),
    shiny$conditionalPanel(
      condition = "input.lot != ''",
      ns = ns,
      # The expiry date in the fluids list (and who corrected it last).
      shiny$uiOutput(ns("expiry")),
      shiny$conditionalPanel(
        condition = "output.confirming_undo",
        ns = ns,
        shiny$div(
          class = "confirm-box",
          role = "alertdialog",
          `aria-labelledby` = ns("undo_question"),
          shiny$div(
            id = ns("undo_question"), class = "confirm-question",
            shiny$textOutput(ns("undo_question_text"))
          ),
          shiny$div(
            class = "confirm-buttons",
            shiny$actionButton(ns("confirm_undo"), "Yes, change back",
              class = "btn-primary", icon = shiny$icon("rotate-left")
            ),
            shiny$actionButton(ns("cancel_undo"), "Cancel")
          )
        )
      ),
      shiny$div(class = "form-success", shiny$textOutput(ns("undo_message"))),
      shiny$radioButtons(ns("matches"), "Does the expiry date on the bottle match?",
        choices = yes_no, selected = character(0), inline = TRUE
      ),
      field_errors$message_ui(ns("matches")),
      # Only when it doesn't match: the date printed on the bottle.
      shiny$conditionalPanel(
        condition = "input.matches == 'no'",
        ns = ns,
        shiny$div(
          class = "correction-box",
          # Starts empty, so the date has to be read from the bottle.
          date_field$ui(ns("bottle_expiry"), "Expiry date on the bottle"),
          field_errors$message_ui(ns("bottle_expiry")),
          shiny$p(
            class = "change-hint",
            "When you save, this date is used for this answer and corrects the fluids list",
            "for everyone. It is logged with your name."
          ),
          inputs$notes(ns("correction_note"), "Note about this correction (optional)")
        )
      )
    ),
    lot_changes$ui(ns("lot_changes"), ask_expiry = TRUE)
  )
}

# The expiry box: the date in the list, a warning if the lot had expired on the date of
# the experiment, and who corrected the date last.
expiry_box <- function(expiry, experiment_date, correction, undo_id) {
  if (expiry == "") {
    return(shiny$div(
      class = "expiry-box expired",
      "No expiry date in the list for this lot. Answer \"No\" below and enter the date on",
      "the bottle."
    ))
  }
  expired <- isTRUE(as.Date(expiry) < as.Date(experiment_date))
  shiny$tagList(
    shiny$div(
      class = paste("expiry-box", if (expired) "expired"),
      shiny$span("Expiry date in the list"),
      shiny$strong(nice_date(expiry)),
      if (expired) {
        shiny$p(
          shiny$icon("triangle-exclamation"),
          "Expired before the experiment date, so this lot can't be used. Choose another",
          "lot, or answer \"No\" below if the bottle shows a later date."
        )
      }
    ),
    if (!is.null(correction)) {
      shiny$div(
        class = "change-note",
        shiny$span(
          shiny$icon("clock-rotate-left"),
          paste0(
            "Corrected by ", changed_by_text(correction), " (was ",
            nice_date(correction$old_value), ").",
            if (correction$note != "") paste0(" Note: ", correction$note)
          )
        ),
        shiny$actionButton(undo_id, "Undo this correction", class = "btn-link")
      )
    }
  )
}

# The messages for `collect()` (NULL = no problem with that field), one per field.
# An expired lot can't be used: the expiry date that counts is the one on the bottle
# when the user corrected it, otherwise the one in the list.
lot_error <- function(lot, other_lot, expiry, experiment_date) {
  if (!shiny$isTruthy(lot)) {
    return("Please choose the lot.")
  }
  if (identical(lot, other_lot)) {
    return("This is the same lot as above. Choose the other lot that was used.")
  }
  if (expiry != "" && isTRUE(as.Date(expiry) < as.Date(experiment_date))) {
    paste0(
      "Lot ", lot, " expired on ", nice_date(expiry),
      ", before the experiment date. Choose another lot."
    )
  }
}

matches_error <- function(matches, system_expiry) {
  if (!shiny$isTruthy(matches)) {
    "Please compare the expiry date with the bottle."
  } else if (system_expiry == "" && matches == "yes") {
    "There is no date in the list to match. Answer \"No\" and enter the date on the bottle."
  }
}

bottle_error <- function(bottle, system_expiry) {
  if (!shiny$isTruthy(bottle)) {
    "Please enter the expiry date printed on the bottle."
  } else if (identical(format(bottle), system_expiry)) {
    "This is the same date as in the list. If it matches the bottle, answer \"Yes\"."
  }
}

slot_errors <- function(input, system_expiry, other_lot, experiment_date) {
  chosen <- shiny$isTruthy(input$lot)
  corrected <- chosen && identical(input$matches, "no")
  bottle <- input$bottle_expiry
  expiry <- if (corrected && shiny$isTruthy(bottle)) format(bottle) else system_expiry
  list(
    lot = lot_error(input$lot, other_lot, expiry, experiment_date),
    matches = if (chosen) matches_error(input$matches, system_expiry),
    bottle_expiry = if (corrected) bottle_error(bottle, system_expiry)
  )
}

#' - `fluid()`: the name in data/fluids.csv, e.g. "System fluid" (a reactive)
#' - `fluids()`: table with fluid, lot, expiry_date
#' - `experiment_date()`: the date of the experiment (to warn about expired lots)
#' - `changes()`: the change log
#' - `add_lot(fluid, lot, expiry_date)`, `remove_lot(fluid, lot)`: lot changes
#' - `undo_expiry(correction)`: changes a corrected expiry date back
#' - `other_lot()`: a lot this one must differ from (for a second lot), or NULL
#'
#' Returns a list:
#' - `collect(focus)`: list(lot, expiry, correction), or NULL after showing what is
#'   missing. `correction` is NULL, or list(fluid, lot, old, new, note) when the bottle date
#'   differs (it is applied only when the whole answer is saved).
#' - `reset()`: empties the slot
#' - `lot()`: the chosen lot
#' @export
server <- function(id, fluid, fluids, experiment_date, changes, add_lot, remove_lot,
                   undo_expiry, other_lot = shiny$reactive(NULL)) {
  shiny$moduleServer(id, function(input, output, session) {
    lots <- shiny$reactive(lots_for_fluid(fluids(), fluid()))
    system_expiry <- shiny$reactive(expiry_of(fluids(), fluid(), input$lot))
    field_errors$clear_on_change(input, session, c("lot", "matches", "bottle_expiry"))
    # The lot's "expired" message may no longer apply once the bottle date is entered.
    shiny$observeEvent(list(input$matches, input$bottle_expiry), ignoreInit = TRUE, {
      field_errors$show(session, list(lot = ""), focus = FALSE)
    })

    # The chosen lot, kept here so it survives when the list is refilled.
    chosen <- shiny$reactiveVal("")
    shiny$observeEvent(input$lot, chosen(input$lot))
    shiny$observeEvent(lots(), ignoreNULL = FALSE, {
      current <- shiny$isolate(chosen())
      shiny$updateSelectInput(session, "lot",
        choices = c("Choose a lot..." = "", lots()),
        selected = if (current %in% lots()) current else ""
      )
    })
    select_lot <- function(lot) {
      chosen(lot)
      shiny$updateSelectInput(session, "lot", choices = c("Choose a lot..." = "", lots()),
        selected = lot
      )
    }

    # The bottle is compared for one lot: another lot (or a changed date in the list)
    # asks again.
    clear_comparison <- function() {
      shiny$updateRadioButtons(session, "matches", selected = character(0))
      shiny$updateTextAreaInput(session, "correction_note", value = "")
      date_field$clear(session, "bottle_expiry")
    }
    shiny$observeEvent(input$lot, clear_comparison(), ignoreInit = TRUE)
    # Only when the date in the list really changes (the lists are re-read after every
    # logged change, also by colleagues). After a correction is saved the list shows
    # the bottle's date, so it now matches.
    last_expiry <- shiny$reactiveVal(NULL)
    shiny$observeEvent(system_expiry(), {
      changed <- !is.null(last_expiry()) && !identical(unname(last_expiry()), system_expiry())
      same_lot <- identical(names(last_expiry()), input$lot)
      last_expiry(setNames(system_expiry(), input$lot))
      if (!changed || !same_lot) {
        return()
      }
      if (identical(format(shiny$isolate(input$bottle_expiry)), system_expiry())) {
        shiny$updateRadioButtons(session, "matches", selected = "yes")
        date_field$clear(session, "bottle_expiry")
      } else {
        clear_comparison()
      }
    })

    # --- Expiry date, and undoing a correction ----------------------------------------
    correction <- shiny$reactive({
      shiny$req(input$lot)
      last_expiry_correction(changes(), fluid(), input$lot)
    })
    output$expiry <- shiny$renderUI({
      shiny$req(input$lot)
      expiry_box(system_expiry(), experiment_date(), correction(), session$ns("undo"))
    })

    pending_undo <- shiny$reactiveVal(FALSE)
    output$confirming_undo <- shiny$reactive(pending_undo())
    shiny$outputOptions(output, "confirming_undo", suspendWhenHidden = FALSE)
    shiny$observeEvent(input$lot, pending_undo(FALSE), ignoreInit = TRUE)
    shiny$observeEvent(input$undo, {
      undo_message("")
      pending_undo(TRUE)
      session$sendCustomMessage("focus-element", session$ns("confirm_undo"))
    })
    output$undo_question_text <- shiny$renderText({
      shiny$req(correction())
      paste0(
        "Change the expiry date of lot ", input$lot, " back to ",
        nice_date(correction()$old_value), " for everyone?"
      )
    })
    shiny$observeEvent(input$cancel_undo, pending_undo(FALSE))
    undo_message <- shiny$reactiveVal("")
    output$undo_message <- shiny$renderText(undo_message())
    shiny$observeEvent(input$confirm_undo, {
      shiny$req(pending_undo(), correction())
      undo_expiry(correction())
      pending_undo(FALSE)
      undo_message("Expiry date changed back and logged.")
    })

    # --- "Lot not listed? Add it" (the new lot is selected) ---------------------------
    added <- lot_changes$server("lot_changes",
      existing = lots,
      item = fluid,
      selected = shiny$reactive(input$lot),
      changes = changes,
      what = "fluid",
      add = add_lot,
      remove = remove_lot,
      ask_expiry = TRUE,
      experiment_date = experiment_date
    )
    shiny$observeEvent(added(), select_lot(added()))

    collect <- function(focus = TRUE) {
      errors <- slot_errors(input, system_expiry(), other_lot(), experiment_date())
      if (!field_errors$show(session, errors, focus = focus)) {
        return(NULL)
      }
      if (identical(input$matches, "no")) {
        bottle <- format(input$bottle_expiry)
        return(list(
          lot = input$lot, expiry = bottle,
          correction = list(
            fluid = fluid(), lot = input$lot, old = system_expiry(), new = bottle,
            note = inputs$clean_notes(input$correction_note)
          )
        ))
      }
      list(lot = input$lot, expiry = system_expiry(), correction = NULL)
    }

    reset <- function() {
      select_lot("")
      clear_comparison()
      field_errors$show(session, list(lot = "", matches = "", bottle_expiry = ""), focus = FALSE)
    }

    list(collect = collect, reset = reset, lot = shiny$reactive(input$lot))
  })
}
