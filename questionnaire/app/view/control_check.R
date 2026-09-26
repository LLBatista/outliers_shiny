# Shiny module: one control of the daily check (its lot, and whether it was valid).
# Used twice by daily_check.R: once for the positive and once for the negative control.
box::use(
  shiny,
  stats[setNames],
)

box::use(
  app/logic/i18n[tr],
  app/view/field_errors,
  app/view/inputs,
  app/view/lot_changes,
)

#' `title` is shown above the form, `next_label` on the main button.
#' @export
ui <- function(id, title, next_label) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$h3(id = ns("title"), class = "step-title", title),
    shiny$selectInput(ns("lot"), tr("common.lot"),
      choices = NULL, selectize = FALSE, width = "100%"
    ),
    field_errors$message_ui(ns("lot")),
    lot_changes$ui(ns("lot_changes")),
    shiny$radioButtons(ns("valid"), tr("control.valid"),
      choices = inputs$yes_no(), selected = character(0), inline = TRUE
    ),
    field_errors$message_ui(ns("valid")),
    # Only asked when the control was not valid.
    shiny$conditionalPanel(
      condition = "input.valid == 'no'",
      ns = ns,
      shiny$radioButtons(ns("rerun_valid"), tr("control.rerun_valid"),
        choices = inputs$yes_no(), selected = character(0), inline = TRUE
      ),
      field_errors$message_ui(ns("rerun_valid"))
    ),
    inputs$notes(ns("notes"), tr("control.notes")),
    shiny$div(
      class = "step-buttons",
      shiny$actionButton(ns("back"), tr("common.back"), icon = shiny$icon("arrow-left")),
      shiny$actionButton(ns("done"), next_label,
        class = "btn-primary", icon = shiny$icon("arrow-right")
      )
    )
  )
}

#' - `control`: the control's name in data/controls.csv, e.g. "Positive Control";
#'   `label`: how it is shown, e.g. "Positivkontrolle"
#' - `lots()`: the lots to choose from (a reactive: it changes when a lot is added)
#' - `reset()`: when it changes, the answers are cleared
#' - `changes()`: the change log (to show who added a lot)
#' - `add_lot(control, lot)`, `remove_lot(control, lot)`: save and log a lot change
#'
#' Returns a list with two reactives:
#' - `result`: the answers (lot, valid, rerun_valid, notes), after the main button is clicked
#' - `back`: changes when the Back button is clicked
#' @export
server <- function(id, control, label, lots, reset, changes, add_lot, remove_lot) {
  shiny$moduleServer(id, function(input, output, session) {
    # The lot the user chose, kept here so it survives when the list is refreshed.
    chosen_lot <- shiny$reactiveVal("")
    shiny$observeEvent(input$lot, chosen_lot(input$lot))

    # Fill the list, and refill it whenever it changes (e.g. a colleague added a lot).
    shiny$observeEvent(lots(), ignoreNULL = FALSE, {
      current <- shiny$isolate(chosen_lot())
      shiny$updateSelectInput(session, "lot",
        choices = c(setNames("", tr("common.choose_lot")), lots()),
        selected = if (current %in% lots()) current else ""
      )
    })

    # Who added the chosen lot, "Remove this lot" and "Lot not listed? Add it".
    # Once added, the new lot is selected.
    added <- lot_changes$server("lot_changes",
      existing = lots,
      item = shiny$reactive(control),
      item_label = shiny$reactive(label),
      selected = shiny$reactive(input$lot),
      changes = changes,
      what = "control",
      add = add_lot,
      remove = remove_lot
    )
    shiny$observeEvent(added(), {
      chosen_lot(added())
      shiny$updateSelectInput(session, "lot",
        choices = c(setNames("", tr("common.choose_lot")), lots()),
        selected = added()
      )
    })

    # Each message is shown under its field and removed when the field changes.
    fields <- c("lot", "valid", "rerun_valid")
    field_errors$clear_on_change(input, session, fields)

    shiny$observeEvent(reset(),
      {
        field_errors$show(session, as.list(setNames(rep("", length(fields)), fields)),
          focus = FALSE
        )
        chosen_lot("")
        shiny$updateSelectInput(session, "lot", selected = "")
        shiny$updateRadioButtons(session, "valid", selected = character(0))
        shiny$updateRadioButtons(session, "rerun_valid", selected = character(0))
        shiny$updateTextAreaInput(session, "notes", value = "")
      },
      ignoreInit = TRUE
    )

    result <- shiny$eventReactive(input$done, {
      ok <- field_errors$show(session, list(
        lot = if (!shiny$isTruthy(input$lot)) tr("common.error_lot"),
        valid = if (!shiny$isTruthy(input$valid)) tr("control.error_valid"),
        rerun_valid = if (identical(input$valid, "no") && !shiny$isTruthy(input$rerun_valid)) {
          tr("control.error_rerun")
        }
      ))
      shiny$req(ok)
      list(
        lot = input$lot,
        valid = input$valid,
        rerun_valid = if (input$valid == "yes") "not needed" else input$rerun_valid,
        notes = inputs$clean_notes(input$notes)
      )
    })

    list(result = result, back = shiny$reactive(input$back))
  })
}
