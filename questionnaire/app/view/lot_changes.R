# Shiny module: changes to a lot list, shown under a lot dropdown.
# Used for the control lots (daily check) and the product lots (experiments).
#
# - If the chosen lot was added in the app: who added it and when, and a button to
#   remove it again (asks for confirmation first).
# - "Lot not listed? Add it": a small form to add a lot (asks for confirmation first,
#   because the new lot is added to the list for everyone).
# Every change is logged with the user's name and the time (see main.R).
# With `ask_expiry = TRUE` (fluids), a new lot also needs its expiry date.
box::use(
  shiny,
)

box::use(
  app/logic/changes[changed_by_text, lot_added_in_app],
  app/logic/i18n[format_date, tr],
  app/view/date_field,
  app/view/field_errors,
  app/view/inputs,
)

#' @export
ui <- function(id, ask_expiry = FALSE) {
  ns <- shiny$NS(id)
  shiny$tagList(
    # Who added the chosen lot (only for lots added in the app), and "Remove this lot".
    shiny$uiOutput(ns("note")),
    shiny$conditionalPanel(
      condition = "output.confirming_remove",
      ns = ns,
      shiny$div(
        class = "confirm-box",
        role = "alertdialog",
        `aria-labelledby` = ns("remove_question"),
        shiny$div(
          id = ns("remove_question"), class = "confirm-question",
          shiny$textOutput(ns("remove_question_text"))
        ),
        shiny$div(
          class = "confirm-buttons",
          shiny$actionButton(ns("confirm_remove"), tr("lot.yes_remove"),
            class = "btn-primary", icon = shiny$icon("trash-can")
          ),
          shiny$actionButton(ns("cancel_remove"), tr("common.cancel"))
        )
      )
    ),
    shiny$div(class = "form-success", shiny$textOutput(ns("removed_message"))),

    # <details> opens and closes by itself in the browser: no server code needed.
    shiny$tags$details(
      class = "change-box",
      shiny$tags$summary(tr("lot.not_listed")),
      inputs$limited_text(ns("new_lot"), tr("lot.new_number"), inputs$max_length$lot),
      field_errors$message_ui(ns("new_lot")),
      if (ask_expiry) {
        shiny$tagList(
          # Starts empty (not today), so the real expiry date has to be entered.
          date_field$ui(ns("new_expiry"), tr("lot.expiry")),
          field_errors$message_ui(ns("new_expiry"))
        )
      },
      inputs$notes(ns("new_note"), tr("lot.note")),
      shiny$conditionalPanel(
        condition = "!output.confirming_add",
        ns = ns,
        shiny$actionButton(ns("add"), tr("lot.add"), icon = shiny$icon("plus"))
      ),
      # Adding a lot changes the list for everyone, so it is confirmed first.
      shiny$conditionalPanel(
        condition = "output.confirming_add",
        ns = ns,
        shiny$div(
          class = "confirm-box",
          role = "alertdialog",
          `aria-labelledby` = ns("add_question"),
          shiny$div(
            id = ns("add_question"), class = "confirm-question",
            shiny$textOutput(ns("add_question_text"))
          ),
          shiny$div(
            class = "confirm-buttons",
            shiny$actionButton(ns("confirm_add"), tr("lot.yes_add"),
              class = "btn-primary", icon = shiny$icon("check")
            ),
            shiny$actionButton(ns("cancel_add"), tr("common.cancel"))
          )
        )
      ),
      shiny$div(class = "form-success", shiny$textOutput(ns("added_message")))
    )
  )
}

# The messages for "Add lot" (NULL = no problem with that field).
new_lot_errors <- function(lot, existing, ask_expiry, expiry) {
  errors <- list(
    new_lot = if (lot == "") {
      tr("lot.error_number")
    } else if (nchar(lot) > inputs$max_length$lot) {
      inputs$too_long(lot, inputs$max_length$lot)
    } else if (tolower(lot) %in% tolower(existing)) {
      # Also catches "sb2601" when "SB2601" is listed.
      tr("lot.error_exists", lot = existing[tolower(existing) == tolower(lot)][1])
    }
  )
  if (ask_expiry) {
    errors$new_expiry <- if (!shiny$isTruthy(expiry)) tr("lot.error_expiry")
  }
  errors
}

# "Add lot SB2603 (expires 31 Jan 2027) to System buffer for everyone? ..."
# A lot that had already expired on the experiment date is pointed out.
add_question <- function(lot, expiry, item, experiment_date = NULL) {
  expires <- if (!is.null(expiry)) tr("lot.expires", date = format_date(expiry))
  expired <- !is.null(expiry) && !is.null(experiment_date) &&
    isTRUE(as.Date(expiry) < as.Date(experiment_date))
  paste0(
    tr("lot.add_question", lot = paste0(lot, expires), item = item),
    if (expired) tr("lot.add_expired")
  )
}

#' Arguments (the reactives are functions to call):
#' - `existing()`: the lots in the list now
#' - `item()`: the control or product the lots belong to, e.g. "Positive Control";
#'   `item_label()`: how it is shown on screen (translated), `item()` by default
#' - `selected()`: the lot chosen in the dropdown
#' - `changes()`: the change log; `what`: "control" or "product"
#' - `add(item, lot, expiry, note)` and `remove(item, lot)`: save and log the change
#'   (from main.R); `expiry` is NULL unless `ask_expiry = TRUE`, `note` is optional text
#' - `experiment_date()`: (fluids) to point out a new lot that is already expired
#' Returns a reactive with the lot that was just added.
#' @export
server <- function(id, existing, item, selected, changes, what, add, remove,
                   ask_expiry = FALSE, experiment_date = shiny$reactive(NULL),
                   item_label = item) {
  shiny$moduleServer(id, function(input, output, session) {
    field_errors$clear_on_change(input, session, c("new_lot", "new_expiry"))

    # --- Adding a lot --------------------------------------------------------------
    # The lot waiting for "Yes, add lot" ("" when nothing is waiting).
    pending_add <- shiny$reactiveVal("")
    output$confirming_add <- shiny$reactive(pending_add() != "")
    shiny$outputOptions(output, "confirming_add", suspendWhenHidden = FALSE)

    # The expiry date waiting with it (fluids only).
    pending_expiry <- shiny$reactiveVal(NULL)
    # And the note written with it.
    pending_note <- shiny$reactiveVal("")

    shiny$observeEvent(input$add, {
      lot <- trimws(input$new_lot)
      errors <- new_lot_errors(lot, existing(), ask_expiry, input$new_expiry)
      if (field_errors$show(session, errors)) {
        pending_expiry(if (ask_expiry) input$new_expiry)
        pending_note(inputs$clean_notes(input$new_note))
        added_message("")
        pending_add(lot)
        session$sendCustomMessage("focus-element", session$ns("confirm_add"))
      }
    })

    output$add_question_text <- shiny$renderText(
      add_question(pending_add(), pending_expiry(), item_label(), experiment_date())
    )

    # Typing again, or Cancel, drops the waiting lot.
    shiny$observeEvent(input$new_lot, pending_add(""), ignoreInit = TRUE)
    shiny$observeEvent(input$new_expiry, pending_add(""), ignoreInit = TRUE)
    shiny$observeEvent(input$cancel_add, {
      pending_add("")
      session$sendCustomMessage("focus-element", session$ns("new_lot"))
    })

    # The lot is added (and logged) here, so that anything reacting to `added()`
    # already sees it in the list.
    added <- shiny$eventReactive(input$confirm_add, {
      lot <- pending_add()
      shiny$req(lot != "")
      add(item(), lot, expiry = pending_expiry(), note = pending_note())
      lot
    })

    added_message <- shiny$reactiveVal("")
    shiny$observeEvent(input$new_lot, if (input$new_lot != "") added_message(""))
    output$added_message <- shiny$renderText(added_message())

    shiny$observeEvent(added(), {
      added_message(tr("lot.added", lot = added()))
      pending_add("")
      shiny$updateTextInput(session, "new_lot", value = "")
      shiny$updateTextAreaInput(session, "new_note", value = "")
      if (ask_expiry) date_field$clear(session, "new_expiry")
    })

    # --- The chosen lot: who added it, and removing it ----------------------------
    added_by <- shiny$reactive({
      lot <- selected()
      if (!shiny$isTruthy(lot) || !shiny$isTruthy(item())) {
        return(NULL)
      }
      lot_added_in_app(changes(), what, item(), lot)
    })

    output$note <- shiny$renderUI({
      change <- added_by()
      if (is.null(change)) {
        return(NULL)
      }
      shiny$div(
        class = "change-note",
        shiny$span(
          shiny$icon("clock-rotate-left"),
          paste0(
            tr("lot.added_by", lot = change$new_value, by = changed_by_text(change)),
            if (change$note != "") paste0(" ", tr("common.note_text", note = change$note))
          )
        ),
        shiny$actionButton(session$ns("remove"), tr("lot.remove"), class = "btn-link")
      )
    })

    pending_remove <- shiny$reactiveVal("")
    output$confirming_remove <- shiny$reactive(pending_remove() != "")
    shiny$outputOptions(output, "confirming_remove", suspendWhenHidden = FALSE)
    # Choosing another lot drops a waiting removal and the "removed" message.
    shiny$observeEvent(selected(), ignoreInit = TRUE, {
      pending_remove("")
      if (shiny$isTruthy(selected())) removed_message("")
    })

    shiny$observeEvent(input$remove, {
      shiny$req(added_by())
      removed_message("")
      pending_remove(added_by()$new_value)
      session$sendCustomMessage("focus-element", session$ns("confirm_remove"))
    })
    output$remove_question_text <- shiny$renderText(paste0(
      tr("lot.remove_question", lot = pending_remove(), item = item_label())
    ))
    shiny$observeEvent(input$cancel_remove, pending_remove(""))

    removed_message <- shiny$reactiveVal("")
    output$removed_message <- shiny$renderText(removed_message())
    shiny$observeEvent(input$confirm_remove, {
      lot <- pending_remove()
      shiny$req(lot != "")
      remove(item(), lot)
      pending_remove("")
      removed_message(tr("lot.removed", lot = lot))
    })

    added
  })
}
