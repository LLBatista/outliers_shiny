# Shiny module: changes to a lot list, shown under a lot dropdown.
# Used for the control lots (daily check) and the product lots (experiments).
#
# - If the chosen lot was added in the app: who added it and when, and a button to
#   remove it again (asks for confirmation first).
# - "Lot not listed? Add it": a small form to add a lot (asks for confirmation first,
#   because the new lot is added to the list for everyone).
# Every change is logged with the user's name and the time (see main.R).
box::use(
  shiny,
)

box::use(
  app/logic/changes[changed_by_text, lot_added_in_app],
  app/view/field_errors,
)

#' @export
ui <- function(id) {
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
          shiny$actionButton(ns("confirm_remove"), "Yes, remove lot",
            class = "btn-primary", icon = shiny$icon("trash-can")
          ),
          shiny$actionButton(ns("cancel_remove"), "Cancel")
        )
      )
    ),
    shiny$div(class = "form-success", shiny$textOutput(ns("removed_message"))),

    # <details> opens and closes by itself in the browser: no server code needed.
    shiny$tags$details(
      class = "change-box",
      shiny$tags$summary("Lot not listed? Add it"),
      shiny$textInput(ns("new_lot"), "New lot number", width = "100%"),
      field_errors$message_ui(ns("new_lot")),
      shiny$conditionalPanel(
        condition = "!output.confirming_add",
        ns = ns,
        shiny$actionButton(ns("add"), "Add lot", icon = shiny$icon("plus"))
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
            shiny$actionButton(ns("confirm_add"), "Yes, add lot",
              class = "btn-primary", icon = shiny$icon("check")
            ),
            shiny$actionButton(ns("cancel_add"), "Cancel")
          )
        )
      ),
      shiny$div(class = "form-success", shiny$textOutput(ns("added_message")))
    )
  )
}

#' Arguments (the reactives are functions to call):
#' - `existing()`: the lots in the list now
#' - `item()`: the control or product the lots belong to, e.g. "Positive Control"
#' - `selected()`: the lot chosen in the dropdown
#' - `changes()`: the change log; `what`: "control" or "product"
#' - `add(item, lot)` and `remove(item, lot)`: save and log the change (from main.R)
#' Returns a reactive with the lot that was just added.
#' @export
server <- function(id, existing, item, selected, changes, what, add, remove) {
  shiny$moduleServer(id, function(input, output, session) {
    field_errors$clear_on_change(input, session, "new_lot")

    # --- Adding a lot --------------------------------------------------------------
    # The lot waiting for "Yes, add lot" ("" when nothing is waiting).
    pending_add <- shiny$reactiveVal("")
    output$confirming_add <- shiny$reactive(pending_add() != "")
    shiny$outputOptions(output, "confirming_add", suspendWhenHidden = FALSE)

    shiny$observeEvent(input$add, {
      lot <- trimws(input$new_lot)
      error <- if (lot == "") {
        "Please type the lot number."
      } else if (lot %in% existing()) {
        paste("Lot", lot, "is already in the list.")
      } else {
        ""
      }
      if (field_errors$show(session, list(new_lot = error))) {
        added_message("")
        pending_add(lot)
        session$sendCustomMessage("focus-element", session$ns("confirm_add"))
      }
    })

    output$add_question_text <- shiny$renderText(paste0(
      "Add lot ", pending_add(), " to ", item(), " for everyone? ",
      "It will be logged with your name."
    ))

    # Typing again, or Cancel, drops the waiting lot.
    shiny$observeEvent(input$new_lot, pending_add(""), ignoreInit = TRUE)
    shiny$observeEvent(input$cancel_add, {
      pending_add("")
      session$sendCustomMessage("focus-element", session$ns("new_lot"))
    })

    # The lot is added (and logged) here, so that anything reacting to `added()`
    # already sees it in the list.
    added <- shiny$eventReactive(input$confirm_add, {
      lot <- pending_add()
      shiny$req(lot != "")
      add(item(), lot)
      lot
    })

    added_message <- shiny$reactiveVal("")
    shiny$observeEvent(input$new_lot, if (input$new_lot != "") added_message(""))
    output$added_message <- shiny$renderText(added_message())

    shiny$observeEvent(added(), {
      added_message(paste("Lot", added(), "added and logged."))
      pending_add("")
      shiny$updateTextInput(session, "new_lot", value = "")
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
            "Lot ", change$new_value, " was added in the app by ", changed_by_text(change), "."
          )
        ),
        shiny$actionButton(session$ns("remove"), "Remove this lot", class = "btn-link")
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
      "Remove lot ", pending_remove(), " from ", item(), " for everyone? ",
      "Records that already use it are kept."
    ))
    shiny$observeEvent(input$cancel_remove, pending_remove(""))

    removed_message <- shiny$reactiveVal("")
    output$removed_message <- shiny$renderText(removed_message())
    shiny$observeEvent(input$confirm_remove, {
      lot <- pending_remove()
      shiny$req(lot != "")
      remove(item(), lot)
      pending_remove("")
      removed_message(paste("Lot", lot, "removed and logged."))
    })

    added
  })
}
