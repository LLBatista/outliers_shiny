# Shiny module: the software and firmware versions of the chosen instrument, in the
# first step of the daily check.
#
# - Shows the versions on record.
# - If they were changed in the app: who changed them last and when, with
#   "Undo this change" (asks for confirmation first).
# - "Versions not right? Correct them here": the corrected versions are saved when the
#   user presses Confirm in daily_check.R (see `new_versions()` below).
box::use(
  shiny,
)

box::use(
  app/logic/changes[changed_by_text, last_version_change],
  app/logic/daily_checks[instrument_versions],
  app/view/field_errors,
)

# How the version fields are called in messages.
version_names <- c(software_version = "software", firmware_version = "firmware")

# The note under the versions: who changed what, e.g. "... firmware v01 to v05".
describe_last_change <- function(change) {
  what <- paste0(version_names[change$field], " ", change$old_value, " \u2192 ",
    change$new_value,
    collapse = ", "
  )
  paste0("Last changed by ", changed_by_text(change), ": ", what, ".")
}

# TRUE when typed versions differ from the ones on record (both fields empty counts
# as "no change").
versions_differ <- function(typed, on_record) {
  if (typed$software == "" && typed$firmware == "") {
    return(FALSE)
  }
  typed$software != on_record$software_version || typed$firmware != on_record$firmware_version
}

# "software v01 and firmware v01": the versions an undo goes back to.
describe_old_versions <- function(change) {
  paste(version_names[change$field], change$old_value, collapse = " and ")
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$uiOutput(ns("versions")),
    shiny$uiOutput(ns("history")),
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
    # Only once an instrument is chosen.
    shiny$conditionalPanel(
      condition = "output.has_instrument",
      ns = ns,
      shiny$tags$details(
        class = "change-box",
        shiny$tags$summary("Versions not right? Correct them here"),
        shiny$p(
          class = "change-hint",
          "The corrected versions are saved when you press Confirm. They apply for",
          "everyone and are logged with your name."
        ),
        shiny$textInput(ns("software"), "Software version", width = "100%"),
        field_errors$message_ui(ns("software")),
        shiny$textInput(ns("firmware"), "Firmware version", width = "100%"),
        field_errors$message_ui(ns("firmware"))
      )
    )
  )
}

#' - `instrument()`: the chosen instrument ("" when none)
#' - `instruments()`: table with instrument, software_version, firmware_version
#' - `changes()`: the change log
#' - `undo_versions(change)`: changes the versions back (`change`: rows of the change log)
#'
#' Returns a list:
#' - `versions()`: the versions on record for the chosen instrument
#' - `edited()`: TRUE when the user typed versions that differ from the ones on record
#' - `new_versions()`: the typed versions, or NULL after showing what is missing
#' - `reset()`: clears messages and questions (when starting a new check)
#' @export
server <- function(id, instrument, instruments, changes, undo_versions) {
  shiny$moduleServer(id, function(input, output, session) {
    output$has_instrument <- shiny$reactive(shiny$isTruthy(instrument()))
    shiny$outputOptions(output, "has_instrument", suspendWhenHidden = FALSE)
    field_errors$clear_on_change(input, session, c("software", "firmware"))

    versions <- shiny$reactive({
      shiny$req(instrument())
      instrument_versions(instruments(), instrument())
    })

    output$versions <- shiny$renderUI({
      v <- versions()
      shiny$div(
        class = "version-box",
        shiny$div(shiny$span("Software version"), shiny$strong(v$software_version)),
        shiny$div(shiny$span("Firmware version"), shiny$strong(v$firmware_version))
      )
    })

    # The form starts with the versions on record. It is filled again only when those
    # change (another instrument, or a correction), not on every re-read of the lists.
    prefilled <- shiny$reactiveVal(NULL)
    shiny$observeEvent(versions(), {
      v <- versions()
      if (!identical(v, prefilled())) {
        prefilled(v)
        shiny$updateTextInput(session, "software", value = v$software_version)
        shiny$updateTextInput(session, "firmware", value = v$firmware_version)
      }
    })

    typed <- shiny$reactive(list(
      software = trimws(paste0(input$software, "")),
      firmware = trimws(paste0(input$firmware, ""))
    ))

    edited <- shiny$reactive({
      shiny$isTruthy(instrument()) && versions_differ(typed(), versions())
    })

    new_versions <- function() {
      t <- typed()
      ok <- field_errors$show(session, list(
        software = if (t$software == "") "Please fill in the software version.",
        firmware = if (t$firmware == "") "Please fill in the firmware version."
      ))
      if (ok) t else NULL
    }

    # --- Who changed the versions last, and undoing that change --------------------
    last_change <- shiny$reactive({
      if (!shiny$isTruthy(instrument())) {
        return(NULL)
      }
      last_version_change(changes(), instrument())
    })

    output$history <- shiny$renderUI({
      change <- last_change()
      if (is.null(change)) {
        return(NULL)
      }
      shiny$div(
        class = "change-note",
        shiny$span(shiny$icon("clock-rotate-left"), describe_last_change(change)),
        shiny$actionButton(session$ns("undo"), "Undo this change", class = "btn-link")
      )
    })

    pending_undo <- shiny$reactiveVal(FALSE)
    output$confirming_undo <- shiny$reactive(pending_undo())
    shiny$outputOptions(output, "confirming_undo", suspendWhenHidden = FALSE)
    shiny$observeEvent(instrument(), pending_undo(FALSE), ignoreInit = TRUE)

    shiny$observeEvent(input$undo, {
      shiny$req(last_change())
      undo_message("")
      pending_undo(TRUE)
      session$sendCustomMessage("focus-element", session$ns("confirm_undo"))
    })
    output$undo_question_text <- shiny$renderText({
      change <- last_change()
      shiny$req(change)
      paste0("Change ", instrument(), " back to ", describe_old_versions(change), " for everyone?")
    })
    shiny$observeEvent(input$cancel_undo, pending_undo(FALSE))

    undo_message <- shiny$reactiveVal("")
    output$undo_message <- shiny$renderText(undo_message())
    shiny$observeEvent(input$confirm_undo, {
      change <- last_change()
      shiny$req(pending_undo(), change)
      undo_versions(change)
      pending_undo(FALSE)
      undo_message("Versions changed back and logged.")
    })

    reset <- function() {
      field_errors$show(session, list(software = "", firmware = ""), focus = FALSE)
      pending_undo(FALSE)
      undo_message("")
    }

    list(versions = versions, edited = edited, new_versions = new_versions, reset = reset)
  })
}
