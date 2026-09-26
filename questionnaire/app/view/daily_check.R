# Shiny module: the first-run-of-the-day check, one step at a time:
#   1. instrument (shows its software and firmware version) -> Confirm
#   2. positive control -> Next
#   3. negative control -> Save daily check
#   done: a summary, and a button to check another instrument
box::use(
  shiny,
)

box::use(
  app/logic/daily_checks[lots_for_control],
  app/view/control_check,
  app/view/field_errors,
  app/view/version_changes,
)

# The steps shown in the progress bar at the top, in order.
steps <- c(instrument = "Instrument", positive = "Positive control", negative = "Negative control")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card daily-check",
    shiny$uiOutput(ns("progress")),

    # One hidden tab per step; the server decides which one is shown.
    shiny$tabsetPanel(
      id = ns("steps"),
      type = "hidden",

      # --- Step 1: instrument ------------------------------------------------
      shiny$tabPanel(
        "instrument",
        shiny$h3(
          id = ns("instrument_title"), class = "step-title",
          "Which instrument are you checking?"
        ),
        shiny$selectInput(ns("instrument"), "Instrument",
          choices = NULL, selectize = FALSE, width = "100%"
        ),
        field_errors$message_ui(ns("instrument")),
        # The versions on record, who changed them, and "Versions not right?".
        version_changes$ui(ns("versions")),
        shiny$div(
          class = "step-buttons",
          shiny$actionButton(ns("confirm_instrument"), "Confirm",
            class = "btn-primary", icon = shiny$icon("check")
          )
        )
      ),

      # --- Steps 2 and 3: the controls (same module, used twice) ----------------
      shiny$tabPanel(
        "positive",
        control_check$ui(ns("positive"), title = "Positive control", next_label = "Next")
      ),
      shiny$tabPanel(
        "negative",
        control_check$ui(ns("negative"),
          title = "Negative control",
          next_label = "Save daily check"
        )
      ),

      # --- Done ----------------------------------------------------------------
      shiny$tabPanel(
        "done",
        # role = "status": screen readers read the summary out when it appears.
        shiny$div(role = "status", shiny$uiOutput(ns("summary"))),
        shiny$div(
          class = "step-buttons",
          shiny$actionButton(ns("another"), "Check another instrument", icon = shiny$icon("plus")),
          shiny$actionButton(ns("continue"), "Continue to an experiment",
            class = "btn-primary", icon = shiny$icon("arrow-right")
          )
        )
      )
    )
  )
}

#' Arguments:
#' - `instruments()`: table with instrument, software_version, firmware_version (reactive,
#'   it changes when versions are corrected)
#' - `controls()`: table with control, lot (reactive, it changes when a lot is added)
#' - `changes()`: the change log (to show who changed what)
#' - `is_already_checked(instrument)`: TRUE if this instrument was already checked today
#' - `save_check(check)`: saves the check; returns TRUE if it was saved
#' - `start()`: changes when the user starts again from the landing page
#' - `change_versions(instrument, software, firmware)`: saves and logs corrected versions
#' - `undo_versions(change)`: changes the versions back (`change`: rows of the change log)
#' - `add_control_lot(control, lot)`, `remove_control_lot(control, lot)`: lot changes
#' - `continue_to_experiment()`: goes to the landing page to choose an experiment
#' @export
server <- function(id, instruments, controls, changes, is_already_checked, save_check, start,
                   change_versions, undo_versions, add_control_lot, remove_control_lot,
                   continue_to_experiment) {
  shiny$moduleServer(id, function(input, output, session) {
    # ------------------------------------------------------------------------
    # Moving between steps
    # ------------------------------------------------------------------------
    current_step <- shiny$reactiveVal("instrument")
    # The heading of each step; focus moves there so keyboard and screen-reader
    # users continue at the new step (see "focus-element" in main.R).
    step_headings <- c(
      instrument = "instrument_title", positive = "positive-title",
      negative = "negative-title", done = "done_title"
    )
    go_to <- function(step, focus = TRUE) {
      current_step(step)
      shiny$updateTabsetPanel(session, "steps", selected = step)
      if (focus) session$sendCustomMessage("focus-element", session$ns(step_headings[[step]]))
    }

    # Changing `reset` clears the answers in both control steps.
    reset <- shiny$reactiveVal(0)
    start_over <- function(focus = TRUE) {
      field_errors$show(session, list(instrument = ""), focus = FALSE)
      versions$reset()
      shiny$updateSelectInput(session, "instrument", selected = "")
      reset(reset() + 1)
      go_to("instrument", focus = focus)
    }
    # Coming from the landing page, focus goes to the page title instead (main.R).
    shiny$observeEvent(start(), start_over(focus = FALSE))
    shiny$observeEvent(input$another, start_over())
    shiny$observeEvent(input$continue, continue_to_experiment())

    # The progress bar: done steps get a tick, the current one is highlighted.
    output$progress <- shiny$renderUI({
      position <- match(current_step(), c(names(steps), "done"))
      items <- lapply(seq_along(steps), function(i) {
        state <- if (i < position) "done" else if (i == position) "current" else "todo"
        marker <- if (state == "done") shiny$icon("check") else i
        shiny$div(
          class = paste("step", state),
          shiny$span(class = "step-marker", marker),
          shiny$span(steps[[i]])
        )
      })
      shiny$div(class = "step-progress", items)
    })

    # ------------------------------------------------------------------------
    # Step 1: instrument
    # ------------------------------------------------------------------------
    # The list of instruments itself doesn't change, only their versions.
    shiny$updateSelectInput(session, "instrument",
      choices = c("Choose an instrument..." = "", shiny$isolate(instruments()$instrument))
    )
    field_errors$clear_on_change(input, session, "instrument")

    versions <- version_changes$server("versions",
      instrument = shiny$reactive(input$instrument),
      instruments = instruments,
      changes = changes,
      undo_versions = undo_versions
    )

    # The button says what will happen.
    shiny$observeEvent(versions$edited(), {
      shiny$updateActionButton(session, "confirm_instrument",
        label = if (versions$edited()) "Confirm with new versions" else "Confirm"
      )
    })

    # Confirm: checks the instrument, and saves corrected versions (if any) first.
    instrument <- shiny$eventReactive(input$confirm_instrument, {
      chosen <- input$instrument
      shiny$req(field_errors$show(session, list(
        instrument = if (!shiny$isTruthy(chosen)) {
          "Please choose an instrument."
        } else if (is_already_checked(chosen)) {
          paste("A daily check for", chosen, "was already saved on this date.")
        }
      )))
      if (versions$edited()) {
        typed <- versions$new_versions()
        shiny$req(typed)
        change_versions(chosen, typed$software, typed$firmware)
        return(list(
          instrument = chosen, software_version = typed$software, firmware_version = typed$firmware
        ))
      }
      c(list(instrument = chosen), versions$versions())
    })

    shiny$observeEvent(instrument(), go_to("positive"))

    # ------------------------------------------------------------------------
    # Steps 2 and 3: the controls
    # ------------------------------------------------------------------------
    control_step <- function(step, control) {
      control_check$server(step,
        control = control,
        lots = shiny$reactive(lots_for_control(controls(), control)),
        reset = reset,
        changes = changes,
        add_lot = add_control_lot,
        remove_lot = remove_control_lot
      )
    }
    positive <- control_step("positive", "Positive Control")
    negative <- control_step("negative", "Negative Control")

    shiny$observeEvent(positive$back(), go_to("instrument"), ignoreInit = TRUE)
    shiny$observeEvent(positive$result(), go_to("negative"))
    shiny$observeEvent(negative$back(), go_to("positive"), ignoreInit = TRUE)

    # After the negative control: save everything, then show the summary.
    saved_check <- shiny$reactiveVal(NULL)
    shiny$observeEvent(negative$result(), {
      check <- c(instrument(), list(positive = positive$result(), negative = negative$result()))
      if (save_check(check)) {
        saved_check(check)
        go_to("done")
      }
    })

    # ------------------------------------------------------------------------
    # Done: summary of what was saved
    # ------------------------------------------------------------------------
    describe_control <- function(control) {
      if (control$valid == "yes") {
        "valid"
      } else {
        paste("not valid, rerun", if (control$rerun_valid == "yes") "valid" else "not valid")
      }
    }

    output$summary <- shiny$renderUI({
      check <- saved_check()
      shiny$req(check)
      shiny$div(
        class = "done-summary",
        shiny$h3(
          id = session$ns("done_title"), class = "step-title",
          shiny$icon("circle-check"), "Daily check saved"
        ),
        shiny$tags$dl(
          shiny$tags$dt("Instrument"),
          shiny$tags$dd(paste0(
            check$instrument, " (software ", check$software_version,
            ", firmware ", check$firmware_version, ")"
          )),
          shiny$tags$dt("Positive control"),
          shiny$tags$dd(paste0("Lot ", check$positive$lot, ": ", describe_control(check$positive))),
          shiny$tags$dt("Negative control"),
          shiny$tags$dd(paste0("Lot ", check$negative$lot, ": ", describe_control(check$negative)))
        )
      )
    })
  })
}
