# Shiny module: the first-run-of-the-day check, one step at a time:
#   1. instrument (shows its software and firmware version) -> Confirm
#   2. positive control -> Next
#   3. negative control -> Save daily check
#   done: a summary, and a button to check another instrument
box::use(
  shiny,
)

box::use(
  app/logic/daily_checks[instrument_versions, lots_for_control],
  app/view/control_check,
)

# The steps shown in the progress bar at the top, in order.
steps <- c(instrument = "Instrument", positive = "Positive control", negative = "Negative control")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card daily-check",
    shiny$h3(shiny$icon("clipboard-check"), "First run of the day"),
    shiny$uiOutput(ns("progress")),

    # One hidden tab per step; the server decides which one is shown.
    shiny$tabsetPanel(
      id = ns("steps"),
      type = "hidden",

      # --- Step 1: instrument ------------------------------------------------
      shiny$tabPanel(
        "instrument",
        shiny$h4(
          id = ns("instrument_title"), class = "step-title",
          "Which instrument are you checking?"
        ),
        shiny$selectInput(ns("instrument"), "Instrument",
          choices = NULL, selectize = FALSE, width = "100%"
        ),
        shiny$uiOutput(ns("versions")),
        # "Versions not right?": only once an instrument is chosen.
        shiny$conditionalPanel(
          condition = "input.instrument != ''",
          ns = ns,
          shiny$tags$details(
            class = "change-box",
            shiny$tags$summary("Versions not right? Enter the correct ones"),
            shiny$textInput(ns("new_software"), "Software version", width = "100%"),
            shiny$textInput(ns("new_firmware"), "Firmware version", width = "100%"),
            shiny$div(class = "form-message", shiny$textOutput(ns("versions_message"))),
            shiny$actionButton(ns("save_versions"), "Save versions", icon = shiny$icon("check")),
            shiny$div(class = "form-success", shiny$textOutput(ns("versions_saved")))
          )
        ),
        shiny$div(class = "form-message", shiny$textOutput(ns("instrument_message"))),
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
          shiny$actionButton(ns("another"), "Check another instrument", icon = shiny$icon("plus"))
        )
      )
    )
  )
}

#' Arguments:
#' - `instruments()`: table with instrument, software_version, firmware_version (reactive,
#'   it changes when versions are corrected)
#' - `controls()`: table with control, lot (reactive, it grows when a lot is added)
#' - `is_already_checked(instrument)`: TRUE if this instrument was already checked today
#' - `save_check(check)`: saves the check; returns TRUE if it was saved
#' - `start()`: changes when the user starts again from the landing page
#' - `change_versions(instrument, software, firmware)`: saves and logs corrected versions
#' - `add_control_lot(control, lot)`: saves and logs a control lot that was not listed
#' @export
server <- function(id, instruments, controls, is_already_checked, save_check, start,
                   change_versions, add_control_lot) {
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
      show_instrument_message(FALSE)
      shiny$updateSelectInput(session, "instrument", selected = "")
      reset(reset() + 1)
      go_to("instrument", focus = focus)
    }
    # Coming from the landing page, focus goes to the page title instead (main.R).
    shiny$observeEvent(start(), start_over(focus = FALSE))
    shiny$observeEvent(input$another, start_over())

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

    versions <- shiny$reactive({
      shiny$req(input$instrument)
      instrument_versions(instruments(), input$instrument)
    })

    # "Versions not right?": the form starts with the current versions.
    shiny$observeEvent(input$instrument, {
      shiny$req(input$instrument)
      v <- versions()
      shiny$updateTextInput(session, "new_software", value = v$software_version)
      shiny$updateTextInput(session, "new_firmware", value = v$firmware_version)
      versions_saved("")
    })

    show_versions_message <- shiny$reactiveVal(FALSE)
    shiny$observeEvent(input$save_versions, show_versions_message(TRUE))
    shiny$observeEvent(list(input$new_software, input$new_firmware), show_versions_message(FALSE),
      ignoreInit = TRUE
    )

    new_versions <- shiny$eventReactive(input$save_versions, {
      software <- trimws(input$new_software)
      firmware <- trimws(input$new_firmware)
      v <- versions()
      shiny$validate(
        shiny$need(software != "" && firmware != "", "Please fill in both versions."),
        shiny$need(
          software != v$software_version || firmware != v$firmware_version,
          "These are the versions already on record."
        )
      )
      list(software = software, firmware = firmware)
    })

    output$versions_message <- shiny$renderText({
      if (show_versions_message()) new_versions()
      ""
    })

    versions_saved <- shiny$reactiveVal("")
    output$versions_saved <- shiny$renderText(versions_saved())
    shiny$observeEvent(new_versions(), {
      change_versions(input$instrument, new_versions()$software, new_versions()$firmware)
      versions_saved("New versions saved and logged.")
    })

    output$versions <- shiny$renderUI({
      v <- versions()
      shiny$div(
        class = "version-box",
        shiny$div(shiny$span("Software version"), shiny$strong(v$software_version)),
        shiny$div(shiny$span("Firmware version"), shiny$strong(v$firmware_version))
      )
    })

    # Error messages: shown after Confirm, hidden again when the instrument changes.
    show_instrument_message <- shiny$reactiveVal(FALSE)
    shiny$observeEvent(input$confirm_instrument, show_instrument_message(TRUE))
    shiny$observeEvent(input$instrument, show_instrument_message(FALSE), ignoreInit = TRUE)

    instrument <- shiny$eventReactive(input$confirm_instrument, {
      shiny$validate(
        shiny$need(input$instrument != "", "Please choose an instrument."),
        shiny$need(
          !is_already_checked(input$instrument),
          paste("A daily check for", input$instrument, "was already saved on this date.")
        )
      )
      c(list(instrument = input$instrument), versions())
    })

    output$instrument_message <- shiny$renderText({
      if (show_instrument_message()) instrument()
      ""
    })

    shiny$observeEvent(instrument(), go_to("positive"))

    # ------------------------------------------------------------------------
    # Steps 2 and 3: the controls
    # ------------------------------------------------------------------------
    positive <- control_check$server("positive",
      lots = shiny$reactive(lots_for_control(controls(), "Positive Control")),
      reset = reset,
      add_new_lot = function(lot) add_control_lot("Positive Control", lot)
    )
    negative <- control_check$server("negative",
      lots = shiny$reactive(lots_for_control(controls(), "Negative Control")),
      reset = reset,
      add_new_lot = function(lot) add_control_lot("Negative Control", lot)
    )

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
        shiny$h4(
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
