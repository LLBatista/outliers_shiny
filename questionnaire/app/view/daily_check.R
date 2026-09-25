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
#' - `instruments`: table with instrument, software_version, firmware_version
#' - `controls`: table with control, lot
#' - `is_already_checked(instrument)`: TRUE if this instrument was already checked today
#' - `save_check(check)`: saves the check; returns TRUE if it was saved
#' - `start()`: changes when the user starts again from the landing page
#' @export
server <- function(id, instruments, controls, is_already_checked, save_check, start) {
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
    shiny$updateSelectInput(session, "instrument",
      choices = c("Choose an instrument..." = "", instruments$instrument)
    )

    versions <- shiny$reactive({
      shiny$req(input$instrument)
      instrument_versions(instruments, input$instrument)
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
      lots = lots_for_control(controls, "Positive Control"),
      reset = reset
    )
    negative <- control_check$server("negative",
      lots = lots_for_control(controls, "Negative Control"),
      reset = reset
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
