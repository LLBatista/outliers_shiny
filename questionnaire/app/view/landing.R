# Shiny module: the landing page (who, when, first run of the day, which experiment).
box::use(
  shiny,
)

box::use(
  app/view/field_errors,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "landing-card",
    shiny$div(class = "landing-icon", shiny$icon("flask")),
    shiny$h2(id = ns("title"), "Welcome!"),
    shiny$p(class = "subtitle", "Choose your name and the date to get started."),
    shiny$selectInput(ns("name"), "Your name", choices = NULL, selectize = FALSE, width = "100%"),
    field_errors$message_ui(ns("name")),
    # No future dates: records can only be for today or earlier.
    shiny$dateInput(ns("experiment_date"), "Date",
      value = Sys.Date(), max = Sys.Date(), width = "100%"
    ),
    field_errors$message_ui(ns("experiment_date")),
    # Which instruments already had their daily check on the chosen date.
    shiny$uiOutput(ns("daily_status")),
    shiny$radioButtons(ns("first_run"), "Is this the first run of the day?",
      choices = c("Yes" = "yes", "No" = "no"), selected = character(0), inline = TRUE
    ),
    field_errors$message_ui(ns("first_run")),
    # Only shown when this is NOT the first run of the day.
    shiny$conditionalPanel(
      condition = "input.first_run == 'no'",
      ns = ns,
      shiny$selectInput(ns("experiment_type"), "Type of experiment",
        choices = NULL, selectize = FALSE, width = "100%"
      ),
      field_errors$message_ui(ns("experiment_type"))
    ),
    shiny$actionButton(ns("start"), "Start",
      class = "btn-primary", icon = shiny$icon("arrow-right")
    )
  )
}

#' `instruments`: names of all instruments.
#' `checked_on(date)`: returns the instruments that already had a daily check on
#' that date. It reads reactive data, so the list updates when a check is saved.
#' `first_run_answer()`: when it changes, "Is this the first run of the day?" is set to
#' its `answer` ("yes", "no", or NULL to clear it); with "no", focus goes to the
#' experiment list.
#' @export
server <- function(id, people, experiment_type, instruments, checked_on, first_run_answer) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "name",
      choices = c("Choose your name..." = "", people)
    )
    shiny$updateSelectInput(session, "experiment_type",
      choices = c("Choose an experiment..." = "", experiment_type)
    )

    shiny$observeEvent(first_run_answer(), ignoreInit = TRUE, {
      answer <- first_run_answer()$answer
      shiny$updateRadioButtons(session, "first_run",
        selected = if (is.null(answer)) character(0) else answer
      )
      if (identical(answer, "no")) {
        session$sendCustomMessage("focus-element", session$ns("experiment_type"))
      }
    })

    # Each message is shown under its field and removed when the field changes.
    field_errors$clear_on_change(input, session,
      c("name", "experiment_date", "first_run", "experiment_type")
    )

    submission <- shiny$eventReactive(input$start, {
      date <- input$experiment_date
      shiny$req(field_errors$show(session, list(
        name = if (input$name == "") "Please choose your name.",
        experiment_date = if (!shiny$isTruthy(date)) {
          "Please choose the date."
        } else if (date > Sys.Date()) {
          "The date can't be in the future."
        },
        first_run = if (!shiny$isTruthy(input$first_run)) {
          "Please say if this is the first run of the day."
        },
        experiment_type = if (identical(input$first_run, "no") && input$experiment_type == "") {
          "Please choose the type of experiment."
        }
      )))
      list(
        name = input$name,
        experiment_date = date,
        first_run = input$first_run == "yes",
        experiment_type = input$experiment_type
      )
    })

    # "Daily checks on 25 Sep 2026: Analyzer 01 done, Analyzer 02 not yet"
    output$daily_status <- shiny$renderUI({
      shiny$req(input$experiment_date)
      checked <- checked_on(input$experiment_date)
      items <- lapply(instruments, function(instrument) {
        done <- instrument %in% checked
        shiny$tags$li(
          class = if (done) "done" else "todo",
          shiny$icon(if (done) "circle-check" else "circle"),
          shiny$span(instrument),
          shiny$span(class = "daily-status-state", if (done) "checked" else "not checked yet")
        )
      })
      shiny$div(
        class = "daily-status",
        shiny$p(
          class = "daily-status-title",
          paste("Daily checks on", format(input$experiment_date, "%d %b %Y"))
        ),
        shiny$tags$ul(items)
      )
    })

    submission
  })
}
