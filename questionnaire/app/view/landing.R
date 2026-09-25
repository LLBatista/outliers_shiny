# Shiny module: the landing page (who, when, first run of the day, which experiment).
box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "landing-card",
    shiny$div(class = "landing-icon", shiny$icon("flask")),
    shiny$h2("Welcome!"),
    shiny$p(class = "subtitle", "Tell us about your experiment to get started."),
    shiny$selectInput(ns("name"), "Your name", choices = NULL, selectize = FALSE, width = "100%"),
    # No future dates: records can only be for today or earlier.
    shiny$dateInput(ns("experiment_date"), "Experiment date",
      value = Sys.Date(), max = Sys.Date(), width = "100%"
    ),
    shiny$radioButtons(ns("first_run"), "Is this the first run of the day?",
      choices = c("Yes" = "yes", "No" = "no"), selected = character(0), inline = TRUE
    ),
    # Only shown when this is NOT the first run of the day.
    shiny$conditionalPanel(
      condition = "input.first_run == 'no'",
      ns = ns,
      shiny$selectInput(ns("experiment_type"), "Type of experiment",
        choices = NULL, selectize = FALSE, width = "100%"
      )
    ),
    shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
    shiny$actionButton(ns("start"), "Start",
      class = "btn-primary", icon = shiny$icon("arrow-right")
    )
  )
}

#' @export
server <- function(id, people, experiment_type) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "name",
      choices = c("Choose your name..." = "", people)
    )
    shiny$updateSelectInput(session, "experiment_type",
      choices = c("Choose an experiment..." = "", experiment_type)
    )

    # Error messages: shown after a click on Start, hidden again when an answer changes.
    show_message <- shiny$reactiveVal(FALSE)
    shiny$observeEvent(input$start, show_message(TRUE))
    shiny$observeEvent(
      list(input$name, input$experiment_date, input$first_run, input$experiment_type),
      show_message(FALSE),
      ignoreInit = TRUE
    )

    submission <- shiny$eventReactive(input$start, {
      shiny$validate(
        shiny$need(input$name != "", "Please choose your name."),
        shiny$need(
          shiny$isTruthy(input$experiment_date),
          "Please choose the experiment date."
        ),
        shiny$need(
          !shiny$isTruthy(input$experiment_date) || input$experiment_date <= Sys.Date(),
          "The experiment date can't be in the future."
        ),
        shiny$need(
          shiny$isTruthy(input$first_run),
          "Please say if this is the first run of the day."
        ),
        shiny$need(
          !identical(input$first_run, "no") || input$experiment_type != "",
          "Please choose the type of experiment."
        )
      )
      list(
        name = input$name,
        experiment_date = input$experiment_date,
        first_run = input$first_run == "yes",
        experiment_type = input$experiment_type
      )
    })

    output$message <- shiny$renderText({
      if (show_message()) submission()
      ""
    })

    submission
  })
}
