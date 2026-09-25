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
    shiny$selectInput(ns("name"), "Your name", choices = NULL, width = "100%"),
    shiny$dateInput(ns("experiment_date"), "Experiment date", value = Sys.Date(), width = "100%"),
    shiny$radioButtons(ns("first_run"), "Is this the first run of the day?",
                       choices = c("Yes" = "yes", "No" = "no"), selected = character(0), inline = TRUE
    ),
    shiny$conditionalPanel(
      condition = "input.first_run == 'no'",
      ns = ns,
      shiny$selectInput(ns("experiment_type"), "Type of experiment", choices = NULL, width = "100%")
    ),    shiny$actionButton(ns("start"), "Start", class = "btn-primary", icon = shiny$icon("arrow-right"))
  )
}

#' @export
server <- function(id, people, experiment_type) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session,
      "name",
      choices = c(
        "Choose your name..." = "",
        people
      )
    )
    shiny$updateSelectInput(session,
      "experiment_type",
      choices = c(
        "Choose an experiment..." = "",
        experiment_type
      )
    )

    shiny$eventReactive(input$start, {
      shiny$validate(
        shiny$need(shiny$isTruthy(input$first_run), "Please say if this is the first run of the day."),
        shiny$need(
          input$first_run == "yes" || input$experiment_type != "",
          "Please choose your experiment type."
        ),
        shiny$need(
          input$name != "",
          "Please choose your name"
        ),
      )
      list(
        name = input$name,
        experiment_date = input$experiment_date,
        first_run = input$first_run == "yes",
        experiment_type = input$experiment_type
      )
    })
  })
}
