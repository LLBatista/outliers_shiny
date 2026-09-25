# Shiny module: the landing page (who, when, first run of the day, which experiment).
box::use(
  shiny,
  shiny.fluent[
    ChoiceGroup.shinyInput, DatePicker.shinyInput, Dropdown.shinyInput, PrimaryButton.shinyInput,
    Stack, Text
  ],
)

box::use(
  app/logic/fluent_helpers[as_local_date, as_options],
)

#' @export
ui <- function(id, people, experiment_types) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "landing-card",
    Stack(
      tokens = list(childrenGap = 16),
      shiny$div(class = "landing-icon", shiny$icon("flask")),
      Text(variant = "xxLarge", class = "landing-title", "Welcome!"),
      Text(class = "subtitle", "Tell us about your experiment to get started."),
      Dropdown.shinyInput(
        ns("name"),
        label = "Your name",
        placeholder = "Choose your name...",
        options = as_options(people)
      ),
      DatePicker.shinyInput(
        ns("experiment_date"),
        label = "Experiment date",
        value = Sys.Date()
      ),
      ChoiceGroup.shinyInput(
        ns("first_run"),
        label = "Is this the first run of the day?",
        options = list(list(key = "yes", text = "Yes"), list(key = "no", text = "No")),
        styles = list(flexContainer = list(display = "flex", gap = "24px"))
      ),
      # Only shown when this is NOT the first run of the day.
      shiny$conditionalPanel(
        condition = "input.first_run == 'no'",
        ns = ns,
        Dropdown.shinyInput(
          ns("experiment_type"),
          label = "Type of experiment",
          placeholder = "Choose an experiment...",
          options = as_options(experiment_types)
        )
      ),
      shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
      PrimaryButton.shinyInput(ns("start"), text = "Start", iconProps = list(iconName = "Forward"))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    submission <- shiny$eventReactive(input$start, {
      shiny$validate(
        shiny$need(shiny$isTruthy(input$name), "Please choose your name."),
        shiny$need(
          shiny$isTruthy(input$first_run),
          "Please say if this is the first run of the day."
        ),
        shiny$need(
          !identical(input$first_run, "no") || shiny$isTruthy(input$experiment_type),
          "Please choose your experiment type."
        )
      )
      list(
        name = input$name,
        experiment_date = as_local_date(input$experiment_date),
        first_run = input$first_run == "yes",
        experiment_type = input$experiment_type
      )
    })

    # Shows the validate() messages under the form.
    output$message <- shiny$renderText({
      submission()
      ""
    })

    submission
  })
}
