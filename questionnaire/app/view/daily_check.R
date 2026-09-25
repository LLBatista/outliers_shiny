# Shiny module: the first-run-of-the-day check (instrument and control lots).
box::use(
  shiny,
  shiny.fluent[
    ChoiceGroup.shinyInput, Dropdown.shinyInput, PrimaryButton.shinyInput, Stack, Text
  ],
)

box::use(
  app/logic/daily_checks[lots_for_control],
  app/logic/fluent_helpers[as_options],
)

# Options for the yes/no questions, shown side by side.
yes_no <- list(list(key = "yes", text = "Yes"), list(key = "no", text = "No"))
side_by_side <- list(flexContainer = list(display = "flex", gap = "24px"))

#' @export
ui <- function(id, instruments, controls) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    Stack(
      tokens = list(childrenGap = 16),
      Text(
        variant = "large", class = "card-title",
        shiny$icon("clipboard-check"), "First run of the day"
      ),
      Dropdown.shinyInput(
        ns("instrument"),
        label = "Instrument",
        placeholder = "Choose an instrument...",
        options = as_options(instruments)
      ),
      # The two lot dropdowns side by side
      Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 16),
        shiny$div(
          class = "half-width",
          Dropdown.shinyInput(
            ns("positive_lot"),
            label = "Positive control lot",
            placeholder = "Choose a lot...",
            options = as_options(lots_for_control(controls, "Positive Control"))
          )
        ),
        shiny$div(
          class = "half-width",
          Dropdown.shinyInput(
            ns("negative_lot"),
            label = "Negative control lot",
            placeholder = "Choose a lot...",
            options = as_options(lots_for_control(controls, "Negative Control"))
          )
        )
      ),
      ChoiceGroup.shinyInput(
        ns("controls_valid"),
        label = "Were the controls valid?",
        options = yes_no,
        styles = side_by_side
      ),
      # Only asked when the controls were not valid.
      shiny$conditionalPanel(
        condition = "input.controls_valid == 'no'",
        ns = ns,
        ChoiceGroup.shinyInput(
          ns("rerun_valid"),
          label = "Was the rerun valid?",
          options = yes_no,
          styles = side_by_side
        )
      ),
      shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
      shiny$div(
        PrimaryButton.shinyInput(
          ns("save"),
          text = "Save",
          iconProps = list(iconName = "CheckMark")
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    submission <- shiny$eventReactive(input$save, {
      shiny$validate(
        shiny$need(shiny$isTruthy(input$instrument), "Please choose an instrument."),
        shiny$need(shiny$isTruthy(input$positive_lot), "Please choose the positive control lot."),
        shiny$need(shiny$isTruthy(input$negative_lot), "Please choose the negative control lot."),
        shiny$need(shiny$isTruthy(input$controls_valid), "Please say if the controls were valid."),
        shiny$need(
          !identical(input$controls_valid, "no") || shiny$isTruthy(input$rerun_valid),
          "Please say if the rerun was valid."
        )
      )
      list(
        instrument = input$instrument,
        positive_lot = input$positive_lot,
        negative_lot = input$negative_lot,
        controls_valid = input$controls_valid,
        rerun_valid = if (input$controls_valid == "yes") "not needed" else input$rerun_valid
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
