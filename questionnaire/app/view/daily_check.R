box::use(
  shiny,
)

box::use(
  app/logic/daily_checks[lots_for_control],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("clipboard-check"), "First run of the day"),
    shiny$selectInput(ns("instrument"), "Instrument", choices = NULL, width = "100%"),
    shiny$selectInput(ns("positive_lot"), "Positive control lot", choices = NULL, width = "100%"),
    shiny$selectInput(ns("negative_lot"), "Negative control lot", choices = NULL, width = "100%"),
    shiny$radioButtons(ns("controls_valid"), "Were the controls valid?",
                       choices = c("Yes" = "yes", "No" = "no"), selected = character(0), inline = TRUE
    ),
    shiny$conditionalPanel(                     # 👈 this was missing too
      condition = "input.controls_valid == 'no'",
      ns = ns,
      shiny$radioButtons(ns("rerun_valid"), "Was the rerun valid?",
                         choices = c("Yes" = "yes", "No" = "no"), selected = character(0), inline = TRUE
      )
    ),
    shiny$actionButton(ns("save"), "Save", class = "btn-primary", icon = shiny$icon("check"))
  )   # 👈 the div closes at the very end
}

#' @export
server <- function(id, instruments, controls) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "instrument",
                            choices = c("Choose an instrument..." = "", instruments)
    )
    shiny$updateSelectInput(session, "positive_lot",
                            choices = c("Choose a lot..." = "", lots_for_control(controls, "Positive Control"))
    )
    shiny$updateSelectInput(session, "negative_lot",
                            choices = c("Choose a lot..." = "", lots_for_control(controls, "Negative Control"))
    )
    
    shiny$eventReactive(input$save, {
      shiny$validate(
        shiny$need(input$instrument != "", "Please choose an instrument."),
        shiny$need(length(input$controls) > 0, "Please choose at least one control."),
        shiny$need(shiny$isTruthy(input$controls_valid), "Please say if the controls were valid."),
        shiny$need(
          input$controls_valid == "yes" || shiny$isTruthy(input$rerun_valid),
          "Please say if the rerun was valid."),
        shiny$need(input$positive_lot != "", "Please choose the positive control lot."),
        shiny$need(input$negative_lot != "", "Please choose the negative control lot."),
      )
      list(
        instrument = input$instrument,
        positive_lot = input$positive_lot,
        negative_lot = input$negative_lot,
        controls_valid = input$controls_valid,
        rerun_valid = if (input$controls_valid == "yes") "not needed" else input$rerun_valid
      )
    })
  })
}