box::use(
  shiny,
)


#' @export
ui <- function(id){
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("clipboard-check"), "First run of the day"),
    shiny$selectInput(ns("instrument"), 
                      "Instrument", 
                      choices = NULL, width = "100%"),
    shiny$selectInput(ns("controls"), 
                      "Controls used", 
                      choices = NULL, 
                      multiple = TRUE, width = "100%"),
    shiny$radioButtons(ns("controls_valid"), "Were the controls valid?",
                       choices = c("Yes" = "yes", "No" = "no"),
                       selected= character(0), inline = TRUE)
  )
  shiny$actionButton(ns("save"), "Save", 
                     class = "btn-primary", 
                     icon = shiny$icon("check"))
}

#' @export
server <- function(id, instruments, controls) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "instrument",
                            choices = c("Choose an instrument..." = "", instruments)
    )
    shiny$updateSelectInput(session, "controls", choices = controls)
    
    shiny$eventReactive(input$save, {
      shiny$validate(
        shiny$need(input$instrument != "", "Please choose an instrument."),
        shiny$need(length(input$controls) > 0, "Please choose at least one control."),
        shiny$need(shiny$isTruthy(input$controls_valid), "Please say if the controls were valid."),
        shiny$need(
          input$controls_valid == "yes" || shiny$isTruthy(input$rerun_valid),
          "Please say if the rerun was valid."
        )
      )
      list(
        instrument = input$instrument,
        controls = input$controls,
        controls_valid = input$controls_valid,
        rerun_valid = if (input$controls_valid == "yes") "not needed" else input$rerun_valid
      )
    })
  })
}