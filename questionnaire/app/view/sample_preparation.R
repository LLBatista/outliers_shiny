# Shiny module: how the samples were prepared (part of the experiment form).
box::use(
  shiny,
)

box::use(
  app/view/field_errors,
)

yes_no <- c("Yes" = "yes", "No" = "no")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("vials"), "Sample preparation"),
    shiny$radioButtons(ns("vortexed"), "Were the samples vortexed?",
      choices = yes_no, selected = character(0), inline = TRUE
    ),
    field_errors$message_ui(ns("vortexed")),
    shiny$radioButtons(ns("thawed"), "Were the samples thawed?",
      choices = yes_no, selected = character(0), inline = TRUE
    ),
    field_errors$message_ui(ns("thawed")),
    # Only asked when the samples were thawed.
    shiny$conditionalPanel(
      condition = "input.thawed == 'yes'",
      ns = ns,
      shiny$numericInput(ns("thaw_minutes"), "How long did they thaw? (minutes)",
        value = NA, min = 0, step = 1, width = "100%"
      ),
      field_errors$message_ui(ns("thaw_minutes"))
    )
  )
}

#' Returns a list:
#' - `collect(focus)`: list(samples_vortexed, samples_thawed, thaw_minutes), or NULL
#'   after showing what is missing (`focus`: move focus to the first problem)
#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    field_errors$clear_on_change(input, session, c("vortexed", "thawed", "thaw_minutes"))

    collect <- function(focus = TRUE) {
      thawed <- identical(input$thawed, "yes")
      minutes <- input$thaw_minutes
      ok <- field_errors$show(session, focus = focus, list(
        vortexed = if (!shiny$isTruthy(input$vortexed)) "Please say if the samples were vortexed.",
        thawed = if (!shiny$isTruthy(input$thawed)) "Please say if the samples were thawed.",
        thaw_minutes = if (thawed && (!is.numeric(minutes) || is.na(minutes) || minutes < 0)) {
          "Please enter how many minutes the samples thawed."
        }
      ))
      if (!ok) {
        return(NULL)
      }
      list(
        samples_vortexed = input$vortexed,
        samples_thawed = input$thawed,
        thaw_minutes = if (thawed) minutes else ""
      )
    }

    list(collect = collect)
  })
}
