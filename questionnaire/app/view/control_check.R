# Shiny module: one control of the daily check (its lot, and whether it was valid).
# Used twice by daily_check.R: once for the positive and once for the negative control.
box::use(
  shiny,
)

yes_no <- c("Yes" = "yes", "No" = "no")

#' `title` is shown above the form, `next_label` on the main button.
#' @export
ui <- function(id, title, next_label) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$h4(id = ns("title"), class = "step-title", title),
    shiny$selectInput(ns("lot"), "Lot", choices = NULL, selectize = FALSE, width = "100%"),
    shiny$radioButtons(ns("valid"), "Was the control valid?",
      choices = yes_no, selected = character(0), inline = TRUE
    ),
    # Only asked when the control was not valid.
    shiny$conditionalPanel(
      condition = "input.valid == 'no'",
      ns = ns,
      shiny$radioButtons(ns("rerun_valid"), "Was the rerun valid?",
        choices = yes_no, selected = character(0), inline = TRUE
      )
    ),
    shiny$div(class = "form-message", shiny$textOutput(ns("message"))),
    shiny$div(
      class = "step-buttons",
      shiny$actionButton(ns("back"), "Back", icon = shiny$icon("arrow-left")),
      shiny$actionButton(ns("done"), next_label,
        class = "btn-primary", icon = shiny$icon("arrow-right")
      )
    )
  )
}

#' `lots` are the lots to choose from; when `reset()` changes, the answers are cleared.
#'
#' Returns a list with two reactives:
#' - `result`: the answers (lot, valid, rerun_valid), after the main button is clicked
#' - `back`: changes when the Back button is clicked
#' @export
server <- function(id, lots, reset) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "lot", choices = c("Choose a lot..." = "", lots))

    # The error messages are shown after a click on the main button, and hidden
    # again as soon as the user changes an answer.
    show_message <- shiny$reactiveVal(FALSE)
    shiny$observeEvent(input$done, show_message(TRUE))
    shiny$observeEvent(list(input$lot, input$valid, input$rerun_valid), show_message(FALSE),
      ignoreInit = TRUE
    )

    shiny$observeEvent(reset(),
      {
        show_message(FALSE)
        shiny$updateSelectInput(session, "lot", selected = "")
        shiny$updateRadioButtons(session, "valid", selected = character(0))
        shiny$updateRadioButtons(session, "rerun_valid", selected = character(0))
      },
      ignoreInit = TRUE
    )

    result <- shiny$eventReactive(input$done, {
      shiny$validate(
        shiny$need(input$lot != "", "Please choose the lot."),
        shiny$need(shiny$isTruthy(input$valid), "Please say if the control was valid."),
        shiny$need(
          !identical(input$valid, "no") || shiny$isTruthy(input$rerun_valid),
          "Please say if the rerun was valid."
        )
      )
      list(
        lot = input$lot,
        valid = input$valid,
        rerun_valid = if (input$valid == "yes") "not needed" else input$rerun_valid
      )
    })

    # Shows the validate() messages under the form.
    output$message <- shiny$renderText({
      if (show_message()) result()
      ""
    })

    list(result = result, back = shiny$reactive(input$back))
  })
}
