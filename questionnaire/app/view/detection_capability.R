# Shiny module: the page for the "Detection Capability" experiment.
box::use(
  shiny,
)

box::use(
  app/view/experiment_form,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  # For now only the common form; the fields for this experiment go here later.
  experiment_form$ui(ns("form"))
}

#' Passes everything on to the common experiment form (see experiment_form.R).
#' @export
server <- function(id, ...) {
  args <- list(...)
  shiny$moduleServer(id, function(input, output, session) {
    do.call(experiment_form$server, c(list("form"), args))
  })
}
