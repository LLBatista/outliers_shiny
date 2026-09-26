# Shiny module: the answers saved so far, one short entry per answer (easier to read
# on a tablet than a wide table):
#   ORG200, lot A1 - Analyzer 01
#   Linearity - Regular - saved at 14:05
#   Fluid SF2601 - buffer SB2601 - vortexed, thawed 30 min
box::use(
  shiny,
)

#' @export
ui <- function(id, title = "Answers so far") {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("table-list"), title),
    shiny$uiOutput(ns("answers"))
  )
}

# The time part of "2026-09-26 14:05:12 CEST".
saved_time <- function(saved_at) substr(saved_at, 12, 16)

answer_entry <- function(answer) {
  details <- c(answer$fluids, answer$samples)
  shiny$tags$li(
    class = "answer",
    shiny$div(
      class = "answer-main",
      paste0(answer$product, ", lot ", answer$lot, " – ", answer$instrument)
    ),
    shiny$div(
      class = "answer-meta",
      paste(c(answer$experiment, answer$run_type, if (answer$saved_at != "") {
        paste("saved at", saved_time(answer$saved_at))
      }), collapse = " · ")
    ),
    if (any(details != "")) {
      shiny$div(class = "answer-details", paste(details[details != ""], collapse = " · "))
    }
  )
}

#' `answers()`: a data frame from responses.R `answers_for_table()`, newest last;
#' `empty_text` is shown when it has no rows.
#' @export
server <- function(id, answers,
                   empty_text = "No answers saved yet. They will appear here after you save one.") {
  shiny$moduleServer(id, function(input, output, session) {
    output$answers <- shiny$renderUI({
      data <- answers()
      if (nrow(data) == 0) {
        return(shiny$p(class = "empty-state", empty_text))
      }
      # Newest first: the answer just saved is at the top.
      rows <- rev(seq_len(nrow(data)))
      shiny$tags$ol(class = "answers", lapply(rows, function(i) answer_entry(data[i, ])))
    })
  })
}
