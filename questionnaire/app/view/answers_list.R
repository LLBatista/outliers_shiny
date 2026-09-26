# Shiny module: the answers saved so far, one short entry per answer (easier to read
# on a tablet than a wide table):
#   ORG200, lot A1 - Analyzer 01
#   Linearity - Regular - saved at 14:05
#   Fluid SF2601 - buffer SB2601 - vortexed, thawed 30 min
box::use(
  shiny,
)

box::use(
  app/logic/i18n[tr],
  app/view/landing[run_type_label],
)

#' @export
ui <- function(id, title = tr("answers.title")) {
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
      tr("answers.main", product = answer$product, lot = answer$lot, instrument = answer$instrument)
    ),
    shiny$div(
      class = "answer-meta",
      paste(c(answer$experiment, run_type_label(answer$run_type), if (answer$saved_at != "") {
        tr("answers.saved_at", time = saved_time(answer$saved_at))
      }), collapse = " \u00b7 ")
    ),
    if (any(details != "")) {
      shiny$div(class = "answer-details", paste(details[details != ""], collapse = " \u00b7 "))
    },
    if (answer$notes != "") {
      shiny$div(class = "answer-details", tr("common.note_text", note = answer$notes))
    }
  )
}

#' `answers()`: a data frame from responses.R `answers_for_table()`, newest last;
#' `empty_text` is shown when it has no rows.
#' @export
server <- function(id, answers, empty_text = tr("answers.empty")) {
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
