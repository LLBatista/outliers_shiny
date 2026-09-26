# Error messages shown right under the field they are about.
#
# In the UI, put `field_errors$message_ui(ns("lot"))` right after the "lot" input.
# In the server, `field_errors$show(session, list(lot = "Please choose the lot."))`
# shows the message, marks the field as invalid (red border, and for screen
# readers aria-invalid + aria-describedby) and moves focus to the first field with
# an error. An empty message ("") removes the error again.
# The browser side is the "field-errors" handler in main.R.
box::use(
  shiny,
  stats[setNames],
)

#' The place for one field's message. `input_id` is the full (namespaced) input id.
#' @export
message_ui <- function(input_id) {
  shiny$div(id = paste0(input_id, "-error"), class = "field-error")
}

#' Show (or with "" clear) the messages of some fields. `errors` is a named list:
#' input id (without namespace) -> message. Returns TRUE if there are no errors.
#' @export
show <- function(session, errors, focus = TRUE) {
  messages <- vapply(errors, function(x) if (is.null(x)) "" else x, character(1))
  session$sendCustomMessage("field-errors", list(
    ids = as.list(session$ns(names(errors))),
    messages = as.list(unname(messages)),
    focus = focus
  ))
  invisible(all(messages == ""))
}

#' Remove a field's message as soon as the user changes that field.
#' @export
clear_on_change <- function(input, session, ids) {
  lapply(ids, function(id) {
    shiny$observeEvent(input[[id]], show(session, setNames(list(""), id), focus = FALSE),
      ignoreInit = TRUE
    )
  })
  invisible(NULL)
}
