# Text fields with a maximum length, and the optional notes field.
# The browser stops typing at the limit (maxlength); the server checks it too
# (`too_long()`), in case a value arrives some other way.
box::use(
  htmltools[tagAppendAttributes],
  shiny,
  stats[setNames],
)

box::use(
  app/logic/i18n[tr],
)

#' Yes / No answers: the saved values are "yes" / "no", the labels are translated.
#' @export
yes_no <- function() {
  setNames(c("yes", "no"), c(tr("common.yes"), tr("common.no")))
}

#' Maximum lengths used in the app.
#' @export
max_length <- list(version = 5, lot = 12, notes = 500)

#' A text field that accepts at most `max` characters. `id` is the namespaced id.
#' @export
limited_text <- function(id, label, max, width = "100%") {
  tagAppendAttributes(
    shiny$textInput(id, label, width = width),
    maxlength = max,
    .cssSelector = "input"
  )
}

#' "Notes (optional)": a short free-text field, saved with the entry.
#' @export
notes <- function(id, label = tr("common.notes")) {
  tagAppendAttributes(
    shiny$textAreaInput(id, label, width = "100%", rows = 2, resize = "vertical"),
    maxlength = max_length$notes,
    .cssSelector = "textarea"
  )
}

#' The message when a value is longer than `max` characters (else NULL).
#' @export
too_long <- function(value, max) {
  if (nchar(paste0(value, "")) > max) tr("common.too_long", max = max)
}

#' A notes value as saved: trimmed, "" when empty, and never longer than allowed.
#' @export
clean_notes <- function(value) {
  substr(trimws(paste0(value, "")), 1, max_length$notes)
}
