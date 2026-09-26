# A date field that uses the device's own date picker (<input type="date">): on a
# tablet it opens the system's date wheel, which is easy to use at the bench.
# Its value is text "YYYY-MM-DD", or "" when empty. It needs no date-picker script;
# the browser side is the "native-date" input binding in main.R.
box::use(
  shiny,
)

#' `id` is the full (namespaced) input id.
#' @export
ui <- function(id, label) {
  shiny$div(
    class = "form-group shiny-input-container",
    style = "width: 100%;",
    shiny$tags$label(class = "control-label", `for` = id, label),
    shiny$tags$input(id = id, type = "date", class = "form-control native-date", value = "")
  )
}

#' Empty the field (`id` without namespace).
#' @export
clear <- function(session, id) {
  session$sendInputMessage(id, list(value = ""))
}
