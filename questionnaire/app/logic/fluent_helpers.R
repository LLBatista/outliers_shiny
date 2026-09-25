# Small helpers for working with shiny.fluent inputs.

#' Fluent dropdowns want their options as a list of list(key = ..., text = ...).
#' @export
as_options <- function(values) {
  lapply(values, function(value) list(key = value, text = value))
}

#' Turn a Fluent DatePicker value into the Date the user picked.
#'
#' The DatePicker sends the chosen day as the UTC time of local midnight, e.g.
#' picking 20 September in Berlin sends "2026-09-19T22:00:00.000Z", and in New
#' York "2026-09-20T04:00:00.000Z". Adding 12 hours before taking the date gives
#' the 20th in both cases, whatever time zone the server runs in (this works for
#' every time zone from UTC-12 to UTC+12).
#' @export
as_local_date <- function(value) {
  if (grepl("T", value)) {
    utc_time <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    as.Date(utc_time + 12 * 60 * 60)
  } else {
    as.Date(value)
  }
}
