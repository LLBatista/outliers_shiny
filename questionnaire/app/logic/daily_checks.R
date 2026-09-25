# Plain R code (no Shiny): instruments, controls and the daily first-run checks.
box::use(
  utils[read.csv],
)

box::use(
  app/logic/records[now_text, read_table],
)

#' Read the instruments and their software / firmware versions from a CSV file.
#' @export
read_instruments <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
}

#' The software and firmware version of one instrument.
#' @export
instrument_versions <- function(instruments, instrument) {
  row <- instruments[instruments$instrument == instrument, ]
  list(
    software_version = row$software_version[1],
    firmware_version = row$firmware_version[1]
  )
}

#' Read the controls and their lots from a CSV file.
#' @export
read_controls <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
}

#' Lots available for one control.
#' @export
lots_for_control <- function(controls, control) {
  sort(unique(controls$lot[controls$control == control]))
}

# The columns of data/daily_checks.csv, in order.
daily_check_columns <- c(
  "date", "user", "instrument", "software_version", "firmware_version",
  "positive_lot", "positive_valid", "positive_rerun_valid",
  "negative_lot", "negative_valid", "negative_rerun_valid", "saved_at"
)

#' Build a one-row data frame with one daily check.
#'
#' `positive` and `negative` are lists with `lot`, `valid` and `rerun_valid`.
#' @export
new_daily_checks <- function(date, user, instrument, software_version, firmware_version,
                             positive, negative) {
  data.frame(
    date = format(as.Date(date), "%Y-%m-%d"),
    user = user,
    instrument = instrument,
    software_version = software_version,
    firmware_version = firmware_version,
    positive_lot = positive$lot,
    positive_valid = positive$valid,
    positive_rerun_valid = positive$rerun_valid,
    negative_lot = negative$lot,
    negative_valid = negative$valid,
    negative_rerun_valid = negative$rerun_valid,
    saved_at = now_text(),
    stringsAsFactors = FALSE
  )
}

#' Read all saved daily checks (an empty table if there are none yet).
#' @export
load_daily_checks <- function(path) {
  read_table(path, daily_check_columns)
}

#' Has this instrument already had a daily check on this date?
#' @export
already_checked <- function(checks, date, instrument) {
  any(checks$date == format(as.Date(date), "%Y-%m-%d") & checks$instrument == instrument)
}
