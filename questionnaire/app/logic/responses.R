# Plain R code (no Shiny): building and loading experiment answers.
box::use(
  app/logic/i18n[tr],
  app/logic/records[now_text, read_table],
)

# What the experiment form collects (see app/view/experiment_form.R).
answer_columns <- c(
  "instrument", "product", "lot",
  "system_buffer_lot", "system_buffer_expiry", "system_buffer_lot_2", "system_buffer_expiry_2",
  "system_fluid_lot", "system_fluid_expiry", "system_fluid_lot_2", "system_fluid_expiry_2",
  "samples_vortexed", "samples_thawed", "thaw_minutes", "notes"
)

# The columns of data/responses.csv, in order.
response_columns <- c("date", "user", "experiment", "run_type", answer_columns, "saved_at")

#' Build a one-row data frame with a single answer.
#' `run_type`: "Regular", "Retest" or "Pre-test".
#' `answer`: a list with the values of `answer_columns` (missing ones are saved empty).
#' @export
new_response <- function(experiment_date, user, experiment, run_type, answer) {
  row <- data.frame(
    date = format(as.Date(experiment_date), "%Y-%m-%d"),
    user = user,
    experiment = experiment,
    run_type = run_type,
    stringsAsFactors = FALSE
  )
  for (column in answer_columns) {
    value <- answer[[column]]
    row[[column]] <- if (length(value) == 0 || is.na(value[1])) "" else as.character(value[1])
  }
  row$saved_at <- now_text()
  row
}

#' Read all saved answers (an empty table if there are none yet).
#' @export
load_responses <- function(path) {
  read_table(path, response_columns)
}

#' Saved answers as shown in "Your answers for this date": the fluid lots and the
#' sample preparation are summed up in one column each.
#' @export
answers_for_table <- function(responses) {
  columns <- c(
    "experiment", "run_type", "instrument", "product", "lot", "fluids", "samples", "notes",
    "saved_at"
  )
  if (nrow(responses) == 0) {
    return(read_table(tempfile(), columns))
  }
  lots <- function(prefix) {
    paste0(responses[[paste0(prefix, "_lot")]], ifelse(
      responses[[paste0(prefix, "_lot_2")]] == "", "",
      paste0(" + ", responses[[paste0(prefix, "_lot_2")]])
    ))
  }
  fluids <- vapply(seq_len(nrow(responses)), function(i) {
    tr("answers.fluids", fluid = lots("system_fluid")[i], buffer = lots("system_buffer")[i])
  }, character(1))
  fluids[responses$system_fluid_lot == ""] <- ""
  samples <- describe_samples(
    responses$samples_vortexed, responses$samples_thawed, responses$thaw_minutes
  )
  data.frame(
    experiment = responses$experiment,
    run_type = responses$run_type,
    instrument = responses$instrument,
    product = responses$product,
    lot = responses$lot,
    fluids = fluids,
    samples = samples,
    notes = responses$notes,
    saved_at = responses$saved_at,
    stringsAsFactors = FALSE
  )
}

#' "vortexed, thawed 30 min" (translated), for each answer; "" when not answered.
#' @export
describe_samples <- function(vortexed, thawed, minutes) {
  text <- paste0(
    ifelse(vortexed == "yes", tr("samples.is_vortexed"), tr("samples.not_vortexed")), ", ",
    ifelse(thawed == "yes",
      vapply(minutes, function(m) tr("samples.is_thawed", minutes = m), character(1)),
      tr("samples.not_thawed")
    )
  )
  text[vortexed == ""] <- ""
  text
}
