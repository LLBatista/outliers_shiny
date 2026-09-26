# Plain R code (no Shiny): building and loading experiment answers.
box::use(
  app/logic/records[now_text, read_table],
)

# What the experiment form collects (see app/view/experiment_form.R).
answer_columns <- c(
  "instrument", "product", "lot",
  "system_buffer_lot", "system_buffer_expiry", "system_buffer_lot_2", "system_buffer_expiry_2",
  "system_fluid_lot", "system_fluid_expiry", "system_fluid_lot_2", "system_fluid_expiry_2",
  "samples_vortexed", "samples_thawed", "thaw_minutes"
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
