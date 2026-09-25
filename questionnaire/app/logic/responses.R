# Plain R code (no Shiny): building and loading experiment answers.
box::use(
  app/logic/records[now_text, read_table],
)

# The columns of data/responses.csv, in order.
response_columns <- c("date", "user", "experiment", "instrument", "product", "lot", "saved_at")

#' Build a one-row data frame with a single answer.
#' @export
new_response <- function(experiment_date, user, experiment, instrument, product, lot) {
  data.frame(
    date = format(as.Date(experiment_date), "%Y-%m-%d"),
    user = user,
    experiment = experiment,
    instrument = instrument,
    product = product,
    lot = lot,
    saved_at = now_text(),
    stringsAsFactors = FALSE
  )
}

#' Read all saved answers (an empty table if there are none yet).
#' @export
load_responses <- function(path) {
  read_table(path, response_columns)
}
